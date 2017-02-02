/* ********************************************************************** */
/* KeyLang: A Scripting Language in the Key of C */
/* ********************************************************************** */
#include "key.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <stdarg.h>
#include <ctype.h>
#include <errno.h>

#define TRACK_USAGES   (1)          /* boolean; record usage statistics */
#define ERROR_BUFLEN   (128)        /* length of error message buffer */
#define TOKEN_BUFLEN   (1024)       /* max length of a token */
#define INTERN_BUFLEN  (16 * 1024)  /* interned string storage */
#define GLOBAL_MAX     (1024)       /* max number of global variables */
#define OPER_DEPTH     (128)        /* operand stack size */
#define SCOPE_DEPTH    (512)        /* local scope stack size */
#define EXEC_DEPTH     (380)        /* exec stack size (make keyenv ~16384 bytes) */

#define Q(EX) do{ keyret rv_ = (EX); if (rv_ != KEY_OK) return rv_; }while(0)

#define keyval_end()        ((keyval){ .type=(KEY_END)                 })
#define keyval_mark()       ((keyval){ .type=(KEY_MARK)                })
#define keyval_bcode(V)     ((keyval){ .type=(KEY_BCODE), .bval=(V)    })
#define keyval_native(V)    ((keyval){ .type=(KEY_NAT),   .eval=(V)    })
#define keyval_func(V)      ((keyval){ .type=(KEY_FUNC),  .fval=(V)    })

#define STREQ(A,B)  ( (A) == (B) || !strcmp((A),(B)) )

/* ********************************************************************** */
/* ********************************************************************** */

typedef enum token_e {
  TOK_EOF = 0,
  /* lexical only */
  TOK_ADD_A, TOK_AND_A, TOK_COAL_A, TOK_DEC, TOK_DIV_A, TOK_INC, TOK_LOGAND_A,
  TOK_LOGOR_A, TOK_LSH_A, TOK_MOD_A, TOK_MUL_A, TOK_OR_A, TOK_RSH_A, TOK_SUB_A,
  TOK_USH_A, TOK_XOR_A,
  /* lexical and ast */
  TOK_ADD, TOK_AND, TOK_ASSIGN, TOK_BREAK, TOK_COAL, TOK_CBRACE, TOK_CBRACK,
  TOK_COLON, TOK_COMMA, TOK_CONT, TOK_CPAREN, TOK_DIV, TOK_DO, TOK_DOT,
  TOK_ELSE, TOK_EQ, TOK_FOR, TOK_FUNC, TOK_GE, TOK_GT, TOK_IDENT, TOK_IF,
  TOK_IS, TOK_ISNOT, TOK_INT, TOK_LOGAND, TOK_LOGNOT, TOK_LOGOR, TOK_LE,
  TOK_LSH, TOK_LT, TOK_MOD, TOK_MUL, TOK_OBRACE, TOK_OBRACK, TOK_OPAREN,
  TOK_OR, TOK_NE, TOK_NOT, TOK_QUEST, TOK_RETURN, TOK_RSH, TOK_SEMI, TOK_SUB,
  TOK_STRING, TOK_TYPEOF, TOK_UNDEF, TOK_USH, TOK_VAR, TOK_VARARGS, TOK_WHILE,
  TOK_XOR,
  /* ast only */
  TOK_LIST, TOK_CALL, TOK_NEG, TOK_ISDEF, TOK_FUNCX, TOK_M_ASSIGN,
  TOK_S_ASSIGN, TOK_M_CALL, TOK_S_CALL, TOK_POSTCOMMA, TOK_OBJECT, TOK_ARRAY
} token_t;

static char *
tok_name(token_t tk)
{
  static char * names[] = {
    "eof",
    "+=", "&=", "?\?=", "--", "/=", "++", "&&=",
    "||=", "<<=", "%=", "*=", "|=", ">>=", "-=",
    ">>>=", "^=",
    "+", "&", "=", "break", "?\?", "}", "]",
    ":", ",", "continue", ")", "/", "do", ".",
    "else", "==", "for", "fn", ">=", ">", "identifier", "if",
    "is", "isnot", "integer", "&&", "!", "||", "<=",
    "<<", "<", "%", "*", "{", "[", "(",
    "|", "!=", "~", "?", "return", ">>", ";", "-",
    "string", "typeof", "undef", ">>>", "var", "...", "while",
    "^",
    "list", "call", "-", "?", "fn", ".=",
    "[]=", ".()", "[]()", ",,", "object", "array"
  };
  return names[tk];
}

typedef struct node node;
struct node {
  token_t type;
  union {
    int ival;
    const char * sval;
  };
  node * first;
  node * second;
  node * third;
  node * fourth;
  node * chain;
};

typedef struct stack stack;
struct stack {
  int cap;
  int len;
#if TRACK_USAGES
  int max;
#endif
  keyval * buf;
};
struct keyenv {
  stack oper;
  stack scope;
  stack exec;
  keyval * root; /* bytecode allocation pointer */
  keyval buf[]; /* all the stack space */
};

static keyret bcode_load(keyenv * e);
static keyret bcode_assign(keyenv * e);
static keyret bcode_dup(keyenv * e);
static keyret bcode_index(keyenv * e);
static keyret bcode_pop(keyenv * e);
static keyret bcode_if(keyenv * e);
static keyret bcode_ifelse(keyenv * e);
static keyret bcode_doloop(keyenv * e);
static keyret bcode_loop(keyenv * e);
static keyret bcode_break(keyenv * e);
static keyret bcode_continue(keyenv * e);
static keyret bcode_call(keyenv * e);
static keyret bcode_return(keyenv * e);
static keyret bcode_var(keyenv * e);
static keyret bcode_defargs(keyenv * e);
static keyret bcode_varargs(keyenv * e);
static keyret bcode_add(keyenv * e);
static keyret bcode_sub(keyenv * e);
static keyret bcode_mul(keyenv * e);
static keyret bcode_div(keyenv * e);
static keyret bcode_mod(keyenv * e);
static keyret bcode_and(keyenv * e);
static keyret bcode_or(keyenv * e);
static keyret bcode_xor(keyenv * e);
static keyret bcode_lsh(keyenv * e);
static keyret bcode_rsh(keyenv * e);
static keyret bcode_ush(keyenv * e);
static keyret bcode_eq(keyenv * e);
static keyret bcode_ne(keyenv * e);
static keyret bcode_gt(keyenv * e);
static keyret bcode_lt(keyenv * e);
static keyret bcode_ge(keyenv * e);
static keyret bcode_le(keyenv * e);
static keyret bcode_is(keyenv * e);
static keyret bcode_isnot(keyenv * e);
static keyret bcode_not(keyenv * e);
static keyret bcode_lognot(keyenv * e);
static keyret bcode_neg(keyenv * e);
static keyret bcode_isdef(keyenv * e);
static keyret bcode_typeof(keyenv * e);
static keyret bcode_coal(keyenv * e);
static keyret bcode_logand(keyenv * e);
static keyret bcode_logor(keyenv * e);

/* ********************************************************************** */
/* Errors */
/* ********************************************************************** */

char key_errmsg[ERROR_BUFLEN] = "";
#define ERRMSG_LIMIT  (key_errmsg + ERROR_BUFLEN)

keyret
key_error(const char * fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(key_errmsg, ERROR_BUFLEN, fmt, ap);
  va_end(ap);
  return KEY_ERROR;
}

keyret
key_type_error(keyval a)
{
  if (a.type == KEY_STR) return key_error("unexpected string: '%s'", a.sval);
  if (a.type == KEY_INT) return key_error("unexpected integer: '%d'", a.ival);
  return key_error("unexpected type: %s", keyval_typename(a.type));
}

keyret
key_type_require(keyval a, keytype type)
{
  if (a.type == type) return KEY_OK;
  return key_error("unexpected type: have %s, expected %s",
                   keyval_typename(a.type), keyval_typename(type));
}

keyret
key_type_require_fn(keyval a)
{
  if (a.type == KEY_FUNC || a.type == KEY_NAT) return KEY_OK;
  return key_type_require(a, KEY_FUNC);
}

/* ********************************************************************** */
/* String Internment */
/* ********************************************************************** */

#if TRACK_USAGES
static size_t intern_max;
#endif

static char internbuf[INTERN_BUFLEN];
#define INTERN_LIMIT  (internbuf + INTERN_BUFLEN)

keyret
key_intern(const char ** dst, const char * src)
{
  char * p = internbuf;
  size_t n;
  if (!src || !*src) { *dst = ""; return KEY_OK; }
  if (src >= internbuf && src < INTERN_LIMIT) { *dst = src; return KEY_OK; }
  while (*p) {
    if (!strcmp(p, src)) { *dst = p; return KEY_OK; }
    p += strlen(p) + 1;
  }
  n = strlen(src) + 1;
  if ((size_t)(INTERN_LIMIT - p) <= n)
    return key_error("out of internment room for '%s'", src);
  strcpy(p, src);
  *(p + n) = '\0'; /* double zero at end makes it all work */
  *dst = p;
#if TRACK_USAGES
  n = (size_t)((p + n) - internbuf);
  if (intern_max < n) intern_max = n;
#endif
  return KEY_OK;
}

/* ********************************************************************** */
/* Tokenizer */
/* ********************************************************************** */

size_t
key_escape(char * dst, size_t dstlen, const char * src)
{
  size_t i = 0;
  const char * p;
#define OUT(C)  do{ if (i < dstlen) dst[i] = (C); ++i; }while(0)
  for (p = src; *p; ++p) {
    switch (*p) {
      case '\\': OUT('\\'); OUT('\\'); break;
      case '\n': OUT('\\'); OUT('n'); break;
      case '\r': OUT('\\'); OUT('r'); break;
      case '\t': OUT('\\'); OUT('t'); break;
      case '\"': OUT('\\'); OUT('\"'); break;
      case '\'': OUT('\\'); OUT('\''); break;
      default: OUT(*p); break;
    }
  }
#undef OUT
  if (i < dstlen) dst[i] = '\0'; else dst[dstlen - 1] = '\0';
  return i;
}

static int isword(int c) {
  return isalnum(c) || c == '_' || c == '$' || c == '@' || c >= 0x80;
}

struct lexer {
  const char * input; /* source text */
  const char * inp;   /* read pointer into input */
  const char * fname; /* name of input 'file' */
  int line;           /* line currently being tokenized */
  char * tokp;        /* write pointer into token buffer */
  char * buf;         /* caller-supplied token buffer */
  char * limit;       /* one-past-end of token buffer */
};
static struct lexer lexer;
#define APPEND(c)   (*(lexer.tokp)++ = (char)(c))
#define ZERO()      (*(lexer.tokp) = '\0')
#define OUTOFROOM() (lexer.tokp >= lexer.limit)

static keyret tokerror(const char * fmt, ...) KEY_FORMAT(1, 2);
static keyret
tokerror(const char * fmt, ...)
{
  va_list ap;
  char * p = key_errmsg;
  if (lexer.fname)
    p += snprintf(p, ERROR_BUFLEN, "%s:%d: ", lexer.fname, lexer.line);
  va_start(ap, fmt);
  vsnprintf(p, (size_t)(ERRMSG_LIMIT - p), fmt, ap);
  va_end(ap);
  return KEY_ERROR;
}

static void
lexer_init(const char * src, const char * fname, int line)
{
  lexer.input = lexer.inp = src;
  lexer.fname = fname;
  lexer.line = line;
  lexer.buf = lexer.limit = lexer.tokp = NULL;
}

static int nextChar(void) {
  int c = *lexer.inp;
  if (!c) return EOF;
  if (c == '\n') lexer.line++;
  lexer.inp++;
  return c;
}
static void ungetChar(void) {
  if (*--lexer.inp == '\n') lexer.line--;
}

static keyret
gatherString(int quote)
{
  int sawslashcr = 0;
  int c;
  while ((c = nextChar()) != quote) {
    if (c == '\\') {
      switch (c = nextChar()) {
        case EOF: return tokerror("unexpected end of file");
        case 'n': APPEND('\n'); break;
        case 'r': APPEND('\r'); break;
        case 't': APPEND('\t'); break;
        case '\r': sawslashcr = 1; break;
        case '\n': break;
        default: APPEND(c); break;
      }
    } else if (sawslashcr) {
      if (c != '\n') APPEND(c);
      sawslashcr = 0;
    } else if (c == EOF) {
      return tokerror("unexpected end of file");
    } else {
      APPEND(c);
    }
    if (OUTOFROOM()) return tokerror("string too long");
  }
  ZERO();
  return KEY_OK;
}

static keyret
gatherWord(int c)
{
  APPEND(c);
  while ((c = nextChar()) != EOF) {
    if (!isword(c)) { ungetChar(); break; }
    APPEND(c);
    if (OUTOFROOM()) return tokerror("token too long");
  }
  ZERO();
  return KEY_OK;
}

static void
skipComment(int type)
{
  int c;
  if (type == '*') {
    int sawstar = 0;
    while ((c = nextChar()) != EOF) {
      if (sawstar && c == '/') break;
      sawstar = (c == '*');
    }
  } else { /* / */
    while ((c = nextChar()) != EOF && c != '\n') /**/;
  }
}

static keyret
lexer_next(token_t * type, char * buf, size_t buflen)
{
  int c;
  lexer.buf = lexer.tokp = buf;
  lexer.limit = buf + buflen;
#define EQ(S)  ( !strcmp(lexer.buf, (S)) )
  while ((c = nextChar()) != EOF) {
    int d;
    if (c <= ' ' || isspace(c)) continue;
    if (c != '/') break;
    d = nextChar();
    if (d == '/' || d == '*') { skipComment(d); continue; }
    ungetChar();
    break;
  }
  if (c == EOF) { *type = TOK_EOF; return KEY_OK; }

  if (isdigit(c)) { Q(gatherWord(c)); *type = TOK_INT; return KEY_OK; }
  if (isword(c)) {
    Q(gatherWord(c));
    if      (EQ("break")) *type = TOK_BREAK;
    else if (EQ("continue")) *type = TOK_CONT;
    else if (EQ("do")) *type = TOK_DO;
    else if (EQ("else")) *type = TOK_ELSE;
    else if (EQ("fn")) *type = TOK_FUNC;
    else if (EQ("for")) *type = TOK_FOR;
    else if (EQ("if")) *type = TOK_IF;
    else if (EQ("is")) *type = TOK_IS;
    else if (EQ("isnot")) *type = TOK_ISNOT;
    else if (EQ("return")) *type = TOK_RETURN;
    else if (EQ("typeof")) *type = TOK_TYPEOF;
    else if (EQ("undef")) *type = TOK_UNDEF;
    else if (EQ("var")) *type = TOK_VAR;
    else if (EQ("while")) *type = TOK_WHILE;
    else *type = TOK_IDENT;
    return KEY_OK;
  }
  if (c == '\'' || c == '\"') {
    Q(gatherString(c));
    *type = TOK_STRING;
    return KEY_OK;
  }
  switch (c) {
    case ',': *type = TOK_COMMA; break;
    case '(': *type = TOK_OPAREN; break;
    case ')': *type = TOK_CPAREN; break;
    case '[': *type = TOK_OBRACK; break;
    case ']': *type = TOK_CBRACK; break;
    case '{': *type = TOK_OBRACE; break;
    case '}': *type = TOK_CBRACE; break;
    case ';': *type = TOK_SEMI; break;
    case ':': *type = TOK_COLON; break;
    case '~': *type = TOK_NOT; break;
    case '.': {
      c = nextChar();
      if (c == '.') {
        c = nextChar();
        if (c == '.') { *type = TOK_VARARGS; break; }
        ungetChar();
      }
      ungetChar();
      *type = TOK_DOT;
      break;
    }
#define MAYBE(C,T)  if (c == (C)) { *type = (T); break; }
#define ONEOREQ(EQ,NE) do{ \
  c = nextChar(); MAYBE('=', (EQ)); ungetChar(); *type = (NE); \
}while(0)
    case '+': {
      c = nextChar();
      MAYBE('+', TOK_INC);
      MAYBE('=', TOK_ADD_A);
      ungetChar(); *type = TOK_ADD;
      break;
    }
    case '-': {
      c = nextChar();
      MAYBE('-', TOK_DEC);
      MAYBE('=', TOK_SUB_A);
      ungetChar(); *type = TOK_SUB;
      break;
    }
    case '*': { ONEOREQ(TOK_MUL_A, TOK_MUL); break; }
    case '/': { ONEOREQ(TOK_DIV_A, TOK_DIV); break; }
    case '%': { ONEOREQ(TOK_MOD_A, TOK_MOD); break; }
    case '!': { ONEOREQ(TOK_NE, TOK_LOGNOT); break; }
    case '=': { ONEOREQ(TOK_EQ, TOK_ASSIGN); break; }
    case '^': { ONEOREQ(TOK_XOR_A, TOK_XOR); break; }
    case '&': {
      c = nextChar();
      if (c == '&') { ONEOREQ(TOK_LOGAND_A, TOK_LOGAND); break; }
      MAYBE('=', TOK_AND_A);
      ungetChar(); *type = TOK_AND;
      break;
    }
    case '|': {
      c = nextChar();
      if (c == '|') { ONEOREQ(TOK_LOGOR_A, TOK_LOGOR); break; }
      MAYBE('=', TOK_OR_A);
      ungetChar(); *type = TOK_OR;
      break;
    }
    case '<': {
      c = nextChar();
      if (c == '<') { ONEOREQ(TOK_LSH_A, TOK_LSH); break; }
      MAYBE('=', TOK_LE);
      ungetChar(); *type = TOK_LT;
      break;
    }
    case '>': {
      c = nextChar();
      if (c == '>') {
        c = nextChar();
        if (c == '>') { ONEOREQ(TOK_USH_A, TOK_USH); break; }
        MAYBE('=', TOK_RSH_A);
        ungetChar(); *type = TOK_RSH;
        break;
      }
      MAYBE('=', TOK_GE);
      ungetChar(); *type = TOK_GT;
      break;
    }
    case '?': {
      c = nextChar();
      if (c == '?') { ONEOREQ(TOK_COAL_A, TOK_COAL); break; }
      ungetChar(); *type = TOK_QUEST;
      break;
    }
    default: { return tokerror("unexpected character '%c'", c); }
  }
  strcpy(lexer.buf, tok_name(*type));
#undef EQ
#undef MAYBE
#undef ONEOREQ
  return KEY_OK;
}

/* ********************************************************************** */
/* AST Node */
/* ********************************************************************** */

struct parser {
  token_t type;            /* type of look-ahead token */
  char buf[TOKEN_BUFLEN];  /* text of look-ahead token */
  int loopDepth;           /* break/continue only allowed inside while */
  int funcDepth;           /* return only allowed inside functions */
  node * chain;            /* for cleanup on error */
};
static struct parser parser;

static void
node_delete_chain(node * n)
{
  if (n) {
    node * m = n->chain;
    free(n);
    node_delete_chain(m);
  }
}

static node *
node_new(token_t t, node * k0, node * k1, node * k2, node * k3)
{
  node * n = malloc(sizeof(node));
  if (!n) { perror("node_new"); exit(2); }
  n->type = t;
  n->sval = NULL;
  n->first = k0;
  n->second = k1;
  n->third = k2;
  n->fourth = k3;
  n->chain = parser.chain;
  parser.chain = n;
  return n;
}
#define node_new0(T)          node_new((T), NULL, NULL, NULL, NULL)
#define node_new1(T,A)        node_new((T), (A),  NULL, NULL, NULL)
#define node_new2(T,A,B)      node_new((T), (A),  (B),  NULL, NULL)
#define node_new3(T,A,B,C)    node_new((T), (A),  (B),  (C),  NULL)
#define node_new4(T,A,B,C,D)  node_new((T), (A),  (B),  (C),  (D) )

#define node_new_undef()      node_new0(TOK_UNDEF)
static node * node_new_int(int i) {
  node * n = node_new0(TOK_INT);
  n->ival = i;
  return n;
}
static node * node_new_string(token_t t, const char * s) {
  node * n = node_new0(t);
  n->sval = s;
  return n;
}

static keyret
convert_int(int * n, const char * txt)
{
  char * endp = NULL;
  long int i;
  errno = 0;
  i = strtol(txt, &endp, 0);
  if (errno || !endp || *endp) return tokerror("invalid integer");
#if LONG_MAX > INT_MAX
  if (i > INT_MAX || i < INT_MIN) return tokerror("invalid integer");
#endif
  *n = (int)i;
  return KEY_OK;
}

/* ********************************************************************** */
/* Parse into AST */
/* ********************************************************************** */

#define NEXT()  lexer_next(&parser.type, parser.buf, TOKEN_BUFLEN)
#define isType(T)  (parser.type == T)
#define MATCH(T)   int found_ = 0; Q(consume((T), &found_)); if (found_)

static keyret parseExpr(node ** n);
static keyret parseAssign(node ** n);
static keyret parseStatement(node ** n);
static keyret parseStatements(node ** n);
static keyret parseFunction(node ** n, int isexpr);

static keyret consume(token_t tk, int * found) {
  *found = (parser.type == tk);
  if (*found) return NEXT();
  return KEY_OK;
}
static keyret expect(token_t tk) {
  if (parser.type == tk) return NEXT();
  return tokerror("unexpected '%s', expected '%s'", parser.buf, tok_name(tk));
}
static keyret require(node * n, const char * name) {
  if (!n) return tokerror("missing %s", name);
  return KEY_OK;
}
static keyret requireIdent(node * n) {
  if (!n || n->type != TOK_IDENT) return tokerror("missing identifier");
  return KEY_OK;
}

static keyret checkAssign(node * n) {
  if (n->first->type == TOK_OBRACK) n->type = TOK_S_ASSIGN;
  else if (n->first->type == TOK_DOT) n->type = TOK_M_ASSIGN;
  else return requireIdent(n->first);
  return KEY_OK;
}

static keyret newIntern(node ** n, token_t t) {
  const char * p = NULL;
  Q(key_intern(&p, parser.buf));
  *n = node_new_string(t, p);
  return NEXT();
}
static keyret newInt(node ** n) {
  int v = 0;
  Q(convert_int(&v, parser.buf));
  *n = node_new_int(v);
  return NEXT();
}

static keyret parseIdent(node ** n) {
  if (isType(TOK_IDENT)) return newIntern(n, TOK_IDENT);
  return KEY_OK;
}
static keyret parseObjectLit(node ** n) {
  for (;;) { /* ident : expr ,} */
    node * m = NULL;
    Q(parseAssign(&m)); if (!m) break;
    if (m->type == TOK_IDENT) m->type = TOK_STRING;
    *n = node_new2(TOK_LIST, *n, m);
    Q(expect(TOK_COLON));
    m = NULL;
    Q(parseAssign(&m)); Q(require(m, "expression"));
    *n = node_new2(TOK_LIST, *n, m);
    if (isType(TOK_CBRACE)) break;
    Q(expect(TOK_COMMA));
  }
  *n = node_new1(TOK_OBJECT, *n);
  return expect(TOK_CBRACE);
}
static keyret parseArrayLit(node ** n) {
  for (;;) { /* expr ,] */
    node * m = NULL;
    Q(parseAssign(&m)); if (!m) break;
    *n = node_new2(TOK_LIST, *n, m);
    if (isType(TOK_CBRACK)) break;
    Q(expect(TOK_COMMA));
  }
  *n = node_new1(TOK_ARRAY, *n);
  return expect(TOK_CBRACK);
}
static keyret parsePrimary(node ** n) {
  if      (isType(TOK_IDENT))  { return newIntern(n, TOK_IDENT); }
  else if (isType(TOK_INT))    { return newInt(n); }
  else if (isType(TOK_STRING)) { return newIntern(n, TOK_STRING); }
  else if (isType(TOK_UNDEF))  { *n = node_new_undef(); return NEXT(); }
  else if (isType(TOK_FUNC))   { Q(NEXT()); return parseFunction(n, 1); }
  else if (isType(TOK_OBRACE)) { Q(NEXT()); return parseObjectLit(n); }
  else if (isType(TOK_OBRACK)) { Q(NEXT()); return parseArrayLit(n); }
  else if (isType(TOK_OPAREN)) {
    Q(NEXT());
    Q(parseExpr(n));
    return expect(TOK_CPAREN);
  }
  return KEY_OK;
}
static keyret parseCallArgs(node ** n) {
  Q(parseAssign(n));
  while (*n) {
    MATCH(TOK_COMMA) {
      node * m = NULL;
      Q(parseAssign(&m)); Q(require(m, "argument"));
      *n = node_new2(TOK_LIST, *n, m);
      continue;
    }
    break;
  }
  return KEY_OK;
}
static keyret parseMember(node ** n) {
  Q(parsePrimary(n));
  while (*n) {
    { MATCH(TOK_DOT) {
      node * m = NULL;
      Q(parseIdent(&m));
      *n = node_new2(TOK_DOT, *n, m);
      continue;
    } }
    { MATCH(TOK_OBRACK) {
      node * m = NULL;
      Q(parseExpr(&m)); Q(require(m, "expression"));
      Q(expect(TOK_CBRACK));
      *n = node_new2(TOK_OBRACK, *n, m);
      continue;
    } }
    break;
  }
  return KEY_OK;
}
static keyret parsePostfix(node ** n) {
  Q(parseMember(n));
  while (*n) {
    { MATCH(TOK_INC) { /* expr++ -> expr ,, expr += 1 */
      node * m = *n;
      Q(require(*n, "expression"));
      *n = node_new2(TOK_ASSIGN, *n, node_new2(TOK_ADD, *n, node_new_int(1)));
      Q(checkAssign(*n));
      *n = node_new2(TOK_POSTCOMMA, m, *n);
      continue;
    } }
    { MATCH(TOK_DEC) { /* expr-- -> expr ,, expr -= 1 */
      node * m = *n;
      Q(require(*n, "expression"));
      *n = node_new2(TOK_ASSIGN, *n, node_new2(TOK_SUB, *n, node_new_int(1)));
      Q(checkAssign(*n));
      *n = node_new2(TOK_POSTCOMMA, m, *n);
      continue;
    } }
    { MATCH(TOK_OPAREN) {
      node * m = NULL;
      Q(parseCallArgs(&m));
      Q(expect(TOK_CPAREN));
      if ((*n)->type == TOK_OBRACK || (*n)->type == TOK_DOT) {
        token_t type = (*n)->type == TOK_OBRACK ? TOK_S_CALL : TOK_M_CALL;
        *n = node_new2(type, *n, m);
      } else {
        *n = node_new2(TOK_CALL, *n, m);
      }
      continue;
    } }
    break;
  }
  return KEY_OK;
}
static keyret parsePrefix(node ** n) {
  if (isType(TOK_INC)) { /* ++expr -> expr += 1 */
    Q(NEXT());
    Q(parsePrefix(n)); Q(require(*n, "expression"));
    *n = node_new2(TOK_ASSIGN, *n, node_new2(TOK_ADD, *n, node_new_int(1)));
    Q(checkAssign(*n));

  } else if (isType(TOK_DEC)) { /* --expr -> expr -= 1 */
    Q(NEXT());
    Q(parsePrefix(n)); Q(require(*n, "expression"));
    *n = node_new2(TOK_ASSIGN, *n, node_new2(TOK_SUB, *n, node_new_int(1)));
    Q(checkAssign(*n));

  } else if (isType(TOK_TYPEOF) || isType(TOK_NOT) || isType(TOK_LOGNOT) ||
             isType(TOK_QUEST) || isType(TOK_SUB) || isType(TOK_ADD)) {
    token_t type = parser.type;
    if (type == TOK_SUB) type = TOK_NEG;
    if (type == TOK_QUEST) type = TOK_ISDEF;
    Q(NEXT());
    Q(parsePrefix(n)); Q(require(*n, "expression"));
    if (type != TOK_ADD) *n = node_new1(type, *n);

  } else {
    Q(parsePostfix(n));
  }
  return KEY_OK;
}
static keyret parseInfixMul(node ** n) {
  Q(parsePrefix(n));
  while (*n) {
    if (isType(TOK_MUL) || isType(TOK_DIV) || isType(TOK_MOD)) {
      token_t type = parser.type;
      node * m = NULL;
      Q(NEXT());
      Q(parsePrefix(&m)); Q(require(m, "expression"));
      *n = node_new2(type, *n, m);
      continue;
    }
    break;
  }
  return KEY_OK;
}
static keyret parseInfixAdd(node ** n) {
  Q(parseInfixMul(n));
  while (*n) {
    if (isType(TOK_ADD) || isType(TOK_SUB)) {
      token_t type = parser.type;
      node * m = NULL;
      Q(NEXT());
      Q(parseInfixMul(&m)); Q(require(m, "expression"));
      *n = node_new2(type, *n, m);
      continue;
    }
    break;
  }
  return KEY_OK;
}
static keyret parseInfixBit(node ** n) {
  Q(parseInfixAdd(n));
  while (*n) {
    if (isType(TOK_LSH) || isType(TOK_RSH) || isType(TOK_USH) ||
        isType(TOK_AND) || isType(TOK_OR) || isType(TOK_XOR)) {
      token_t type = parser.type;
      node * m = NULL;
      Q(NEXT());
      Q(parseInfixAdd(&m)); Q(require(m, "expression"));
      *n = node_new2(type, *n, m);
      continue;
    }
    break;
  }
  return KEY_OK;
}
static keyret parseInfixCmp(node ** n) {
  Q(parseInfixBit(n));
  while (*n) {
    if (isType(TOK_LT) || isType(TOK_LE) || isType(TOK_GT) ||
        isType(TOK_GE) || isType(TOK_EQ) || isType(TOK_NE) ||
        isType(TOK_IS) || isType(TOK_ISNOT)) {
      token_t type = parser.type;
      node * m = NULL;
      Q(NEXT());
      Q(parseInfixBit(&m)); Q(require(m, "expression"));
      *n = node_new2(type, *n, m);
      continue;
    }
    break;
  }
  return KEY_OK;
}
static keyret parseShortfix(node ** n) {
  Q(parseInfixCmp(n));
  while (*n) {
    if (isType(TOK_LOGAND) || isType(TOK_LOGOR) || isType(TOK_COAL)) {
      token_t type = parser.type;
      node * m = NULL;
      Q(NEXT());
      Q(parseInfixCmp(&m)); Q(require(m, "expression"));
      *n = node_new2(type, *n, m);
      continue;
    }
    break;
  }
  return KEY_OK;
}
static keyret parseCond(node ** n) {
  Q(parseShortfix(n));
  if (*n && isType(TOK_QUEST)) {
    node * t = NULL;
    node * f = NULL;
    Q(NEXT());
    Q(parseAssign(&t)); Q(require(t, "expression"));
    Q(expect(TOK_COLON));
    Q(parseAssign(&f)); Q(require(f, "expression"));
    *n = node_new3(TOK_IF, *n, t, f);
  }
  return KEY_OK;
}
static keyret parseAssign(node ** n) {
  Q(parseCond(n));
  if (*n) {
    if (isType(TOK_ASSIGN)) {
      node * m = NULL;
      Q(NEXT());
      Q(parseAssign(&m)); Q(require(m, "expression"));
      *n = node_new2(TOK_ASSIGN, *n, m);
    } else if (isType(TOK_ADD_A) || isType(TOK_SUB_A) ||
      isType(TOK_MUL_A) || isType(TOK_DIV_A) || isType(TOK_MOD_A) ||
      isType(TOK_AND_A) || isType(TOK_OR_A) || isType(TOK_XOR_A) ||
      isType(TOK_LSH_A) || isType(TOK_RSH_A) || isType(TOK_USH_A) ||
      isType(TOK_LOGAND_A) || isType(TOK_LOGOR_A) || isType(TOK_COAL_A)) {
      token_t type = parser.type;
      node * m = NULL;
      Q(NEXT());
      Q(parseAssign(&m)); Q(require(m, "expression"));
      switch (type) {
        case TOK_ADD_A: type = TOK_ADD; break;
        case TOK_SUB_A: type = TOK_SUB; break;
        case TOK_MUL_A: type = TOK_MUL; break;
        case TOK_DIV_A: type = TOK_DIV; break;
        case TOK_MOD_A: type = TOK_MOD; break;
        case TOK_AND_A: type = TOK_AND; break;
        case TOK_OR_A:  type = TOK_OR; break;
        case TOK_XOR_A: type = TOK_XOR; break;
        case TOK_LSH_A: type = TOK_LSH; break;
        case TOK_RSH_A: type = TOK_RSH; break;
        case TOK_USH_A: type = TOK_USH; break;
        case TOK_LOGAND_A: type = TOK_LOGAND; break;
        case TOK_LOGOR_A: type = TOK_LOGOR; break;
        case TOK_COAL_A: type = TOK_COAL; break;
        default: type = TOK_EOF; break;
      }
      /* n op= m -> n = n op m */
      *n = node_new2(TOK_ASSIGN, *n, node_new2(type, *n, m));
    }
    if ((*n)->type == TOK_ASSIGN) Q(checkAssign(*n));
  }
  return KEY_OK;
}
static keyret parseExpr(node ** n) {
  Q(parseAssign(n));
  while (*n) {
    { MATCH(TOK_COMMA) {
      node * m = NULL;
      Q(parseAssign(&m)); Q(require(m, "expression"));
      *n = node_new2(TOK_COMMA, *n, m);
      continue;
    } }
    break;
  }
  return KEY_OK;
}
static keyret parseVarOne(node ** n) {
  node * name = NULL;
  node * value = NULL;
  Q(parseIdent(&name)); Q(requireIdent(name));
  { MATCH(TOK_ASSIGN) {
    Q(parseAssign(&value));
    Q(require(value, "expression"));
  } }
  *n = node_new2(TOK_VAR, name, value);
  return KEY_OK;
}
static keyret parseVar(node ** n) {
  Q(parseVarOne(n));
  while (*n) {
    { MATCH(TOK_COMMA) {
      node * m = NULL;
      Q(parseVarOne(&m)); Q(require(m, "declaration"));
      *n = node_new2(TOK_LIST, *n, m);
      continue;
    } }
    break;
  }
  return expect(TOK_SEMI);
}
static keyret parseIf(node ** n) {
  node * cond = NULL;
  node * body = NULL;
  Q(expect(TOK_OPAREN));
  Q(parseExpr(&cond)); Q(require(cond, "condition"));
  Q(expect(TOK_CPAREN));
  Q(parseStatement(&body)); Q(require(body, "statement"));
  *n = node_new2(TOK_IF, cond, body);
  { MATCH(TOK_ELSE) {
    Q(parseStatement(&((*n)->third)));
    Q(require((*n)->third, "statement"));
  } }
  return KEY_OK;
}
static keyret parseFunctionArglistOne(node ** n, int * va) {
  node * m = NULL;
  Q(parseIdent(&m));
  if (!m && !*n) return KEY_OK; /* empty list */
  Q(requireIdent(m));
  m->type = TOK_STRING;
  *n = node_new2(TOK_LIST, *n, m);
  { MATCH(TOK_VARARGS) {
    *n = node_new1(TOK_VARARGS, *n);
    *va = 1; /* '...' must be last in list */
  } }
  return KEY_OK;
}
static keyret parseFunctionArglist(node ** n) {
  int va = 0;
  Q(parseFunctionArglistOne(n, &va));
  while (*n && !va) {
    { MATCH(TOK_COMMA) {
      Q(parseFunctionArglistOne(n, &va));
      continue;
    } }
    break;
  }
  return KEY_OK;
}
static keyret makeName(node ** n) {
  static unsigned int anon_id = 0;
  char buf[64];
  const char * p;
  snprintf(buf, sizeof(buf), "__anon_%u__", ++anon_id);
  Q(key_intern(&p, buf));
  *n = node_new_string(TOK_IDENT, p);
  return KEY_OK;
}
static keyret parseFunction(node ** n, int isexpr) {
  node * name = NULL;
  node * args = NULL;
  node * body = NULL;
  Q(parseIdent(&name));
  if (!isexpr) Q(requireIdent(name));
  else if (!name) Q(makeName(&name));
  Q(expect(TOK_OPAREN));
  Q(parseFunctionArglist(&args));
  Q(expect(TOK_CPAREN));
  parser.funcDepth++;
  Q(parseStatement(&body)); Q(require(body, "statement"));
  parser.funcDepth--;
  token_t type = isexpr ? TOK_FUNCX : TOK_FUNC;
  *n = node_new3(type, name, args, body);
  return KEY_OK;
}
static keyret parseDo(node ** n) {
  node * body = NULL;
  node * cond = NULL;
  parser.loopDepth++;
  Q(parseStatement(&body)); Q(require(body, "statement"));
  parser.loopDepth--;
  Q(expect(TOK_WHILE));
  Q(expect(TOK_OPAREN));
  Q(parseExpr(&cond)); Q(require(cond, "condition"));
  Q(expect(TOK_CPAREN));
  *n = node_new2(TOK_DO, cond, body);
  return KEY_OK;
}
static keyret parseWhile(node ** n) {
  node * cond = NULL;
  node * body = NULL;
  Q(expect(TOK_OPAREN));
  Q(parseExpr(&cond)); Q(require(cond, "condition"));
  Q(expect(TOK_CPAREN));
  parser.loopDepth++;
  Q(parseStatement(&body)); Q(require(body, "statement"));
  parser.loopDepth--;
  *n = node_new2(TOK_WHILE, cond, body);
  return KEY_OK;
}
static keyret parseFor(node ** n) {
  node * init = NULL;
  node * cond = NULL;
  node * inc = NULL;
  node * body = NULL;
  Q(expect(TOK_OPAREN));
  Q(parseExpr(&init));
  if (init) init = node_new1(TOK_COMMA, init); /* 'pop' the result */
  Q(expect(TOK_SEMI));
  Q(parseExpr(&cond));
  if (!cond) cond = node_new_int(1); /* empty cond is always true */
  Q(expect(TOK_SEMI));
  Q(parseExpr(&inc));
  if (inc) inc = node_new1(TOK_COMMA, inc); /* 'pop' the result */
  Q(expect(TOK_CPAREN));
  parser.loopDepth++;
  Q(parseStatement(&body)); Q(require(body, "statement"));
  parser.loopDepth--;
  *n = node_new4(TOK_FOR, init, cond, inc, body);
  return KEY_OK;
}
static keyret parseBlock(node ** n) {
  Q(parseStatements(n));
  if (!*n) *n = node_new0(TOK_LIST); /* nop; can't return NULL */
  return expect(TOK_CBRACE);
}
static keyret parseReturn(node ** n) {
  if (parser.funcDepth == 0) return tokerror("invalid return statement");
  Q(parseExpr(n));
  *n = node_new1(TOK_RETURN, *n);
  return expect(TOK_SEMI);
}
static keyret parseBranch(node ** n, token_t tk) {
  if (parser.loopDepth == 0) return tokerror("invalid branch statement");
  *n = node_new0(tk);
  return expect(TOK_SEMI);
}
static keyret parseStatement(node ** n) {
  { MATCH(TOK_BREAK)  { return parseBranch(n, TOK_BREAK); } }
  { MATCH(TOK_CONT)   { return parseBranch(n, TOK_CONT); } }
  { MATCH(TOK_DO)     { return parseDo(n); } }
  { MATCH(TOK_FOR)    { return parseFor(n); } }
  { MATCH(TOK_IF)     { return parseIf(n); } }
  { MATCH(TOK_OBRACE) { return parseBlock(n); } }
  { MATCH(TOK_RETURN) { return parseReturn(n); } }
  { MATCH(TOK_VAR)    { return parseVar(n); } }
  { MATCH(TOK_WHILE)  { return parseWhile(n); } }
  { MATCH(TOK_SEMI)   { *n = node_new0(TOK_LIST); return KEY_OK; } }
  { MATCH(TOK_FUNC)   { return parseFunction(n, 0); } }
  { MATCH(TOK_VAR)    { return parseVar(n); } }
  Q(parseExpr(n));
  if (*n) {
    *n = node_new1(TOK_COMMA, *n); /* 'pop' the result */
    return expect(TOK_SEMI);
  }
  return KEY_OK;
}
static keyret parseStatements(node ** n) {
  Q(parseStatement(n));
  while (*n) {
    node * m = NULL;
    Q(parseStatement(&m));
    if (!m) break;
    *n = node_new2(TOK_LIST, *n, m);
  }
  return KEY_OK;
}

static keyret
parser_parse(node ** n, const char * src, const char * fname, int line)
{
  keyret err;
  lexer_init(src, fname, line);
  parser.loopDepth = 0;
  parser.funcDepth = 0;
  *n = parser.chain = NULL;
  Q(NEXT()); /* initialize look-ahead */
  err = parseStatements(n);
  if (!err) err = expect(TOK_EOF);
  if (err) {
    node_delete_chain(parser.chain);
    *n = parser.chain = NULL;
  }
  return err;
}

/* ********************************************************************** */
/* Compile AST to Bytecode */
/* ********************************************************************** */

/* work buffer:  mark proc1... mark proc2...
 * final buffer: proc2... end proc1... end
 * proc:         &(proc1)
 */
struct compiler {
  keyval * work;
  keyval * final;
  keyval * proc;
  int wlen;
  int flen;
  int counting;
};
static struct compiler comp;

static void compPush(keyval v) {
  if (comp.counting) {
    comp.wlen++;
  } else {
    comp.work[comp.wlen++] = v;
  }
}

static int compCountList(const node * n, token_t tk) {
  /* n: null || expr || list(expr expr) || list(list(... expr) expr) */
  int cnt = 0;
  while (n) { ++cnt; if (n->type == tk) n = n->first; else break; }
  return cnt;
}

#define procBegin()   compPush(keyval_mark())
#define procEnd()     compProc()
#define compCode(C)   compPush(keyval_bcode(C))
#define compString(N) compPush(keyval_str((N)->sval))

static int findMark(void) {
  int i;
  for (i = comp.wlen; i--; ) {
    if (comp.work[i].type == KEY_MARK) return i;
  }
  return -1; /* should never happen */
}
static void compProc(void) { /* mark body... -> func */
  if (comp.counting) {
    comp.wlen++;
  } else { /* move from 'work' to 'final' */
    int i = findMark();
    int len = comp.wlen - i - 1;
    comp.proc = comp.final + comp.flen;
    memcpy(comp.proc, comp.work + i + 1, (size_t)len * sizeof(keyval));
    comp.proc[len] = keyval_end();
    comp.flen += len + 1;
    comp.work[i] = keyval_func(comp.proc);
    comp.wlen = i + 1;
  }
}

static void compOne(const node * n) {
  if (!n) return;
  switch (n->type) {
    case TOK_UNDEF:  { compPush(keyval_undef()); break; }
    case TOK_INT:    { compPush(keyval_int(n->ival)); break; }
    case TOK_STRING: { compPush(keyval_str(n->sval)); break; }
    case TOK_IDENT:  { compString(n); compCode(bcode_load); break; }

    case TOK_NOT:    { compOne(n->first); compCode(bcode_not); break; }
    case TOK_LOGNOT: { compOne(n->first); compCode(bcode_lognot); break; }
    case TOK_NEG:    { compOne(n->first); compCode(bcode_neg); break; }
    case TOK_ISDEF:  { compOne(n->first); compCode(bcode_isdef); break; }
    case TOK_TYPEOF: { compOne(n->first); compCode(bcode_typeof); break; }

    case TOK_ADD: { compOne(n->first); compOne(n->second); compCode(bcode_add); break; }
    case TOK_SUB: { compOne(n->first); compOne(n->second); compCode(bcode_sub); break; }
    case TOK_MUL: { compOne(n->first); compOne(n->second); compCode(bcode_mul); break; }
    case TOK_DIV: { compOne(n->first); compOne(n->second); compCode(bcode_div); break; }
    case TOK_MOD: { compOne(n->first); compOne(n->second); compCode(bcode_mod); break; }
    case TOK_AND: { compOne(n->first); compOne(n->second); compCode(bcode_and); break; }
    case TOK_OR:  { compOne(n->first); compOne(n->second); compCode(bcode_or); break; }
    case TOK_XOR: { compOne(n->first); compOne(n->second); compCode(bcode_xor); break; }
    case TOK_LSH: { compOne(n->first); compOne(n->second); compCode(bcode_lsh); break; }
    case TOK_RSH: { compOne(n->first); compOne(n->second); compCode(bcode_rsh); break; }
    case TOK_USH: { compOne(n->first); compOne(n->second); compCode(bcode_ush); break; }
    case TOK_EQ:  { compOne(n->first); compOne(n->second); compCode(bcode_eq); break; }
    case TOK_NE:  { compOne(n->first); compOne(n->second); compCode(bcode_ne); break; }
    case TOK_GT:  { compOne(n->first); compOne(n->second); compCode(bcode_gt); break; }
    case TOK_LT:  { compOne(n->first); compOne(n->second); compCode(bcode_lt); break; }
    case TOK_GE:  { compOne(n->first); compOne(n->second); compCode(bcode_ge); break; }
    case TOK_LE:  { compOne(n->first); compOne(n->second); compCode(bcode_le); break; }
    case TOK_IS:  { compOne(n->first); compOne(n->second); compCode(bcode_is); break; }
    case TOK_ISNOT: { compOne(n->first); compOne(n->second); compCode(bcode_isnot); break; }

    case TOK_COAL: { /* expr expr -> expr { expr } op */
      compOne(n->first);
      procBegin(); compOne(n->second); procEnd();
      compCode(bcode_coal);
      break;
    }
    case TOK_LOGAND: { /* expr expr -> expr { expr } op */
      compOne(n->first);
      procBegin(); compOne(n->second); procEnd();
      compCode(bcode_logand);
      break;
    }
    case TOK_LOGOR: { /* expr expr -> expr { expr } op */
      compOne(n->first);
      procBegin(); compOne(n->second); procEnd();
      compCode(bcode_logor);
      break;
    }

    case TOK_ASSIGN: { /* ident expr -> expr str assign */
      compOne(n->second);
      compString(n->first);
      compCode(bcode_assign);
      break;
    }
    case TOK_M_ASSIGN:
    case TOK_S_ASSIGN: { /* expr expr -> expr expr expr member */
      const node * m = n->first;
      compOne(m->first);
      if (n->type == TOK_M_ASSIGN) compString(m->second);
      else compOne(m->second);
      compOne(n->second);
      compPush(keyval_int(3));
      compPush(keyval_str("__member__"));
      compCode(bcode_load);
      compCode(bcode_call);
      break;
    }
    case TOK_DOT:
    case TOK_OBRACK: { /* expr expr -> expr expr member */
      compOne(n->first);
      if (n->type == TOK_DOT) compString(n->second);
      else compOne(n->second);
      compPush(keyval_int(2));
      compPush(keyval_str("__member__"));
      compCode(bcode_load);
      compCode(bcode_call);
      break;
    }
    case TOK_ARRAY:
    case TOK_OBJECT: { /* list -> list N literal */
      compOne(n->first);
      compPush(keyval_int(compCountList(n->first, TOK_LIST)));
      compPush(keyval_str(n->type == TOK_ARRAY ? "__array__" : "__object__"));
      compCode(bcode_load);
      compCode(bcode_call);
      break;
    }

    case TOK_LIST: { /* expr|stmt? expr|stmt? -> expr|stmt expr|stmt */
      compOne(n->first);
      compOne(n->second);
      break;
    }
    case TOK_COMMA: { /* expr expr? -> expr pop expr */
      compOne(n->first);
      compCode(bcode_pop);
      compOne(n->second);
      break;
    }
    case TOK_POSTCOMMA: { /* expr expr -> expr expr pop */
      compOne(n->first);
      compOne(n->second);
      compCode(bcode_pop);
      break;
    }

    case TOK_IF: { /* expr stmt?|expr stmt?|expr -> expr {} {} ifelse */
      compOne(n->first);
      procBegin(); compOne(n->second); procEnd();
      if (n->third) {
        procBegin(); compOne(n->third); procEnd();
        compCode(bcode_ifelse);
      } else {
        compCode(bcode_if);
      }
      break;
    }

    case TOK_DO: { /* expr stmt? -> {expr} {} {stmt} dowhile */
      procBegin(); compOne(n->first); procEnd();
      procBegin(); procEnd();
      procBegin(); compOne(n->second); procEnd();
      compCode(bcode_doloop);
      break;
    }
    case TOK_WHILE: { /* expr stmt? -> {expr} {} {stmt} while */
      procBegin(); compOne(n->first); procEnd();
      procBegin(); procEnd();
      procBegin(); compOne(n->second); procEnd();
      compCode(bcode_loop);
      break;
    }
    case TOK_FOR: { /* init cond inc stmt? -> init; {cond} {inc} {stmt} for */
      compOne(n->first);
      procBegin(); compOne(n->second); procEnd();
      procBegin(); compOne(n->third); procEnd();
      procBegin(); compOne(n->fourth); procEnd();
      compCode(bcode_loop);
      break;
    }
    case TOK_BREAK: { compCode(bcode_break); break; }
    case TOK_CONT:  { compCode(bcode_continue); break; }

    case TOK_CALL: { /* expr call-list -> call-list N expr call */
      compOne(n->second);
      compPush(keyval_int(compCountList(n->second, TOK_LIST)));
      compOne(n->first);
      compCode(bcode_call);
      break;
    }
    case TOK_M_CALL:
    case TOK_S_CALL: { /* expr call-list -> object call-list N+1 index expr member call */
      const node * m = n->first;
      compOne(m->first);
      compOne(n->second);
      compPush(keyval_int(compCountList(n->second, TOK_LIST) + 1));
      compCode(bcode_index); /* don't twice-compile/eval an expr */
      if (n->type == TOK_M_CALL) compString(m->second);
      else compOne(m->second);
      compPush(keyval_int(2));
      compPush(keyval_str("__member__"));
      compCode(bcode_load);  /* load __member__ to get the method-getter */
      compCode(bcode_call);  /* call the method-getter to get the method */
      compCode(bcode_call);  /* call the method */
      break;
    }
    case TOK_RETURN: { /* expr? -> expr|undef return */
      if (n->first) compOne(n->first); else compPush(keyval_undef());
      compCode(bcode_return);
      break;
    }

    case TOK_VAR: { /* ident expr? -> expr|undef str var */
      if (n->second) { compOne(n->second); } else { compPush(keyval_undef()); }
      compString(n->first);
      compCode(bcode_var);
      break;
    }
    case TOK_FUNCX:
    case TOK_FUNC: { /* name args body -> proc dup? name var */
      /* proc: name pop arg-names... arg-name-count defargs body */
      int va = n->second && n->second->type == TOK_VARARGS;
      node * args = va ? n->second->first : n->second;
      procBegin();
      compString(n->first);
      compCode(bcode_pop);
      compOne(args);
      compPush(keyval_int(compCountList(args, TOK_LIST)));
      compCode(va ? bcode_varargs : bcode_defargs);
      compOne(n->third);
      procEnd();
      if (n->type == TOK_FUNCX) compCode(bcode_dup);
      compString(n->first);
      compCode(bcode_var);
      break;
    }
    default: break; /* noisy compiler */
  }
}

static keyret
compiler_compile(keyval ** proot, keyval * proc,
                 const char * src, const char * fname, int line)
{
  node * n = NULL;
  Q(parser_parse(&n, src, fname, line));
  comp.work = NULL;
  comp.final = NULL;
  comp.proc = NULL;
  comp.wlen = 0;
  comp.flen = 0;
  comp.counting = 1;
  compOne(n);
  comp.counting = 0;
  comp.wlen += 2;
  comp.work = malloc((size_t)comp.wlen * sizeof(keyval));
  comp.final = malloc((size_t)comp.wlen * sizeof(keyval));
  if (!comp.work || !comp.final) { perror("compiler_compile"); exit(2); }
  comp.wlen = 0;
  procBegin();
  compOne(n);
  procEnd();
  node_delete_chain(parser.chain);
  *proc = keyval_func(comp.proc);
  *proot = comp.final;
  free(comp.work);
  return KEY_OK;
}

/* ********************************************************************** */
/* Key Value Type */
/* ********************************************************************** */

static keytype custom = KEY_CUSTOM;
static const char * custom_names[KEY_CUSTOM_MAX_] = {
  "@end",
  "undef",
  "int",
  "string",
  "mark",
  "bytecode",
  "native",
  "function",
  NULL
};

keytype
keyval_custom_type(const char * name)
{
  keytype i;
  for (i = 0; i < custom; ++i) {
    if (STREQ(name, custom_names[i])) {
      key_error("duplicate type name");
      return 0;
    }
  }
  if (custom >= KEY_CUSTOM_MAX_) {
    key_error("too many types");
    return 0;
  }
  if (key_intern(&name, name)) return 0;
  custom_names[custom] = name;
  return custom++;
}

const char *
keyval_typename(keytype a)
{
  if (a >= custom) return "bad-handle";
  return custom_names[a];
}

int
keyval_tobool(keyval a)
{
  switch (a.type) {
    case KEY_END: break;
    case KEY_MARK: return 0;
    case KEY_UNDEF: return 0;
    case KEY_INT: return a.ival != 0;
    case KEY_STR: return a.sval && *a.sval != '\0';
    case KEY_NAT: return 1;
    case KEY_FUNC: return 1;
    case KEY_BCODE: return 1;
    default: return a.pval != NULL;
  }
  return 0;
}

int
keyval_equal(keyval a, keyval b)
{
  if (a.type == b.type) {
    switch (a.type) {
      case KEY_END: return 1;
      case KEY_MARK: return 1;
      case KEY_UNDEF: return 1;
      case KEY_INT: return a.ival == b.ival;
      case KEY_STR: return STREQ(a.sval, b.sval);
      case KEY_NAT: return a.eval == b.eval;
      case KEY_FUNC: return a.fval == b.fval;
      case KEY_BCODE: return a.bval == b.bval;
      default: return a.pval == b.pval;
    }
  }
  return 0;
}

/* ********************************************************************** */
/* Stack */
/* ********************************************************************** */

static keyret stk_push(stack * s, keyval a) {
  if (s->len >= s->cap) return key_error("stack overflow");
  s->buf[s->len++] = a;
#if TRACK_USAGES
  if (s->max < s->len) s->max = s->len;
#endif
  return KEY_OK;
}
static int stk_length(stack * s) { return s->len; }
static void stk_set_length(stack * s, int n) { s->len = n; }
static void stk_drop(stack * s, int n) { s->len -= n; }
static keyval stk_pop(stack * s) { return s->buf[--s->len]; }
static keyval stk_peek(stack * s) { return s->buf[s->len - 1]; }
static keyval stk_get(stack * s, int n) { return s->buf[s->len - n]; }
static keyval stk_set(stack * s, int n, keyval a) {
  return s->buf[s->len - n] = a;
}
static keyval * stk_ptr(stack * s, int n) {
  return &s->buf[s->len - n];
}
static int stk_find_mark(stack * s) {
  int i;
  for (i = s->len; i--; ) {
    if (s->buf[i].type == KEY_MARK) return i;
  }
  return -1;
}
static int stk_find(stack * s, keyval a) {
  int i;
  for (i = s->len; i--; ) {
    if (keyval_equal(a, s->buf[i])) return i;
  }
  return -1;
}

/* ********************************************************************** */
/* Global Environment */
/* ********************************************************************** */

#if TRACK_USAGES
static int global_max;
#endif

static keyval globalbuf[GLOBAL_MAX*2];
#if TRACK_USAGES
static stack global = { GLOBAL_MAX*2, 0, 0, globalbuf };
#else
static stack global = { GLOBAL_MAX*2, 0, globalbuf };
#endif

#define OPER   (&e->oper)
#define SCOPE  (&e->scope)
#define EXEC   (&e->exec)
#define GLOBAL (&global)

/* ********************************************************************** */
/* Scope Stack */
/* ********************************************************************** */

static keyret scope_push(keyenv * e) {
  Q(stk_push(SCOPE, keyval_mark()));
  return stk_push(SCOPE, keyval_mark());
}
static void scope_pop(keyenv * e) {
  stk_set_length(SCOPE, stk_find_mark(SCOPE));
  stk_pop(SCOPE);
}

static int scope_find(const stack * s, const char * name, keyval ** valp) {
  int i;
  for (i = s->len - 2; i >= 0 && s->buf[i].type != KEY_MARK; i -= 2) {
    if (STREQ(s->buf[i].sval, name)) { *valp = &s->buf[i+1]; return 1; }
  }
  if (i > 0) { /* if scanning multi-scoped stack, check file scope */
    for (i = 2; i < s->len && s->buf[i].type != KEY_MARK; i += 2) {
      if (STREQ(s->buf[i].sval, name)) { *valp = &s->buf[i+1]; return 1; }
    }
  }
  return 0;
}
static int scope_find_full(const keyenv * e, const char * name, keyval ** valp) {
  return scope_find(SCOPE, name, valp) || scope_find(GLOBAL, name, valp);
}

static keyret scope_load(const keyenv * e, const char * name, keyval * valp) {
  keyval * p;
  if (!scope_find_full(e, name, &p)) return key_error("'%s' not defined", name);
  *valp = *p;
  return KEY_OK;
}
static keyret scope_store(keyenv * e, const char * name, keyval val) {
  keyval * p;
  if (!scope_find_full(e, name, &p)) return key_error("'%s' not defined", name);
  *p = val;
  return KEY_OK;
}

static keyret scope_make_(stack * s, const char * name, keyval val) {
  Q(stk_push(s, keyval_str(name)));
  return stk_push(s, val);
}
static keyret scope_gdef(const char * name, keyval val) {
  keyval * valp;
  if (scope_find(GLOBAL, name, &valp)) return key_error("'%s' redeclared", name);
#if TRACK_USAGES
  global_max++;
#endif
  return scope_make_(GLOBAL, name, val);
}
static keyret scope_def(keyenv * e, const char * name, keyval val) {
  keyval * valp;
  if (scope_find_full(e, name, &valp)) return key_error("'%s' redeclared", name);
  return scope_make_(SCOPE, name, val);
}

/* ********************************************************************** */
/* Bytecode Function to Execute a Bytecode Array */
/* ********************************************************************** */

static keyret
bcode_proc_next(keyenv * e)
{
  int i = stk_get(EXEC, 2).ival; /* 1:body, 2:idx */
  keyval * proc = stk_get(EXEC, 1).fval;
  keyval elem = proc[i++];
  if (elem.type == KEY_END || proc[i].type == KEY_END) {
    stk_drop(EXEC, 2);
  } else {
    stk_set(EXEC, 2, keyval_int(i));
    Q(stk_push(EXEC, keyval_bcode(bcode_proc_next)));
  }
  return stk_push(EXEC, elem);
}

/* both fn(){} and conditional expressions/statements */
static keyret
exec_func(keyenv * e, keyval t) /* t.type == KEY_FUNC */
{
  Q(stk_push(EXEC, keyval_int(0)));
  Q(stk_push(EXEC, t));
  return stk_push(EXEC, keyval_bcode(bcode_proc_next));
}

/* ********************************************************************** */
/* Bytecode Functions */
/* ********************************************************************** */

#define BCODE_INFIX(NAME,EXPR) \
static keyret NAME (keyenv * e) { /* val val -> val */ \
  keyval b = stk_pop(OPER); \
  keyval a = stk_pop(OPER); \
  Q(key_type_require(a, KEY_INT)); \
  Q(key_type_require(b, KEY_INT)); \
  return stk_push(OPER, keyval_int(EXPR)); \
}
BCODE_INFIX(bcode_add, a.ival + b.ival)
BCODE_INFIX(bcode_sub, a.ival - b.ival)
BCODE_INFIX(bcode_mul, a.ival * b.ival)
BCODE_INFIX(bcode_div, a.ival / b.ival)
BCODE_INFIX(bcode_mod, a.ival % b.ival)
BCODE_INFIX(bcode_and, a.ival & b.ival)
BCODE_INFIX(bcode_or,  a.ival | b.ival)
BCODE_INFIX(bcode_xor, a.ival ^ b.ival)
BCODE_INFIX(bcode_lsh, a.ival << b.ival)
BCODE_INFIX(bcode_rsh, (int)(((signed)a.ival) >> b.ival))
BCODE_INFIX(bcode_ush, (int)(((unsigned)a.ival) >> b.ival))

#define BCODE_CMP(NAME,OP) \
static keyret NAME (keyenv * e) { /* val val -> val */ \
  keyval b = stk_pop(OPER); \
  keyval a = stk_pop(OPER); \
  int r = 0; \
  if (a.type == KEY_INT) { \
    if (b.type == KEY_INT) r = a.ival OP b.ival; \
    else return key_type_error(b); \
  } else if (a.type == KEY_STR) { \
    if (b.type == KEY_STR) r = strcmp(a.sval, b.sval) OP 0; \
    else return key_type_error(b); \
  } else { \
    return key_type_error(a); \
  } \
  return stk_push(OPER, keyval_int(r)); \
}
BCODE_CMP(bcode_gt, > )
BCODE_CMP(bcode_lt, < )
BCODE_CMP(bcode_ge, >= )
BCODE_CMP(bcode_le, <= )

#define BCODE_EQUAL(NAME,OP) \
static keyret NAME (keyenv * e) { /* val val -> val */ \
  keyval b = stk_pop(OPER); \
  keyval a = stk_pop(OPER); \
  return stk_push(OPER, keyval_int(keyval_equal(a, b) OP 1)); \
}
BCODE_EQUAL(bcode_eq, ==)
BCODE_EQUAL(bcode_ne, !=)

#define BCODE_TYPEIS(NAME,OP) \
static keyret NAME (keyenv * e) { /* val val -> int */ \
  keyval b = stk_pop(OPER); \
  const char * type = keyval_typename(stk_pop(OPER).type); \
  int r; \
  Q(key_type_require(b, KEY_STR)); \
  r = STREQ(type, b.sval) OP 1; \
  return stk_push(OPER, keyval_int(r)); \
}
BCODE_TYPEIS(bcode_is, ==)
BCODE_TYPEIS(bcode_isnot, !=)

#define BCODE_PREFIX(NAME,EXPR) \
static keyret NAME (keyenv * e) { /* val -> val */ \
  keyval a = stk_pop(OPER); \
  Q(key_type_require(a, KEY_INT)); \
  return stk_push(OPER, keyval_int(EXPR)); \
}
BCODE_PREFIX(bcode_not, ~a.ival)
BCODE_PREFIX(bcode_neg, -a.ival)

static keyret bcode_lognot(keyenv * e) { /* val -> int */
  keyval a = stk_pop(OPER);
  return stk_push(OPER, keyval_int(!keyval_tobool(a)));
}
static keyret bcode_isdef(keyenv * e) { /* val -> int */
  keyval a = stk_pop(OPER);
  return stk_push(OPER, keyval_int(a.type != KEY_UNDEF));
}
static keyret bcode_typeof(keyenv * e) { /* val -> str */
  keytype type = stk_pop(OPER).type;
  return stk_push(OPER, keyval_str(keyval_typename(type)));
}

#define BCODE_SHORTFIX(NAME,EXPR) \
static keyret NAME (keyenv * e) { /* val proc -> val */ \
  keyval b = stk_pop(OPER); \
  keyval a = stk_pop(OPER); \
  if (EXPR) return exec_func(e, b); \
  return stk_push(OPER, a); \
}
BCODE_SHORTFIX(bcode_logand, keyval_tobool(a) )
BCODE_SHORTFIX(bcode_logor,  !keyval_tobool(a) )
BCODE_SHORTFIX(bcode_coal,   a.type == KEY_UNDEF )

static keyret bcode_load(keyenv * e) { /* str -> val */
  keyval a;
  Q(scope_load(e, stk_pop(OPER).sval, &a));
  return stk_push(OPER, a);
}
static keyret bcode_assign(keyenv * e) { /* val str -> val */
  const char * name = stk_pop(OPER).sval;
  keyval val = stk_peek(OPER);
  return scope_store(e, name, val);
}
static keyret bcode_var(keyenv * e) { /* val|proc str -> - */
  const char * name = stk_pop(OPER).sval;
  keyval val = stk_pop(OPER);
  return scope_def(e, name, val);
}

static keyret bcode_dup(keyenv * e) { /* any -> any any */
  return stk_push(OPER, stk_peek(OPER));
}
static keyret bcode_index(keyenv * e) { /* any ... n -> any ... n any */
  return stk_push(OPER, stk_get(OPER, stk_peek(OPER).ival + 1));
}
static keyret bcode_pop(keyenv * e) { /* any -> - */
  stk_drop(OPER, 1);
  return KEY_OK;
}
static keyret bcode_popexec(keyenv * e) { /* - -> - */
  /* used for storing function names on the exec stack */
  stk_drop(EXEC, 1);
  return KEY_OK;
}

static keyret bcode_if(keyenv * e) { /* val proc -> - */
  keyval t = stk_pop(OPER);
  keyval c = stk_pop(OPER);
  if (keyval_tobool(c)) return exec_func(e, t);
  return KEY_OK;
}
static keyret bcode_ifelse(keyenv * e) { /* val proc proc -> - */
  keyval f = stk_pop(OPER);
  keyval t = stk_pop(OPER);
  keyval c = stk_pop(OPER);
  if (keyval_tobool(c)) return exec_func(e, t);
  return exec_func(e, f);
}

static keyret bcode_endcall(keyenv * e) { /* - -> undef */
  /* mark end-of-function on stack, or return without a 'return' */
  scope_pop(e);
  return stk_push(OPER, keyval_undef());
}
static keyret bcode_return(keyenv * e) { /* val -> val */
  /* pop exec stack up to and including endcall */
  int i = stk_find(EXEC, keyval_bcode(bcode_endcall));
  stk_set_length(EXEC, i);
  scope_pop(e);
  return KEY_OK;
}
static keyret bcode_call(keyenv * e) { /* vals...N fn -> vals...N || rv */
  keyval proc = stk_pop(OPER);
  if (proc.type == KEY_NAT) {
    int argc = stk_pop(OPER).ival;
    keyval rv = keyval_undef();
    keyret err = proc.eval(e, &rv, argc, stk_ptr(OPER, argc));
    stk_drop(OPER, argc);
    stk_push(OPER, rv);
    return err;
  }
  if (proc.type == KEY_FUNC) {
    Q(stk_push(EXEC, proc.fval[0])); /* name, for stack tracing */
    Q(stk_push(EXEC, keyval_bcode(bcode_popexec)));
    Q(stk_push(EXEC, keyval_bcode(bcode_endcall)));
    Q(scope_push(e));
    return exec_func(e, proc);
  }
  return key_type_error(proc);
}
static keyret bcode_varargs(keyenv * e) { /* vals...N names...N -> - */
  int nlen = stk_get(OPER, 1).ival;
  keyval * names = stk_ptr(OPER, nlen + 1);
  int vlen = stk_get(OPER, nlen + 2).ival;
  keyval * vals = stk_ptr(OPER, nlen + 2 + vlen);
  keyval rname = names[nlen - 1];
  int rlen = vlen >= nlen ? vlen - (nlen - 1) : 0;
  int i;
  for (i = 0; i < nlen - 1 && i < vlen; ++i) {
    Q(scope_def(e, names[i].sval, vals[i]));
  }
  for ( ; i < nlen - 1; ++i) {
    Q(scope_def(e, names[i].sval, keyval_undef()));
  }
  stk_drop(OPER, nlen + 2); /* stack: vals... rvals... */
  if (rlen) memmove(vals, vals + vlen - rlen, (size_t)rlen * sizeof(keyval));
  stk_drop(OPER, (vlen - rlen));
  Q(stk_push(OPER, keyval_int(rlen))); /* stack: rvals... rlen */
  /* var rname = __array__(rlen, rvals); */
  Q(stk_push(OPER, keyval_str("__array__")));
  Q(stk_push(EXEC, keyval_bcode(bcode_var)));
  Q(stk_push(EXEC, rname));
  Q(stk_push(EXEC, keyval_bcode(bcode_call)));
  return bcode_load(e);
}
static keyret bcode_defargs(keyenv * e) { /* vals...N names...N -> - */
  int nlen = stk_get(OPER, 1).ival;
  keyval * names = stk_ptr(OPER, nlen + 1);
  int vlen = stk_get(OPER, nlen + 2).ival;
  keyval * vals = stk_ptr(OPER, nlen + 2 + vlen);
  int i;
  for (i = 0; i < nlen && i < vlen; ++i) {
    Q(scope_def(e, names[i].sval, vals[i]));
  }
  for ( ; i < nlen; ++i) {
    Q(scope_def(e, names[i].sval, keyval_undef()));
  }
  stk_drop(OPER, nlen + 2 + vlen);
  return KEY_OK;
}

static keyret bcode_break_mark(keyenv * e) { e = e; return KEY_OK; }
static keyret bcode_continue_mark(keyenv * e) { e = e; return KEY_OK; }
static keyret bcode_loop_cond(keyenv * e);
static keyret bcode_loop_inc(keyenv * e);
static keyret bcode_break(keyenv * e) {
  stk_set_length(EXEC, stk_find(EXEC, keyval_bcode(bcode_break_mark)));
  return KEY_OK;
}
static keyret bcode_continue(keyenv * e) {
  /* pop until the loop, keep the continue-mark, continue with the cond */
  stk_set_length(EXEC, stk_find(EXEC, keyval_bcode(bcode_continue_mark)) + 1);
  return stk_push(EXEC, keyval_bcode(bcode_loop_inc));
}
static keyret bcode_loop_inc(keyenv * e) {
  /* queue inc,cond for exec */
  keyval inc = stk_get(EXEC, 3); /* 1: continue, 2:cond, 3:inc, 4:body, 5:break */
  Q(stk_push(EXEC, keyval_bcode(bcode_loop_cond)));
  return exec_func(e, inc);
}
static keyret bcode_loop_body(keyenv * e) { /* val -> - */
  /* queue body,inc for exec */
  keyval body = stk_get(EXEC, 4); /* 1: continue, 2:cond, 3:inc, 4:body, 5:break */
  int pred = keyval_tobool(stk_pop(OPER));
  if (pred) {
    Q(stk_push(EXEC, keyval_bcode(bcode_loop_inc)));
    return exec_func(e, body);
  }
  stk_drop(EXEC, 4);
  return KEY_OK;
}
static keyret bcode_loop_cond(keyenv * e) { /* - -> val */
  /* queue cond,body for exec */
  keyval cond = stk_get(EXEC, 2); /* 1: continue, 2:cond, 3:inc, 4:body, 5:break */
  Q(stk_push(EXEC, keyval_bcode(bcode_loop_body)));
  return exec_func(e, cond);
}
static keyret bcode_doloop(keyenv * e) {
  Q(stk_push(EXEC, keyval_bcode(bcode_break_mark)));
  Q(stk_push(EXEC, stk_pop(OPER))); /* body */
  Q(stk_push(EXEC, stk_pop(OPER))); /* inc */
  Q(stk_push(EXEC, stk_pop(OPER))); /* cond */
  Q(stk_push(EXEC, keyval_bcode(bcode_continue_mark)));
  Q(stk_push(EXEC, keyval_bcode(bcode_loop_inc)));
  return exec_func(e, stk_get(EXEC, 5));
}
static keyret bcode_loop(keyenv * e) {
  Q(stk_push(EXEC, keyval_bcode(bcode_break_mark)));
  Q(stk_push(EXEC, stk_pop(OPER))); /* body */
  Q(stk_push(EXEC, stk_pop(OPER))); /* inc */
  Q(stk_push(EXEC, stk_pop(OPER))); /* cond */
  Q(stk_push(EXEC, keyval_bcode(bcode_continue_mark)));
  return stk_push(EXEC, keyval_bcode(bcode_loop_cond));
}

/* ********************************************************************** */
/* Script Environment */
/* ********************************************************************** */

void
keyenv_reset(keyenv * e)
{
  stack * s = SCOPE;
  int i;
  for (i = 2; i < s->len && s->buf[i].type != KEY_MARK; i += 2) {
    if (s->buf[i].type == KEY_MARK) { s->len = i; break; }
  }
  OPER->len = 0;
  EXEC->len = 0;
}

int
keyenv_running(const keyenv * e)
{
  return e->exec.len > 0;
}

keyret
keyenv_run(keyenv * e)
{
  keyret err = KEY_OK;
  while (stk_length(EXEC) && err == KEY_OK) {
    keyval a = stk_pop(EXEC);
    if (a.type == KEY_BCODE) {
      err = a.bval(e);
    } else {
      err = stk_push(OPER, a);
    }
  }
  return err;
}

void
keyenv_resume(keyenv * e, keyval rv)
{
  stk_set(OPER, 1, rv);
}

keyret
keyenv_pop(keyenv * e, keyval * rv)
{
  if (stk_length(OPER) == 0) *rv = keyval_undef();
  else *rv = stk_pop(OPER);
  return KEY_OK;
}

keyret
keyenv_push(keyenv * e, const char * name, int argc, const keyval * argv)
{
  int i;
  Q(key_intern(&name, name));
  for (i = 0; i < argc; ++i) Q(stk_push(OPER, argv[i]));
  Q(stk_push(OPER, keyval_int(argc)));
  Q(stk_push(OPER, keyval_str(name)));
  Q(stk_push(EXEC, keyval_bcode(bcode_call)));
  return stk_push(EXEC, keyval_bcode(bcode_load));
}

keyret
keyenv_interrupt(keyenv * e, const char * name, int argc, const keyval * argv)
{
  /* interrupts don't have return values */
  Q(stk_push(EXEC, keyval_bcode(bcode_pop)));
  return keyenv_push(e, name, argc, argv);
}

static keyret
bcode_pause(keyenv * e)
{
  e = e; return KEY_PAUSE;
}

keyret
keyenv_callback(keyenv * e, keyval * rv, keyval func, int argc, const keyval * argv)
{
  /* call immediately, stop when done, and pop retval */
  keyret err;
  int i;
  for (i = 0; i < argc; ++i) Q(stk_push(OPER, argv[i]));
  Q(stk_push(OPER, keyval_int(argc)));
  Q(stk_push(OPER, func));
  Q(stk_push(EXEC, keyval_bcode(bcode_pause)));
  Q(stk_push(EXEC, keyval_bcode(bcode_call)));
  err = keyenv_run(e);
  if (err != KEY_PAUSE) return err;
  return keyenv_pop(e, rv);
}

#define SIZEOF_KEYENV  (sizeof(keyenv) + ( \
    (OPER_DEPTH + SCOPE_DEPTH + EXEC_DEPTH) * sizeof(keyval) ))

keyret
keyenv_new(keyenv ** ep, const char * src, const char * fname, int line)
{
  keyenv * e;
  keyval proc;
  e = *ep = calloc(1, SIZEOF_KEYENV);
  if (!e) { perror("keyenv_new"); exit(2); }
  e->oper.buf = e->buf; e->oper.cap = OPER_DEPTH;
  e->scope.buf = e->oper.buf + OPER_DEPTH; e->scope.cap = SCOPE_DEPTH;
  e->exec.buf = e->scope.buf + SCOPE_DEPTH; e->exec.cap = EXEC_DEPTH;
  Q(compiler_compile(&e->root, &proc, src, fname, line));
  Q(scope_push(e));
  return exec_func(e, proc);
}

#if TRACK_USAGES
static int oper_max;
static int scope_max;
static int exec_max;
#endif

void
keyenv_delete(keyenv * e)
{
  if (e) {
#if TRACK_USAGES
    if (oper_max < OPER->max) oper_max = OPER->max;
    if (scope_max < SCOPE->max) scope_max = SCOPE->max;
    if (exec_max < EXEC->max) exec_max = EXEC->max;
#endif
    if (e->root) {
      free(e->root);
      e->root = NULL;
    }
    free(e);
  }
}

size_t
keyenv_stack_trace(const keyenv * e, char * buf, size_t buflen)
{
  keyval mark = keyval_bcode(bcode_popexec);
  const stack * s = EXEC;
  size_t n = 0;
  int i;
#define BUF     (buf ? buf + n : buf)
#define BUFLEN  (buflen > n ? buflen - n : 0)
  for (i = 0; i < s->len; ++i) {
    if (keyval_equal(s->buf[i], mark)) {
      keyval name = s->buf[i - 1];
      if (name.type == KEY_STR) {
        n += (size_t)snprintf(BUF, BUFLEN, "in %s:\n", name.sval);
      } else {
        n += (size_t)snprintf(BUF, BUFLEN, "in <unknown>:\n");
      }
    }
  }
  if (buflen) buf[buflen - 1] = '\0';
  return n;
#undef BUFLEN
#undef BUF
}

int
keyenv_has(const keyenv * e, const char * name)
{
  keyval * p;
  return scope_find(SCOPE, name, &p);
}

keyret
keyenv_get(const keyenv * e, const char * name, keyval * valp)
{
  keyval * p;
  if (!scope_find(SCOPE, name, &p)) return key_error("'%s' not defined", name);
  *valp = *p;
  return KEY_OK;
}

keyret
keyenv_set(keyenv * e, const char * name, keyval val)
{
  keyval * p;
  if (!scope_find(SCOPE, name, &p)) return key_error("'%s' not defined", name);
  *p = val;
  return KEY_OK;
}

keyret
keyenv_def(keyenv * e, const char * name, keyval val)
{
  Q(key_intern(&name, name));
  return scope_def(e, name, val);
}

/* ********************************************************************** */
/* Global Environment */
/* ********************************************************************** */

int
key_global_has(const char * name)
{
  keyval * p;
  return scope_find(GLOBAL, name, &p);
}

keyret
key_global_get(const char * name, keyval * valp)
{
  keyval * p;
  if (!scope_find(GLOBAL, name, &p)) return key_error("'%s' not defined", name);
  *valp = *p;
  return KEY_OK;
}

keyret
key_global_set(const char * name, keyval val)
{
  keyval * p;
  if (!scope_find(GLOBAL, name, &p)) return key_error("'%s' not defined", name);
  *p = val;
  return KEY_OK;
}

keyret
key_global_def(const char * name, keyval val)
{
  Q(key_intern(&name, name));
  return scope_gdef(name, val);
}

keyret
key_global_fndef(const char * name, key_native_fn eval)
{
  Q(key_intern(&name, name));
  return scope_gdef(name, keyval_native(eval));
}

/* ********************************************************************** */
/* Default Functions */
/* ********************************************************************** */

keyret
key_default_fail(keyenv * e, keyval * rv, int argc, const keyval * argv)
{
  char * p = key_errmsg;
  char * endp = ERRMSG_LIMIT;
  int i;
  for (i = 0; i < argc; ++i) {
    keyval a = argv[i];
    if (a.type == KEY_INT) {
      p += snprintf(p, (size_t)(endp - p), "%d", a.ival);
    } else if (a.type == KEY_STR) {
      p += snprintf(p, (size_t)(endp - p), "%s", a.sval);
    } else if (a.type == KEY_UNDEF) {
      p += snprintf(p, (size_t)(endp - p), "undef");
    } else {
      p += snprintf(p, (size_t)(endp - p), "<%s:%p>",
                    keyval_typename(a.type), a.pval);
    }
  }
  *(endp - 1) = '\0';
  e = e; rv = rv; return KEY_ERROR;
}

keyret
key_default_debug(keyenv * e, keyval * rv, int argc, const keyval * argv)
{
  int i;
  for (i = 0; i < argc; ++i) {
    keyval a = argv[i];
    if (a.type == KEY_INT) fprintf(stderr, "%d", a.ival);
    else if (a.type == KEY_STR) fprintf(stderr, "%s", a.sval);
    else if (a.type == KEY_UNDEF) fprintf(stderr, "<undef>");
    else fprintf(stderr, "<%s:%p>", keyval_typename(a.type), a.pval);
  }
  fprintf(stderr, "\n");
  fflush(stderr);
  e = e; rv = rv; return KEY_OK;
}

keyret
key_default_trace(keyenv * e, keyval * rv, int argc, const keyval * argv)
{
  char buf[1024];
  keyenv_stack_trace(e, buf, sizeof(buf));
  fprintf(stderr, "%s", buf);
  fflush(stderr);
  rv = rv; argc = argc; argv = argv; return KEY_OK;
}

keyret
key_default_pause(keyenv * e, keyval * rv, int argc, const keyval * argv)
{
  e = e; rv = rv; argc = argc; argv = argv;
  return KEY_PAUSE;
}

keyret
key_global_defaults(void)
{
  static int inited = 0;
  if (inited) return KEY_OK;
  Q(key_global_fndef("debug", key_default_debug));
  Q(key_global_fndef("fail", key_default_fail));
  Q(key_global_fndef("trace", key_default_trace));
  Q(key_global_fndef("pause", key_default_pause));
  inited = 1;
  return KEY_OK;
}

/* ********************************************************************** */
/* Debugging Functions */
/* ********************************************************************** */

void
key_print_stats(void)
{
  printf("sizeof keyenv: %zu\n", SIZEOF_KEYENV);
#if TRACK_USAGES
  printf("internment room: %zu bytes (%zu KB)\n", intern_max, (intern_max + 1023) / 1024);
  printf("global variable count: %d\n", global_max);
  printf("oper stack length: %d\n", oper_max);
  printf("scope stack length: %d\n", scope_max);
  printf("exec stack length: %d\n", exec_max);
#endif
}

/* ********************************************************************** */
/* Basic Interpreter (Test Harness) */
/* ********************************************************************** */

#ifdef TEST

static void print_proc(keyval * proc);
static void print_one(keyval a) {
  switch (a.type) {
    case KEY_END:   printf("$"); break;
    case KEY_MARK:  printf("@"); break;
    case KEY_UNDEF: printf("undef"); break;
    case KEY_INT:   printf("(%d)", a.ival); break;
    case KEY_STR: {
      char ebuf[512];
      key_escape(ebuf, 512, a.sval);
      printf("\"%s\"", ebuf);
      break;
    }
    case KEY_FUNC:  printf("{ "); print_proc(a.fval); printf("}"); break;
    case KEY_NAT:   printf("<native:%p>", a.pval); break; /* can't cast ptr-to-func, so we cheat */
    case KEY_BCODE: printf("<bcode:%p>", a.pval); break;
    default: {
      printf("<%s:%p>", keyval_typename(a.type), a.pval);
      break;
    }
  }
}
static void
print_proc(keyval * proc)
{
  int i;
  if (!proc) { printf("<nullproc>"); return; }
  for (i = 0; proc[i].type != KEY_END && i < 9999; ++i) {
    print_one(proc[i]);
    printf(" ");
  }
  if (i >= 9999) printf("\nerror: end not found\n");
  fflush(stdout);
}

#define SCRIPT_LEN  (32 * 1024)
static char scriptbuf[SCRIPT_LEN];

static void
freadall(FILE * fp)
{
  size_t len = fread(scriptbuf, sizeof(char), SCRIPT_LEN - 1, fp);
  scriptbuf[len] = '\0';
}

static keyret
runall(keyenv * e)
{
  int i = 0;
  keyret err;
  while ((err = keyenv_run(e)) == KEY_PAUSE) {
    keyenv_resume(e, keyval_int(i++));
  }
  return err;
}

#define arrlength  (32)
static keytype arrtype;
static int arrvalue[arrlength];

static keyret
cb_member(keyenv * e, keyval * rv, int argc, const keyval * argv)
{
  int idx;
  int * arr;

#if 0
  printf("member:");
  if (argc > 0) { printf(" "); print_one(argv[0]); }
  if (argc > 1) { printf(" "); print_one(argv[1]); }
  if (argc > 2) { printf(" "); print_one(argv[2]); }
  if (argc > 3) { printf(" "); print_one(argv[3]); }
  printf("\n");
#endif

  Q(key_type_require(argv[0], arrtype));
  arr = argv[0].pval;

  if (argv[1].type == KEY_STR) {
    if (!strcmp(argv[1].sval, "memberfunc") ||
        !strcmp(argv[1].sval, "sort")) {
      if (keyenv_get(e, argv[1].sval, rv) == KEY_OK) return KEY_OK;
      return key_global_get(argv[1].sval, rv);
    }
  }

  if (argv[1].type == KEY_INT) idx = argv[1].ival;
  else if (argv[1].type == KEY_STR) idx = argv[1].sval[0] - 'a';
  else return key_type_error(argv[1]);

  if (argc == 3) {
    Q(key_type_require(argv[2], KEY_INT));
    arr[idx] = argv[2].ival;
  }
  *rv = keyval_int(arr[idx]);
  return KEY_OK;
}

static keyret
cb_sort(keyenv * e, keyval * rv, int argc, const keyval * argv)
{
  /* test callbacks by sorting an array */
  keyval cmp;
  int * arr;
  int len;
  int i;
  int j;

#if 0
  printf("sort:");
  if (argc > 0) { printf(" "); print_one(argv[0]); }
  if (argc > 1) { printf(" "); print_one(argv[1]); }
  if (argc > 2) { printf(" "); print_one(argv[2]); }
  if (argc > 3) { printf(" "); print_one(argv[3]); }
  printf("\n");
#endif

#define SWAP(A,I,J) do{ \
  int t_ = (A)[(I)]; \
  (A)[(I)] = (A)[(J)]; \
  (A)[(J)] = t_; \
}while(0)
  if (argc < 3) return key_error("missing argument to sort\n");

  Q(key_type_require(argv[0], arrtype));
  arr = argv[0].pval;
  Q(key_type_require(argv[1], KEY_INT));
  len = argv[1].ival;
  Q(key_type_require_fn(argv[2]));
  cmp = argv[2];

  for (i = 1; i < len; ++i) { /* simple insertion sort */
    for (j = i; j > 0; --j) {
      keyval ab[2] = { keyval_int(arr[j - 1]), keyval_int(arr[j]) };
      keyval crv;
      Q(keyenv_callback(e, &crv, cmp, 2, ab));
      if (crv.ival <= 0) break;
      SWAP(arr, j, j - 1);
    }
  }
#undef SWAP
  rv = rv; return KEY_OK;
}

#define QE(EX) do{ if ((EX) != KEY_OK) goto errorL; }while(0)

static void
run(const char * src, const char * fname)
{
  keyret err;
  keyval a;
  keyenv * e = NULL;

  arrtype = keyval_custom_type("array");
  QE(key_global_fndef("__member__", cb_member));
  QE(key_global_fndef("sort", cb_sort));
  QE(key_global_def("ARRAY", keyval_custom(arrtype, arrvalue)));
  QE(key_global_defaults());

  QE(keyenv_new(&e, src, fname, 1));
  while ((err = keyenv_run(e)) == KEY_PAUSE) /**/;
  QE(err);

  QE(keyenv_push(e, "main", 0, NULL));
  if ((err = runall(e)) == KEY_OK) {
    QE(keyenv_pop(e, &a));
    print_one(a);
    printf("\n");
    fflush(stdout);
  } else {
errorL:;
    char buf[1024] = "";
    if (e) keyenv_stack_trace(e, buf, sizeof(buf));
    fprintf(stderr, "%serror: %s\n", buf, key_errmsg);
    fflush(stderr);
  }
  if (e) keyenv_delete(e);
}

int
main(int argc, char ** argv)
{
  const char * fname = "<stdin>";
  FILE * fp;
  if (argc > 1) {
    fname = argv[1];
    fp = fopen(fname, "r");
    if (!fp) { perror("main"); exit(2); }
    freadall(fp);
    fclose(fp);
  } else {
    freadall(stdin);
  }
  run(scriptbuf, fname);
  key_print_stats();
  return 0;
}

#endif /* TEST */

/* ********************************************************************** */
/* ********************************************************************** */
