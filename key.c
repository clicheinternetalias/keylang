/* ********************************************************************** */
/* KeyLang: A Scripting Language in the Key of C */
/* ********************************************************************** */

#include "key.h"

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <errno.h>

#define ERROR_BUFLEN   (128)       /* length of erorr message buffer */
#define TOKEN_BUFLEN   (1024)      /* max length of a token */
#define INTERN_BUFLEN  (16 * 1024) /* interned string storage */
#define NATIVEDEF_MAX  (256)       /* max number of native funcs/opers */
#define GLOBAL_MAX     (128)       /* max number of global variables */
#define OPER_DEPTH     (128)       /* operand stack size */
#define SCOPE_DEPTH    (256)       /* local scope stack size */
#define EXEC_DEPTH     (128)       /* exec stack size */

#define Q(EX) do{ keyret_e err_ = (EX); if (err_ != KEY_OK) return err_; }while(0)

#define keyval_end()       ((keyval){ .type=(KEY_END)                 })
#define keyval_mark()      ((keyval){ .type=(KEY_MARK)                })
#define keyval_bcode(V)    ((keyval){ .type=(KEY_BCODE), .bval=(V)    })
#define keyval_native(V)   ((keyval){ .type=(KEY_NAT),   .eval=(V)    })
#define keyval_func(V)     ((keyval){ .type=(KEY_FUNC),  .fval=(V)    })

/* ********************************************************************** */
/* ********************************************************************** */

typedef enum token_e {
  TOK_EOF = 0,
  /* lexical and/or ast */
  TOK_ASSIGN, TOK_ASFIX, TOK_BREAK, TOK_CBRACE, TOK_COMMA, TOK_CONT,
  TOK_CPAREN, TOK_ELSE, TOK_FUNC, TOK_GFUNC, TOK_GVAR, TOK_IDENT, TOK_IF,
  TOK_INFIX, TOK_INT, TOK_OBRACE, TOK_OPAREN, TOK_PREFIX,
  TOK_RETURN, TOK_SEMI, TOK_SHFIX, TOK_STRING, TOK_UNDEF, TOK_VAR, TOK_WHILE,
  /* ast-only */
  TOK_LIST, TOK_NATIVE, TOK_CALL, TOK_ARGLIST
} token_t;

static char *
tok_string(token_t tk)
{
  static char * names[] = {
    "end of file", "=", "asfix", "break", "}", ",", "continue",
    ")", "else", "fn", "gfn", "gvar", "identifier", "if",
    "infix", "integer", "{", "(", "prefix",
    "return", ";", "shortfix", "string", "undef", "var", "while",
    "list", "native", "call", "alist"
  };
  return names[tk];
}

typedef struct node node;
struct node {
  token_t type;
  union {
    int ival;
    const char * sval;
    key_native_fn eval;
  };
  node * first;
  node * second;
  node * third;
  node * chain;
};

struct native {
  token_t type;
  bool pre;
  const char * name;
  key_native_fn eval;
};
static struct native * native_find(const char * name);

typedef struct stack stack;
struct stack {
  int cap;
  int len;
  keyval * buf;
};
struct keyenv {
  stack oper;
  stack scope;
  stack exec;
  keyval * root; /* bytecode allocation pointer */
  keyval buf[]; /* all the stack space */
};

static keyret_e bcode_load(keyenv * e);
static keyret_e bcode_assign(keyenv * e);
static keyret_e bcode_dup(keyenv * e);
static keyret_e bcode_pop(keyenv * e);
static keyret_e bcode_if(keyenv * e);
static keyret_e bcode_ifelse(keyenv * e);
static keyret_e bcode_while(keyenv * e);
static keyret_e bcode_break(keyenv * e);
static keyret_e bcode_continue(keyenv * e);
static keyret_e bcode_unary(keyenv * e);
static keyret_e bcode_binary(keyenv * e);
static keyret_e bcode_native(keyenv * e);
static keyret_e bcode_call(keyenv * e);
static keyret_e bcode_return(keyenv * e);
static keyret_e bcode_var(keyenv * e);
static keyret_e bcode_gvar(keyenv * e);
static keyret_e bcode_defargs(keyenv * e);

/* ********************************************************************** */
/* Errors */
/* ********************************************************************** */

char key_errmsg[ERROR_BUFLEN];
#define ERRMSG_LIMIT  (key_errmsg + ERROR_BUFLEN)

keyret_e
key_error(const char * fmt, ...)
{
  va_list ap;
  va_start(ap, fmt);
  vsnprintf(key_errmsg, ERROR_BUFLEN, fmt, ap);
  va_end(ap);
  return KEY_ERROR;
}

/* ********************************************************************** */
/* String Internment */
/* ********************************************************************** */

static char internbuf[INTERN_BUFLEN];
#define INTERN_LIMIT  (internbuf + INTERN_BUFLEN)

keyret_e
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
  if ((size_t)(INTERN_LIMIT - p) <= n) return key_error("out of internment room");
  strcpy(p, src);
  *(p + n) = '\0'; /* double zero at end make it all work */
#ifdef TEST_INTERN
  printf("interned: %u\n", (unsigned)((size_t)(p - internbuf) + n));
#endif
  *dst = p;
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

static bool isident(int c) {
  return isalnum(c) || c == '_' || c >= 0x80;
}
static bool isoper(int c) {
  static char * special = "(){},;_";
  return ispunct(c) && strchr(special, c) == NULL;
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

static keyret_e tokerror(const char * fmt, ...) KEY_FORMAT(1, 2);
static keyret_e
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

static keyret_e
gatherString(int quote)
{
  bool sawslashcr = false;
  int c;
  while ((c = nextChar()) != quote) {
    if (c == '\\') {
      switch (c = nextChar()) {
        case EOF: return tokerror("unexpected end of file"); break;
        case 'n': APPEND('\n'); break;
        case 'r': APPEND('\r'); break;
        case 't': APPEND('\t'); break;
        case '\r': sawslashcr = true; break;
        case '\n': break;
        default: APPEND(c); break;
      }
    } else if (sawslashcr) {
      if (c != '\n') APPEND(c);
      sawslashcr = false;
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

static keyret_e
gather(int c, bool (*pred)(int))
{
  APPEND(c);
  while ((c = nextChar()) != EOF) {
    if (!pred(c)) { ungetChar(); break; }
    APPEND(c);
    if (OUTOFROOM()) return tokerror("token too long");
  }
  ZERO();
  return KEY_OK;
}
static keyret_e gatherWord(int c) { return gather(c, isident); }
static keyret_e gatherOper(int c) { return gather(c, isoper); }
static void gatherChar(int c) { APPEND(c); ZERO(); }

static void
skipComment(int type)
{
  int c;
  if (type == '*') {
    bool sawstar = false;
    while ((c = nextChar()) != EOF) {
      if (sawstar && c == '/') break;
      sawstar = (c == '*');
    }
  } else { /* / */
    while ((c = nextChar()) != EOF && c != '\n') /**/;
  }
}

static keyret_e
lexer_next(token_t * type, char * buf, size_t buflen, struct native ** nat)
{
  int c;
  lexer.buf = lexer.tokp = buf;
  lexer.limit = buf + buflen;
  *nat = NULL;
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
  if (isident(c)) {
    Q(gatherWord(c));
    *type = TOK_IDENT;
    if      (EQ("break")) *type = TOK_BREAK;
    else if (EQ("continue")) *type = TOK_CONT;
    else if (EQ("else")) *type = TOK_ELSE;
    else if (EQ("fn")) *type = TOK_FUNC;
    else if (EQ("gfn")) *type = TOK_GFUNC;
    else if (EQ("gvar")) *type = TOK_GVAR;
    else if (EQ("if")) *type = TOK_IF;
    else if (EQ("return")) *type = TOK_RETURN;
    else if (EQ("undef")) *type = TOK_UNDEF;
    else if (EQ("var")) *type = TOK_VAR;
    else if (EQ("while")) *type = TOK_WHILE;
    return KEY_OK;
  }
  if (c == '\'' || c == '\"') {
    Q(gatherString(c));
    *type = TOK_STRING;
    return KEY_OK;
  }
  switch (c) {
    case ',': gatherChar(c); *type = TOK_COMMA; break;
    case '(': gatherChar(c); *type = TOK_OPAREN; break;
    case ')': gatherChar(c); *type = TOK_CPAREN; break;
    case '{': gatherChar(c); *type = TOK_OBRACE; break;
    case '}': gatherChar(c); *type = TOK_CBRACE; break;
    case ';': gatherChar(c); *type = TOK_SEMI; break;
    default: {
      struct native * o;
      Q(gatherOper(c));
      if (EQ("=")) { *type = TOK_ASSIGN; break; }
      o = native_find(buf);
      if (!o) return tokerror("unknown operator '%s'", lexer.buf);
      *type = o->type;
      *nat = o;
      break;
    }
  }
#undef EQ
  return KEY_OK;
}

/* ********************************************************************** */
/* AST Node */
/* ********************************************************************** */

struct parser {
  token_t type;            /* type of look-ahead token */
  char buf[TOKEN_BUFLEN];  /* text of look-ahead token */
  struct native * nat;     /* info of native function / operator token */
  int loopDepth;           /* break/continue only allowed inside while */
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
node_new(token_t t, node * k0, node * k1, node * k2)
{
  node * n = malloc(sizeof(node));
  if (!n) { perror("node_new"); exit(2); }
  n->type = t;
  n->sval = NULL;
  n->first = k0;
  n->second = k1;
  n->third = k2;
  n->chain = parser.chain;
  parser.chain = n;
  return n;
}
#define node_new0(T)          node_new((T), NULL, NULL, NULL)
#define node_new1(T,A)        node_new((T), (A),  NULL, NULL)
#define node_new2(T,A,B)      node_new((T), (A),  (B),  NULL)
#define node_new3(T,A,B,C)    node_new((T), (A),  (B),  (C) )

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

static keyret_e
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

#define NEXT()  lexer_next(&parser.type, parser.buf, TOKEN_BUFLEN, &parser.nat)
#define isType(T)  (parser.type == T)
#define MATCH(T)   bool found_ = false; Q(consume((T), &found_)); if (found_)

static keyret_e
consume(token_t tk, bool * found)
{
  *found = (parser.type == tk);
  if (*found) return NEXT();
  return KEY_OK;
}
static keyret_e
expect(token_t tk)
{
  if (parser.type == tk) return NEXT();
  return tokerror("unexpected '%s', expected '%s'", parser.buf, tok_string(tk));
}
static keyret_e
require(node * n, const char * name)
{
  if (!n) return tokerror("missing %s", name);
  return KEY_OK;
}
static keyret_e
requireIdent(node * n)
{
  if (!n || n->type != TOK_IDENT) return tokerror("missing identifier");
  return KEY_OK;
}

static keyret_e parseExpr(node ** n);
static keyret_e parseAssign(node ** n);
static keyret_e parseStatement(node ** n);
static keyret_e parseStatements(node ** n);
static keyret_e parseTop(node ** n);

static keyret_e newIntern(node ** n, token_t t) {
  const char * p = NULL;
  Q(key_intern(&p, parser.buf));
  *n = node_new_string(t, p);
  return NEXT();
}
static keyret_e newInt(node ** n) {
  int v = 0;
  Q(convert_int(&v, parser.buf));
  *n = node_new_int(v);
  return NEXT();
}

static keyret_e parseIdent(node ** n) {
  if (isType(TOK_IDENT)) return newIntern(n, TOK_IDENT);
  return KEY_OK;
}
static keyret_e parseConst(node ** n) {
  if      (isType(TOK_INT))    { return newInt(n); }
  else if (isType(TOK_STRING)) { return newIntern(n, TOK_STRING); }
  else if (isType(TOK_UNDEF))  { *n = node_new_undef(); return NEXT(); }
  return KEY_OK;
}
static keyret_e parsePrimary(node ** n) {
  if      (isType(TOK_IDENT))  { return newIntern(n, TOK_IDENT); }
  else if (isType(TOK_INT))    { return newInt(n); }
  else if (isType(TOK_STRING)) { return newIntern(n, TOK_STRING); }
  else if (isType(TOK_UNDEF))  { *n = node_new_undef(); return NEXT(); }
  else if (isType(TOK_OPAREN)) {
    Q(NEXT());
    Q(parseExpr(n));
    return expect(TOK_CPAREN);
  }
  return KEY_OK;
}
static keyret_e parseCallArgs(node ** n) {
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
static keyret_e parseCall(node ** n) {
  Q(parsePrimary(n));
  if (*n) {
    MATCH(TOK_OPAREN) {
      struct native * nat;
      node * m = NULL;
      Q(requireIdent(*n));
      nat = native_find((*n)->sval);
      Q(parseCallArgs(&m));
      if (nat) {
        *n = node_new2(TOK_NATIVE, *n, m); /* link ident-node for mem cleanup */
        (*n)->eval = nat->eval;
      } else {
        *n = node_new2(TOK_CALL, *n, m);
      }
      Q(expect(TOK_CPAREN));
    }
  }
  return KEY_OK;
}
static keyret_e parsePrefix(node ** n) {
  if (isType(TOK_PREFIX) || (parser.nat && parser.nat->pre)) {
    struct native * nat = parser.nat;
    Q(NEXT());
    Q(parsePrefix(n)); Q(require(*n, "expression"));
    *n = node_new1(TOK_PREFIX, *n);
    (*n)->eval = nat->eval;
  } else {
    Q(parseCall(n));
  }
  return KEY_OK;
}
static keyret_e parseInfix(node ** n) {
  Q(parsePrefix(n));
  while (*n) {
    if (isType(TOK_INFIX)) {
      struct native * nat = parser.nat;
      node * m = NULL;
      Q(NEXT());
      Q(parsePrefix(&m)); Q(require(m, "expression"));
      *n = node_new2(TOK_INFIX, *n, m);
      (*n)->eval = nat->eval;
      continue;
    }
    break;
  }
  return KEY_OK;
}
static keyret_e parseShortfix(node ** n) {
  Q(parseInfix(n));
  while (*n) {
    if (isType(TOK_SHFIX)) {
      struct native * nat = parser.nat;
      node * m = NULL;
      Q(NEXT());
      Q(parseInfix(&m)); Q(require(m, "expression"));
      *n = node_new2(TOK_SHFIX, *n, m);
      (*n)->eval = nat->eval;
      continue;
    }
    break;
  }
  return KEY_OK;
}
static keyret_e parseAssign(node ** n) {
  Q(parseShortfix(n));
  if (*n) {
    MATCH(TOK_ASSIGN) {
      node * m = NULL;
      Q(requireIdent(*n));
      Q(parseAssign(&m)); Q(require(m, "expression"));
      *n = node_new2(TOK_ASSIGN, *n, m);
      return KEY_OK;
    }
    if (isType(TOK_ASFIX)) {
      struct native * nat = parser.nat;
      node * m = NULL;
      Q(NEXT());
      Q(requireIdent(*n));
      Q(parseAssign(&m)); Q(require(m, "expression"));
      *n = node_new2(TOK_ASFIX, *n, m);
      (*n)->eval = nat->eval;
    }
  }
  return KEY_OK;
}
static keyret_e parseExpr(node ** n) {
  Q(parseAssign(n));
  while (*n) {
    MATCH(TOK_COMMA) {
      node * m = NULL;
      Q(parseAssign(&m)); Q(require(m, "expression"));
      *n = node_new2(TOK_COMMA, *n, m);
      continue;
    }
    break;
  }
  return KEY_OK;
}

static keyret_e parseVarOne(node ** n, bool isconst, bool isglobal) {
  node * name = NULL;
  node * value = NULL;
  Q(parseIdent(&name)); Q(requireIdent(name));
  { MATCH(TOK_ASSIGN) {
    if (isconst) Q(parseConst(&value)); else Q(parseAssign(&value));
    Q(require(value, "expression"));
  } }
  *n = node_new2(isglobal ? TOK_GVAR : TOK_VAR, name, value);
  return KEY_OK;
}
static keyret_e parseVar(node ** n, bool isconst, bool isglobal) {
  Q(parseVarOne(n, isconst, isglobal));
  while (*n) {
    MATCH(TOK_COMMA) {
      node * m = NULL;
      Q(parseVarOne(&m, isconst, isglobal)); Q(require(m, "declaration"));
      *n = node_new2(TOK_LIST, *n, m);
      continue;
    }
    break;
  }
  return expect(TOK_SEMI);
}
static keyret_e parseIf(node ** n) {
  node * cond = NULL;
  node * body = NULL;
  Q(expect(TOK_OPAREN));
  Q(parseExpr(&cond)); Q(require(cond, "condition"));
  Q(expect(TOK_CPAREN));
  Q(parseStatement(&body)); Q(require(body, "statement"));
  *n = node_new2(TOK_IF, cond, body);
  { MATCH(TOK_ELSE) {
    Q(parseStatement(&((*n)->third))); Q(require((*n)->third, "statement"));
  } }
  return KEY_OK;
}
static keyret_e parseFunctionArglist(node ** n) {
  Q(parseIdent(n));
  while (*n) {
    MATCH(TOK_COMMA) {
      node * m = NULL;
      Q(parseIdent(&m)); Q(requireIdent(m));
      *n = node_new2(TOK_ARGLIST, *n, m);
      continue;
    }
    break;
  }
  return KEY_OK;
}
static keyret_e parseFunction(node ** n, bool isglobal) {
  node * name = NULL;
  node * args = NULL;
  node * body = NULL;
  Q(parseIdent(&name)); Q(requireIdent(name));
  Q(expect(TOK_OPAREN));
  Q(parseFunctionArglist(&args));
  Q(expect(TOK_CPAREN));
  Q(parseStatement(&body)); Q(require(body, "statement"));
  *n = node_new3(isglobal ? TOK_GFUNC : TOK_FUNC, name, args, body);
  return KEY_OK;
}
static keyret_e parseWhile(node ** n) {
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
static keyret_e parseBlock(node ** n) {
  Q(parseStatements(n));
  if (!*n) *n = node_new0(TOK_LIST); /* nop; can't return NULL */
  return expect(TOK_CBRACE);
}
static keyret_e parseReturn(node ** n) {
  Q(parseExpr(n));
  *n = node_new1(TOK_RETURN, *n);
  return expect(TOK_SEMI);
}
static keyret_e parseBranch(node ** n, token_t tk) {
  if (parser.loopDepth == 0) return tokerror("invalid branch statement");
  *n = node_new0(tk);
  return expect(TOK_SEMI);
}
static keyret_e parseStatement(node ** n) {
  { MATCH(TOK_BREAK)  { return parseBranch(n, TOK_BREAK); } }
  { MATCH(TOK_CONT)   { return parseBranch(n, TOK_CONT); } }
  { MATCH(TOK_IF)     { return parseIf(n); } }
  { MATCH(TOK_OBRACE) { return parseBlock(n); } }
  { MATCH(TOK_RETURN) { return parseReturn(n); } }
  { MATCH(TOK_VAR)    { return parseVar(n, false, false); } }
  { MATCH(TOK_WHILE)  { return parseWhile(n); } }
  { MATCH(TOK_SEMI)   { *n = node_new0(TOK_LIST); return KEY_OK; } }
  Q(parseExpr(n));
  if (*n) {
    *n = node_new1(TOK_COMMA, *n); /* so we can compile a 'pop' */
    return expect(TOK_SEMI);
  }
  return KEY_OK;
}
static keyret_e parseStatements(node ** n) {
  Q(parseStatement(n));
  while (*n) {
    node * m = NULL;
    Q(parseStatement(&m));
    if (!m) break;
    *n = node_new2(TOK_LIST, *n, m);
  }
  return KEY_OK;
}

static keyret_e parseTopOne(node ** n) {
  { MATCH(TOK_FUNC)  { return parseFunction(n, false); } }
  { MATCH(TOK_GFUNC) { return parseFunction(n, true); } }
  { MATCH(TOK_GVAR)  { return parseVar(n, true, true); } }
  { MATCH(TOK_VAR)   { return parseVar(n, true, false); } }
  return KEY_OK;
}
static keyret_e parseTop(node ** n) {
  Q(parseTopOne(n));
  while (*n) {
    node * m = NULL;
    Q(parseTopOne(&m));
    if (!m) break;
    *n = node_new2(TOK_LIST, *n, m);
  }
  return KEY_OK;
}

static keyret_e
parser_parse(node ** n, const char * src, const char * fname, int line)
{
  keyret_e err;
  lexer_init(src, fname, line);
  parser.loopDepth = 0;
  *n = parser.chain = NULL;
  Q(NEXT()); /* initialize look-ahead */
  err = parseTop(n);
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
  bool counting;
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
#define compQuoted(N) do{ \
  if ((N)->type == TOK_IDENT) compString((N)); else compOne((N)); \
}while(0)

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
    case TOK_ASSIGN: { /* ident expr -> expr str assign */
      compOne(n->second);
      compString(n->first);
      compCode(bcode_assign);
      break;
    }
    case TOK_ASFIX: { /* id expr -> id load expr .eval binary id assign */
      compOne(n->first);
      compOne(n->second);
      compPush(keyval_native(n->eval));
      compCode(bcode_binary);
      compString(n->first);
      compCode(bcode_assign);
      break;
    }
    case TOK_COMMA: { /* expr expr? -> expr pop expr */
      compOne(n->first);
      compCode(bcode_pop);
      compOne(n->second);
      break;
    }
    case TOK_LIST: { /* expr|stmt? expr|stmt? -> expr|stmt expr|stmt */
      compOne(n->first);
      compOne(n->second);
      break;
    }
    case TOK_IF: { /* expr stmt? stmt? -> expr {stmt} {stmt} ifelse */
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
    case TOK_WHILE: { /* expr stmt? -> {expr} {stmt} while */
      procBegin(); compOne(n->first); procEnd();
      procBegin(); compOne(n->second); procEnd();
      compCode(bcode_while);
      break;
    }
    case TOK_BREAK: { compCode(bcode_break); break; }
    case TOK_CONT: { compCode(bcode_continue); break; }
    case TOK_PREFIX: { /* expr -> expr .eval unary */
      compOne(n->first);
      compPush(keyval_native(n->eval));
      compCode(bcode_unary);
      break;
    }
    case TOK_INFIX: { /* expr expr -> expr expr .eval binary */
      compOne(n->first);
      compOne(n->second);
      compPush(keyval_native(n->eval));
      compCode(bcode_binary);
      break;
    }
    case TOK_SHFIX: { /* expr expr -> expr dup .eval unary { pop expr } if */
      compOne(n->first);
      compCode(bcode_dup);
      compPush(keyval_native(n->eval));
      compCode(bcode_unary);
      procBegin(); compCode(bcode_pop); compOne(n->second); procEnd();
      compCode(bcode_if);
      break;
    }
    case TOK_NATIVE: { /* call-list -> call-list N .eval native */
      compOne(n->second);
      compPush(keyval_native(n->eval));
      compPush(keyval_int(compCountList(n->second, TOK_LIST)));
      compCode(bcode_native);
      break;
    }
    case TOK_CALL: { /* name call-list -> call-list N name call */
      compOne(n->second);
      compPush(keyval_int(compCountList(n->second, TOK_LIST)));
      compQuoted(n->first);
      compCode(bcode_call);
      break;
    }
    case TOK_RETURN: { /* expr? -> expr|undef return */
      if (n->first) compOne(n->first); else compPush(keyval_undef());
      compCode(bcode_return);
      break;
    }
    case TOK_GVAR:
    case TOK_VAR: { /* ident expr? -> expr|undef str var */
      if (n->second) { compOne(n->second); } else { compPush(keyval_undef()); }
      compString(n->first);
      compCode((n->type == TOK_VAR) ? bcode_var : bcode_gvar);
      break;
    }
    case TOK_ARGLIST: { /* ident ident -> str str */
      compQuoted(n->first);
      compQuoted(n->second);
      break;
    }
    case TOK_GFUNC:
    case TOK_FUNC: { /* name args body -> proc name var */
      /* proc: arg-names... arg-name-count defargs body */
      procBegin();
      compOne(n->second);
      compPush(keyval_int(compCountList(n->second, TOK_ARGLIST)));
      compCode(bcode_defargs);
      compOne(n->third);
      procEnd();
      compString(n->first);
      compCode((n->type == TOK_FUNC) ? bcode_var : bcode_gvar);
      break;
    }
    default: break; /* noisy compiler */
  }
}

static keyret_e
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
  comp.counting = true;
  compOne(n);
  comp.counting = false;
  comp.wlen += 2;
  comp.work = malloc((size_t)comp.wlen * sizeof(keyval));
  comp.final = malloc((size_t)comp.wlen * sizeof(keyval));
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

static keyval_e custom = KEY_CUSTOM;
keyval_e keyval_custom_handle(void) { return ++custom; }

bool
keyval_tobool(keyval a)
{
  switch (a.type) {
    case KEY_END: return false;
    case KEY_MARK: return false;
    case KEY_UNDEF: return false;
    case KEY_INT: return a.ival != 0;
    case KEY_STR: return a.sval && *a.sval != '\0';
    case KEY_NAT: return true;
    case KEY_FUNC: return true;
    case KEY_BCODE: return true;
    default: return a.pval != NULL;
  }
  return false;
}

bool
keyval_equal(keyval a, keyval b)
{
  if (a.type != b.type) return false;
  switch (a.type) {
    case KEY_END: return true;
    case KEY_MARK: return true;
    case KEY_UNDEF: return true;
    case KEY_INT: return a.ival == b.ival;
    case KEY_STR: return a.sval == b.sval;
    case KEY_NAT: return a.eval == b.eval;
    case KEY_FUNC: return a.fval == b.fval;
    case KEY_BCODE: return a.bval == b.bval;
    default: return a.pval == b.pval;
  }
  return false;
}

/* ********************************************************************** */
/* Stack */
/* ********************************************************************** */

static keyret_e stk_push(stack * s, keyval a) {
  if (s->len >= s->cap) return key_error("stack overflow");
  s->buf[s->len++] = a;
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
static void stk_replace(stack * s, int n, keyval a) {
  s->len -= n; s->buf[s->len++] = a;
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

static keyval globalbuf[GLOBAL_MAX*2];
static stack global = { GLOBAL_MAX*2, 0, globalbuf };

#define OPER   (&e->oper)
#define SCOPE  (&e->scope)
#define EXEC   (&e->exec)
#define GLOBAL (&global)

/* ********************************************************************** */
/* Scope Stack */
/* ********************************************************************** */

static keyret_e scope_push(keyenv * e) {
  Q(stk_push(SCOPE, keyval_mark()));
  return stk_push(SCOPE, keyval_mark());
}
static void scope_pop(keyenv * e) {
  stk_set_length(SCOPE, stk_find_mark(SCOPE));
  stk_pop(SCOPE);
}

static bool
scope_find(const stack * s, const char * name, keyval ** valp) {
  int i;
  for (i = s->len - 2; i >= 0 && s->buf[i].type != KEY_MARK; i -= 2) {
    if (s->buf[i].sval == name) { *valp = &s->buf[i+1]; return true; }
  }
  if (i > 0) { /* if scanning multi-scoped stack, check file scope */
    for (i = 2; i < s->len && s->buf[i].type != KEY_MARK; i += 2) {
      if (s->buf[i].sval == name) { *valp = &s->buf[i+1]; return true; }
    }
  }
  return false;
}
static keyret_e scope_make(stack * s, const char * name, keyval val) {
  keyval * valp;
  if (scope_find(s, name, &valp)) return key_error("'%s' redeclared", name);
  Q(stk_push(s, keyval_str(name)));
  return stk_push(s, val);
}

static keyret_e scope_load(const keyenv * e, const char * name, keyval * valp) {
  keyval * p;
  if (!scope_find(SCOPE, name, &p) && !scope_find(GLOBAL, name, &p))
    return key_error("'%s' not defined", name);
  *valp = *p;
  return KEY_OK;
}
static keyret_e scope_store(keyenv * e, const char * name, keyval val) {
  keyval * p;
  if (!scope_find(SCOPE, name, &p) && !scope_find(GLOBAL, name, &p))
    return key_error("'%s' not defined", name);
  *p = val;
  return KEY_OK;
}
static keyret_e scope_gdef(const char * name, keyval val) {
  return scope_make(GLOBAL, name, val);
}
static keyret_e scope_def(keyenv * e, const char * name, keyval val) {
  return scope_make(SCOPE, name, val);
}

/* ********************************************************************** */
/* Bytecode Function to Execute a Bytecode Array */
/* ********************************************************************** */

static keyret_e
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

static keyret_e
exec_func(keyenv * e, keyval t) /* t.type == KEY_FUNC */
{
  Q(stk_push(EXEC, keyval_int(0)));
  Q(stk_push(EXEC, t));
  return stk_push(EXEC, keyval_bcode(bcode_proc_next));
}

/* ********************************************************************** */
/* Bytecode Functions */
/* ********************************************************************** */

static keyret_e bcode_load(keyenv * e) { /* str -> val */
  keyval a;
  Q(scope_load(e, stk_pop(OPER).sval, &a));
  return stk_push(OPER, a);
}
static keyret_e bcode_assign(keyenv * e) { /* val str -> val */
  const char * name = stk_pop(OPER).sval;
  keyval val = stk_peek(OPER);
  return scope_store(e, name, val);
}
static keyret_e bcode_var(keyenv * e) { /* val|proc str -> - */
  const char * name = stk_pop(OPER).sval;
  keyval val = stk_pop(OPER);
  return scope_def(e, name, val);
}
static keyret_e bcode_gvar(keyenv * e) { /* val|proc str -> - */
  const char * name = stk_pop(OPER).sval;
  keyval val = stk_pop(OPER);
  return scope_gdef(name, val);
}

static keyret_e bcode_pop(keyenv * e) { /* any -> - */
  stk_drop(OPER, 1);
  return KEY_OK;
}
static keyret_e bcode_dup(keyenv * e) { /* any -> any any */
  return stk_push(OPER, stk_peek(OPER));
}

static keyret_e bcode_if(keyenv * e) { /* val proc -> - */
  keyval t = stk_pop(OPER);
  keyval c = stk_pop(OPER);
  if (keyval_tobool(c)) return exec_func(e, t);
  return KEY_OK;
}
static keyret_e bcode_ifelse(keyenv * e) { /* val proc proc -> - */
  keyval f = stk_pop(OPER);
  keyval t = stk_pop(OPER);
  keyval c = stk_pop(OPER);
  if (keyval_tobool(c)) return exec_func(e, t);
  return exec_func(e, f);
}

static keyret_e bcode_native_(keyenv * e, int argc) {
  keyval fn = stk_pop(OPER);
  keyval rv = keyval_undef();
  keyret_e err = fn.eval(e, &rv, argc, stk_ptr(OPER, argc));
  stk_replace(OPER, argc, rv);
  return err;
}
static keyret_e bcode_unary(keyenv * e) { /* val nat -> val */
  return bcode_native_(e, 1);
}
static keyret_e bcode_binary(keyenv * e) { /* val val nat -> val */
  return bcode_native_(e, 2);
}
static keyret_e bcode_native(keyenv * e) { /* val... nat N -> val */
  return bcode_native_(e, stk_pop(OPER).ival);
}

static keyret_e bcode_endcall(keyenv * e) { /* - -> undef */
  /* returning without a 'return' */
  scope_pop(e);
  return stk_push(OPER, keyval_undef());
}
static keyret_e bcode_return(keyenv * e) { /* val -> val */
  /* pop exec stack up to and including endcall */
  int i = stk_find(EXEC, keyval_bcode(bcode_endcall));
  stk_set_length(EXEC, i);
  scope_pop(e);
  return KEY_OK;
}
static keyret_e bcode_call(keyenv * e) { /* vals...N str -> vals...N */
  const char * name = stk_pop(OPER).sval;
  keyval proc;
  Q(scope_load(e, name, &proc));
  if (proc.type != KEY_FUNC) return key_error("'%s' is not a function", name);
  Q(stk_push(EXEC, keyval_bcode(bcode_endcall)));
  Q(scope_push(e));
  return exec_func(e, proc);
}
static keyret_e bcode_defargs(keyenv * e) { /* vals...N names...N -> - */
  int nlen = stk_pop(OPER).ival;
  keyval * names = stk_ptr(OPER, nlen);
  int vlen = stk_get(OPER, nlen + 1).ival;
  keyval * vals = stk_ptr(OPER, nlen + 1 + vlen);
  int i;
  for (i = 0; i < nlen && i < vlen; ++i) {
    Q(scope_def(e, names[i].sval, vals[i]));
  }
  stk_drop(OPER, nlen + 1 + vlen);
  return KEY_OK;
}

static keyret_e bcode_break_mark(keyenv * e) { e = e; return KEY_OK; }
static keyret_e bcode_continue_mark(keyenv * e) { e = e; return KEY_OK; }
static keyret_e bcode_while_cond(keyenv * e);
static keyret_e bcode_break(keyenv * e) {
  stk_set_length(EXEC, stk_find(EXEC, keyval_bcode(bcode_break_mark)));
  return KEY_OK;
}
static keyret_e bcode_continue(keyenv * e) {
  /* pop until the loop, keep the continue-mark, continue with the cond */
  stk_set_length(EXEC, stk_find(EXEC, keyval_bcode(bcode_continue_mark)) + 1);
  return stk_push(EXEC, keyval_bcode(bcode_while_cond));
}
static keyret_e bcode_while_body(keyenv * e) { /* val -> - */
  /* queue cond for exec */
  keyval body = stk_get(EXEC, 3); /* 1: continue, 2:cond, 3:body, 4:break */
  bool pred = keyval_tobool(stk_pop(OPER));
  if (pred) {
    Q(stk_push(EXEC, keyval_bcode(bcode_while_cond)));
    return exec_func(e, body);
  }
  stk_drop(EXEC, 4);
  return KEY_OK;
}
static keyret_e bcode_while_cond(keyenv * e) { /* - -> val */
  /* queue cond for exec */
  keyval cond = stk_get(EXEC, 2); /* 1: continue, 2:cond, 3:body, 4:break */
  Q(stk_push(EXEC, keyval_bcode(bcode_while_body)));
  return exec_func(e, cond);
}
static keyret_e bcode_while(keyenv * e) {
  Q(stk_push(EXEC, keyval_bcode(bcode_break_mark)));
  Q(stk_push(EXEC, stk_pop(OPER))); /* body */
  Q(stk_push(EXEC, stk_pop(OPER))); /* cond */
  Q(stk_push(EXEC, keyval_bcode(bcode_continue_mark)));
  return stk_push(EXEC, keyval_bcode(bcode_while_cond));
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

bool
keyenv_running(const keyenv * e)
{
  return e->exec.len > 0;
}

keyret_e
keyenv_run(keyenv * e)
{
  keyret_e err = KEY_OK;
  while (stk_length(EXEC) && err == KEY_OK) {
    keyval a = stk_pop(EXEC);
    if (a.type == KEY_BCODE) {
      err = a.bval(e);
    } else {
      err = stk_push(OPER, a);
    }
  }
  if (err == KEY_ERROR) keyenv_reset(e);
  return err;
}

void
keyenv_resume(keyenv * e, keyval rv)
{
  stk_replace(OPER, 1, rv);
}

keyret_e
keyenv_push(keyenv * e, const char * name, int argc, const keyval * argv)
{
  int i;
  for (i = 0; i < argc; ++i) Q(stk_push(OPER, argv[i]));
  Q(stk_push(OPER, keyval_int(argc)));
  Q(key_intern(&name, name));
  Q(stk_push(OPER, keyval_str(name)));
  return stk_push(EXEC, keyval_bcode(bcode_call));
}
keyret_e
keyenv_pop(keyenv * e, keyval * rv)
{
  if (stk_length(OPER) == 0) *rv = keyval_undef();
  else *rv = stk_pop(OPER);
  return KEY_OK;
}

keyret_e
keyenv_new(keyenv ** ep, const char * src, const char * fname, int line)
{
  keyenv * e;
  keyret_e err;
  keyval proc;
  size_t bufn = (OPER_DEPTH + SCOPE_DEPTH + EXEC_DEPTH) * sizeof(keyval);
  e = *ep = malloc(sizeof(keyenv) + bufn);
  if (!e) { perror("keyenv_new"); exit(2); }
  memset(e, 0, sizeof(keyenv));
  e->oper.buf = e->buf; e->oper.cap = OPER_DEPTH;
  e->scope.buf = e->oper.buf + OPER_DEPTH; e->scope.cap = SCOPE_DEPTH;
  e->exec.buf = e->scope.buf + SCOPE_DEPTH; e->exec.cap = EXEC_DEPTH;
  Q(compiler_compile(&e->root, &proc, src, fname, line));
  Q(scope_push(e));
  Q(exec_func(e, proc));
  err = keyenv_run(e);
  return err;
}

void
keyenv_delete(keyenv * e)
{
  if (e) {
    if (e->root) {
      free(e->root);
      e->root = NULL;
    }
    free(e);
  }
}

bool
keyenv_has(const keyenv * e, const char * name)
{
  keyval * p;
  Q(key_intern(&name, name));
  return scope_find(SCOPE, name, &p);
}
keyret_e
keyenv_get(const keyenv * e, const char * name, keyval * valp)
{
  Q(key_intern(&name, name));
  return scope_load(e, name, valp);
}
keyret_e
keyenv_set(keyenv * e, const char * name, keyval val)
{
  Q(key_intern(&name, name));
  return scope_store(e, name, val);
}
keyret_e
keyenv_def(keyenv * e, const char * name, keyval val)
{
  Q(key_intern(&name, name));
  return scope_def(e, name, val);
}

/* ********************************************************************** */
/* Global Environment */
/* ********************************************************************** */

bool
key_global_has(const char * name)
{
  keyval * p;
  Q(key_intern(&name, name));
  return scope_find(GLOBAL, name, &p);
}
keyret_e
key_global_get(const char * name, keyval * valp)
{
  keyval * p;
  Q(key_intern(&name, name));
  if (!scope_find(GLOBAL, name, &p)) return key_error("'%s' not defined", name);
  *valp = *p;
  return KEY_OK;
}
keyret_e
key_global_set(const char * name, keyval val)
{
  keyval * p;
  if (!scope_find(GLOBAL, name, &p)) return key_error("'%s' not defined", name);
  *p = val;
  return KEY_OK;
}
keyret_e
key_global_def(const char * name, keyval val)
{
  Q(key_intern(&name, name));
  return scope_gdef(name, val);
}

/* ********************************************************************** */
/* Native Function Registration */
/* ********************************************************************** */

static struct native natives[NATIVEDEF_MAX];
static int natives_cnt = 0;

static keyret_e
native_define(token_t type, bool allowPrefix, const char * name, key_native_fn eval)
{
  /* don't fail on redef, in case user wants to override just a few defaults */
  struct native * o;
  int i;
  Q(key_intern(&name, name));
  for (i = 0; i < natives_cnt; ++i) {
    o = &natives[i];
    if (o->name == name) {
      o->type = type;
      o->eval = eval;
      o->pre = allowPrefix;
      return KEY_OK;
    }
  }
  if (natives_cnt >= NATIVEDEF_MAX) return key_error("too many functions");
  o = &natives[natives_cnt++];
  o->type = type;
  o->name = name;
  o->eval = eval;
  o->pre = allowPrefix;
  return KEY_OK;
}

keyret_e key_native_assign(const char * name, key_native_fn eval) {
  return native_define(TOK_ASFIX, false, name, eval);
}
keyret_e key_native_shortfix(const char * name, key_native_fn eval) {
  return native_define(TOK_SHFIX, false, name, eval);
}
keyret_e key_native_infix(const char * name, bool allowPrefix, key_native_fn eval) {
  return native_define(TOK_INFIX, allowPrefix, name, eval);
}
keyret_e key_native_prefix(const char * name, key_native_fn eval) {
  return native_define(TOK_PREFIX, true, name, eval);
}
keyret_e key_native_func(const char * name, key_native_fn eval) {
  return native_define(TOK_IDENT, false, name, eval);
}

static struct native *
native_find(const char * name)
{
  struct native * o;
  int i;
  for (i = 0; i < natives_cnt; ++i) {
    o = &natives[i];
    if (!strcmp(o->name, name)) return o;
  }
  return NULL;
}

/* ********************************************************************** */
/* Default Operators */
/* ********************************************************************** */

#define UNARY_OPERS \
UNARY_OP(cb_bnot, ~  )

#define BINARY_OPERS \
BINARY_OP(cb_add,  +  ) \
BINARY_OP(cb_mul,  *  ) \
BINARY_OP(cb_div,  /  ) \
BINARY_OP(cb_mod,  %  ) \
BINARY_OP(cb_lsh,  << ) \
BINARY_OP(cb_rsh,  >> ) \
BINARY_OP(cb_band, &  ) \
BINARY_OP(cb_bor,  |  ) \
BINARY_OP(cb_bxor, ^  )

#define EQ_OPERS \
EQ_OP(cb_eq,  == ) \
EQ_OP(cb_ne,  != )

#define CMP_OPERS \
CMP_OP(cb_lt,  <  ) \
CMP_OP(cb_gt,  >  ) \
CMP_OP(cb_le,  <= ) \
CMP_OP(cb_ge,  >= )

#define MINMAX_OPERS \
MINMAX_OP(cb_min, "=<=", <= ) \
MINMAX_OP(cb_max, "=>=", >= ) \

#define SHORT_OPERS \
SHORT_OP(cb_shand, "&&", a ) \
SHORT_OP(cb_shor,  "||", keyval_int(!keyval_tobool(a)) ) \
SHORT_OP(cb_coal,  "??", keyval_int(a.type == KEY_UNDEF) )

#define SHORT_OP(NAME,OP,EXPR) \
static keyret_e \
NAME (keyenv * e, keyval * rv, int argc, const keyval * argv) { \
  keyval a = argv[0]; \
  *rv = EXPR; \
  e = e; argc = argc; return KEY_OK; \
}
SHORT_OPERS
#undef SHORT_OP

#define UNARY_OP(NAME,OP) \
static keyret_e \
NAME (keyenv * e, keyval * rv, int argc, const keyval * argv) { \
  keyval a = argv[0]; \
  if (a.type != KEY_INT) return key_error("type mismatch in '%s'", #OP ); \
  *rv = keyval_int( OP a.ival); \
  e = e; argc = argc; return KEY_OK; \
}
UNARY_OPERS
#undef UNARY_OP

#define BINARY_OP(NAME,OP) \
static keyret_e \
NAME (keyenv * e, keyval * rv, int argc, const keyval * argv) { \
  keyval a = argv[0]; \
  keyval b = argv[1]; \
  if (a.type != KEY_INT || b.type != KEY_INT) \
    return key_error("type mismatch in '%s'", #OP ); \
  *rv = keyval_int(a.ival OP b.ival); \
  e = e; argc = argc; return KEY_OK; \
}
BINARY_OPERS
#undef BINARY_OP

#define EQ_OP(NAME,OP) \
static keyret_e \
NAME (keyenv * e, keyval * rv, int argc, const keyval * argv) { \
  keyval a = argv[0]; \
  keyval b = argv[1]; \
  if (a.type != b.type) return key_error("type mismatch in '%s'", #OP ); \
  *rv = keyval_int(keyval_equal(a, b) OP true); \
  e = e; argc = argc; return KEY_OK; \
}
EQ_OPERS
#undef EQ_OP

#define CMP_OP(NAME,OP) \
static keyret_e \
NAME (keyenv * e, keyval * rv, int argc, const keyval * argv) { \
  keyval a = argv[0]; \
  keyval b = argv[1]; \
  if (a.type != KEY_INT || b.type != KEY_INT) \
    return key_error("type mismatch in '%s'", #OP ); \
  *rv = keyval_int(a.ival OP b.ival); \
  e = e; argc = argc; return KEY_OK; \
}
CMP_OPERS
#undef CMP_OP

#define MINMAX_OP(NAME,OP,EXPR) \
static keyret_e \
NAME (keyenv * e, keyval * rv, int argc, const keyval * argv) { \
  keyval a = argv[0]; \
  keyval b = argv[1]; \
  if (a.type != KEY_INT || b.type != KEY_INT) \
    return key_error("type mismatch in '%s'", OP ); \
  *rv = a.ival EXPR b.ival ? a : b; \
  e = e; argc = argc; return KEY_OK; \
}
MINMAX_OPERS
#undef MINMAX_OP

static keyret_e
cb_not(keyenv * e, keyval * rv, int argc, const keyval * argv) {
  keyval a = argv[0];
  *rv = keyval_int(!keyval_tobool(a));
  e = e; argc = argc; return KEY_OK;
}
static keyret_e
cb_subneg(keyenv * e, keyval * rv, int argc, const keyval * argv) {
  if (argc == 1) {
    keyval a = argv[0];
    if (a.type != KEY_INT) return key_error("type mismatch in '%s'", "-");
    *rv = keyval_int(-a.ival);
  } else {
    keyval a = argv[0];
    keyval b = argv[1];
    if (a.type != KEY_INT || b.type != KEY_INT)
      return key_error("type mismatch in '%s'", "-");
    *rv = keyval_int(a.ival - b.ival);
  }
  e = e; argc = argc; return KEY_OK;
}
static keyret_e
cb_isdef(keyenv * e, keyval * rv, int argc, const keyval * argv) {
  int i;
  *rv = keyval_int(1);
  for (i = 0; i < argc; ++i) {
    if (argv[i].type == KEY_UNDEF) { *rv = keyval_int(0); break; }
  }
  e = e; argc = argc; return KEY_OK;
}
static keyret_e
cb_debug(keyenv * e, keyval * rv, int argc, const keyval * argv) {
  int i;
  for (i = 0; i < argc; ++i) {
    keyval a = argv[i];
    if (a.type == KEY_INT) printf("%d", a.ival);
    else if (a.type == KEY_STR) printf("%s", a.sval);
    else if (a.type == KEY_UNDEF) printf("<undef>");
    else printf("<%d:%p>", a.type, a.pval);
  }
  printf("\n");
  e = e; argc = argc; rv = rv; return KEY_OK;
}
static keyret_e
cb_fail(keyenv * e, keyval * rv, int argc, const keyval * argv) {
  char * dst = key_errmsg;
  char * endp = ERRMSG_LIMIT;
  int i;
  for (i = 0; i < argc; ++i) {
    keyval a = argv[i];
    if (a.type == KEY_INT)
      dst += snprintf(dst, (size_t)(endp - dst), "%d", a.ival);
    else if (a.type == KEY_STR)
      dst += snprintf(dst, (size_t)(endp - dst), "%s", a.sval);
    else if (a.type == KEY_UNDEF)
      dst += snprintf(dst, (size_t)(endp - dst), "<undef>");
    else
      dst += snprintf(dst, (size_t)(endp - dst), "<%d:%p>", a.type, a.pval);
  }
  *(endp - 1) = '\0';
  keyenv_reset(e);
  argc = argc; rv = rv;
  return KEY_ERROR;
}

keyret_e
key_native_defaults(void)
{
  static bool inited = false;
  if (inited) return KEY_OK;
#define MINMAX_OP(NAME,OP,EXPR) Q(key_native_infix(OP, false, NAME));
#define SHORT_OP(NAME,OP,EXPR)  Q(key_native_shortfix(OP, NAME));
#define UNARY_OP(NAME,OP)       Q(key_native_prefix(#OP, NAME));
#define BINARY_OP(NAME,OP)      Q(key_native_infix(#OP, false, NAME));
#define CMP_OP(NAME,OP)         Q(key_native_infix(#OP, false, NAME));
#define EQ_OP(NAME,OP)          Q(key_native_infix(#OP, false, NAME));
MINMAX_OPERS
SHORT_OPERS
UNARY_OPERS
BINARY_OPERS
EQ_OPERS
CMP_OPERS
#undef MINMAX_OP
#undef SHORT_OP
#undef BINARY_OP
#undef UNARY_OP
#undef EQ_OP
#undef CMP_OP

#define BINARY_OP(NAME,OP) Q(key_native_assign(#OP "=", NAME));
BINARY_OPERS
#undef BINARY_OP
  Q(key_native_prefix("!", cb_not));
  Q(key_native_infix("-", true, cb_subneg));
  Q(key_native_func("isdef", cb_isdef));
  Q(key_native_func("debug", cb_debug));
  Q(key_native_func("fail", cb_fail));
  inited = true;
  return KEY_OK;
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
    case KEY_UNDEF: printf("null"); break;
    case KEY_INT:   printf("(%d)", a.ival); break;
    case KEY_STR: {
      char ebuf[512];
      key_escape(ebuf, 512, a.sval);
      printf("\"%s\"", ebuf);
      break;
    }
    case KEY_FUNC:  printf("{ "); print_proc(a.fval); printf("}"); break;
    case KEY_NAT:   printf("<native>"); break;
    case KEY_BCODE: {
#define BFUNC(P)  if (a.bval == bcode_##P) { printf("%s", #P); break; }
      BFUNC(load); BFUNC(assign); BFUNC(pop); BFUNC(if); BFUNC(ifelse);
      BFUNC(while); BFUNC(break); BFUNC(continue);
      BFUNC(unary); BFUNC(binary); BFUNC(native); BFUNC(call); BFUNC(return);
      BFUNC(var); BFUNC(gvar); BFUNC(defargs);
      printf("bcode:%p", a.pval); /* can't cast ptr-to-func, so we cheat */
      break;
#undef BFUNC
    }
    default: {
      printf("custom:%d:%p", a.type, a.pval);
      break;
    }
  }
}
static void
print_proc(keyval * proc)
{
  int i;
  if (!proc) { printf("<<null proc>>"); return; }
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

static keyret_e
runall(keyenv * e)
{
  int i = 0;
  keyret_e err;
  while ((err = keyenv_run(e)) == KEY_PAUSE) {
    keyenv_resume(e, keyval_int(i++));
  }
  return err;
}

static keyret_e
run(const char * src, const char * fname)
{
  keyret_e err;
  keyval a;
  keyenv * e;
  Q(key_native_defaults());
  Q(keyenv_new(&e, src, fname, 1));
  Q(keyenv_push(e, "main", 0, NULL));
  if ((err = runall(e)) == KEY_OK) {
    Q(keyenv_pop(e, &a));
    print_one(a);
    printf("\n");
  }
  keyenv_delete(e);
  return err;
}

static keyret_e
cb_pause(keyenv * e, keyval * rv, int argc, const keyval * argv)
{
  e = e; rv = rv; argc = argc; argv = argv;
  return KEY_PAUSE;
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
  Q(key_native_func("pause", cb_pause));
  {
    keyret_e rv = run(scriptbuf, fname);
    if (rv != KEY_OK) printf("error: %s\n", key_errmsg);
  }
  return 0;
}

#endif /* TEST */

/* ********************************************************************** */
/* ********************************************************************** */
