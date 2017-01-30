/* ********************************************************************** */
/* KeyLang: A Scripting Language in the Key of C */
/* ********************************************************************** */
#ifndef KEYLANG_H
#define KEYLANG_H 1

#include <stddef.h>

#ifdef __GNUC__
#define KEY_FORMAT(n,m)  __attribute__(( __format__(__printf__, (n), (m)) )) \
                         __attribute__(( __nonnull__(n) ))
#else
#define KEY_FORMAT(n,m)  /**/
#endif

typedef enum keyret {
  KEY_OK = 0,  /* everything's fine; keep going */
  KEY_PAUSE,   /* script execution will be resumed later */
  KEY_ERROR    /* there was an error; check key_errmsg */
} keyret;

extern char key_errmsg[];

typedef struct keyval keyval;
typedef struct keyenv keyenv;

typedef keyret (*key_native_fn)(keyenv * e, keyval * rv, int argc, const keyval * argv);
typedef keyret (*key_bcode_fn)(keyenv * e); /* private */

typedef enum keytype {
  KEY_END = 0,           /* end of bytecode array; must equal 0 (private) */
  KEY_UNDEF,             /* undefined: value has no value */
  KEY_INT,               /* integers */
  KEY_STR,               /* string literals and identifiers */
  KEY_MARK,              /* for building arrays and stuff (private) */
  KEY_BCODE,             /* bytecode C functions (private) */
  KEY_NAT,               /* registered C functions (private) */
  KEY_FUNC,              /* arrays of keyval as bytecode (private) */
  KEY_CUSTOM,            /* start of custom types (private) */
  KEY_CUSTOM_MAX_ = 64   /* maximum number of types (private) */
} keytype;

struct keyval {
  keytype type;
  union {
    int ival;            /* KEY_INT */
    const char * sval;   /* KEY_STR */
    key_bcode_fn bval;   /* KEY_BCODE (private) */
    key_native_fn eval;  /* KEY_NAT (private) */
    keyval * fval;       /* KEY_FUNC (private) */
    void * pval;         /* KEY_CUSTOM */
  };
};
#define keyval_undef()      ((keyval){ .type=(KEY_UNDEF), .pval=(NULL) })
#define keyval_int(V)       ((keyval){ .type=(KEY_INT),   .ival=(V)    })
#define keyval_str(V)       ((keyval){ .type=(KEY_STR),   .sval=(V)    })
#define keyval_custom(T,V)  ((keyval){ .type=(T),         .pval=(V)    })

extern keytype keyval_custom_type(const char * name);
extern const char * keyval_typename(keytype a);
extern int keyval_tobool(keyval a);
extern int keyval_equal(keyval a, keyval b);

extern keyret key_error(const char * fmt, ...) KEY_FORMAT(1, 2);
extern keyret key_type_error(keyval a);
extern keyret key_type_require(keyval a, keytype type);
extern keyret key_type_require_fn(keyval a);
extern void key_print_stats(void);

extern keyret key_intern(const char ** dst, const char * src);
extern size_t key_escape(char * dst, size_t dstlen, const char * src);

extern keyret keyenv_new(keyenv ** ep, const char * src, const char * fname, int line);
extern void keyenv_delete(keyenv * e);
extern void keyenv_reset(keyenv * e);
extern keyret keyenv_run(keyenv * e);
extern void keyenv_resume(keyenv * e, keyval rv);
extern int keyenv_running(const keyenv * e);
extern keyret keyenv_pop(keyenv * e, keyval * rv);
extern keyret keyenv_push(keyenv * e, const char * name, int argc, const keyval * argv);
extern keyret keyenv_interrupt(keyenv * e, const char * name, int argc, const keyval * argv);
extern keyret keyenv_callback(keyenv * e, keyval * rv, keyval func, int argc, const keyval * argv);
extern size_t keyenv_stack_trace(const keyenv * e, char * buf, size_t buflen);

extern int keyenv_has(const keyenv * e, const char * name);
extern keyret keyenv_get(const keyenv * e, const char * name, keyval * valp);
extern keyret keyenv_set(keyenv * e, const char * name, keyval val);
extern keyret keyenv_def(keyenv * e, const char * name, keyval val);

extern int key_global_has(const char * name);
extern keyret key_global_get(const char * name, keyval * valp);
extern keyret key_global_set(const char * name, keyval val);
extern keyret key_global_def(const char * name, keyval val);
extern keyret key_global_fndef(const char * name, key_native_fn eval);

extern keyret key_default_fail(keyenv * e, keyval * rv, int argc, const keyval * argv);
extern keyret key_default_debug(keyenv * e, keyval * rv, int argc, const keyval * argv);
extern keyret key_default_trace(keyenv * e, keyval * rv, int argc, const keyval * argv);
extern keyret key_default_pause(keyenv * e, keyval * rv, int argc, const keyval * argv);
extern keyret key_global_defaults(void);

#endif /* KEYLANG_H */
/* ********************************************************************** */
/* ********************************************************************** */
