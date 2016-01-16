/* ********************************************************************** */
/* KeyLang: A Scripting Language in the Key of C */
/* ********************************************************************** */

#ifndef KEYLANG_H
#define KEYLANG_H 1

#include <stdbool.h>
#include <stddef.h>

#ifdef __GNUC__
#define KEY_FORMAT(n,m)  __attribute__(( __format__(__printf__, (n), (m)) )) \
                         __attribute__(( __nonnull__ (n) ))
#else
#define KEY_FORMAT(n,m)  /**/
#endif

typedef enum keyret_e {
  KEY_OK = 0, /* everything's fine; keep going */
  KEY_PAUSE,  /* script execution will be resumed later */
  KEY_ERROR   /* there was an error; check key_errmsg */
} keyret_e;

extern char key_errmsg[];

extern keyret_e key_error(const char * fmt, ...) KEY_FORMAT(1, 2);
extern keyret_e key_intern(const char ** dst, const char * src);
extern size_t key_escape(char * dst, size_t dstlen, const char * src);

typedef struct keyval keyval;
typedef struct keyenv keyenv;

typedef keyret_e (*key_native_fn)(keyenv * e, keyval * rv, int argc, const keyval * argv);
typedef keyret_e (*key_bcode_fn)(keyenv * e); /* private */

typedef enum keyval_e {
  KEY_END = 0,  /* end of bytecode array; must equal 0 (private) */
  KEY_UNDEF,    /* undefined */
  KEY_INT,      /* integers */
  KEY_STR,      /* string literals and identifiers */
  KEY_MARK,     /* for building arrays and stuff (private) */
  KEY_BCODE,    /* bytecode C functions (private) */
  KEY_NAT,      /* registered C functions (private) */
  KEY_FUNC,     /* bytecode arrays (private) */
  KEY_CUSTOM    /* start of custom types (private) */
} keyval_e;

struct keyval {
  keyval_e type;
  union {
    int ival;            /* KEY_INT */
    const char * sval;   /* KEY_STR */
    key_bcode_fn bval;   /* KEY_BCODE (private) */
    key_native_fn eval;  /* KEY_CALL (private) */
    keyval * fval;       /* KEY_FUNC (private) */
    void * pval;         /* KEY_CUSTOM */
  };
};
#define keyval_undef()     ((keyval){ .type=(KEY_UNDEF), .pval=(NULL) })
#define keyval_int(V)      ((keyval){ .type=(KEY_INT),   .ival=(V)    })
#define keyval_str(V)      ((keyval){ .type=(KEY_STR),   .sval=(V)    })
#define keyval_custom(T,V) ((keyval){ .type=(T),         .pval=(V)    })

extern bool keyval_tobool(keyval a);
extern bool keyval_equal(keyval a, keyval b);
extern keyval_e keyval_custom_handle(void);

extern keyret_e key_native_assign(const char * name, key_native_fn eval);
extern keyret_e key_native_shortfix(const char * name, key_native_fn eval);
extern keyret_e key_native_infix(const char * name, bool allowPrefix, key_native_fn eval);
extern keyret_e key_native_prefix(const char * name, key_native_fn eval);
extern keyret_e key_native_func(const char * name, key_native_fn eval);
extern keyret_e key_native_defaults(void);

extern keyret_e keyenv_new(keyenv ** ep, const char * src, const char * fname, int line);
extern void keyenv_delete(keyenv * e);
extern void keyenv_reset(keyenv * e);
extern keyret_e keyenv_run(keyenv * e);
extern void keyenv_resume(keyenv * e, keyval rv);
extern bool keyenv_running(const keyenv * e);
extern keyret_e keyenv_push(keyenv * e, const char * name, int argc, const keyval * argv);
extern keyret_e keyenv_pop(keyenv * e, keyval * rv);

extern bool keyenv_has(const keyenv * e, const char * name);
extern keyret_e keyenv_get(const keyenv * e, const char * name, keyval * valp);
extern keyret_e keyenv_set(keyenv * e, const char * name, keyval val);
extern keyret_e keyenv_def(keyenv * e, const char * name, keyval val);

extern bool key_global_has(const char * name);
extern keyret_e key_global_get(const char * name, keyval * valp);
extern keyret_e key_global_set(const char * name, keyval val);
extern keyret_e key_global_def(const char * name, keyval val);

/* ********************************************************************** */
/* ********************************************************************** */

#endif /* KEYLANG_H */
