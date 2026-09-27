#ifndef V_CLOSURE_ONCE_WINDOWS_H
#define V_CLOSURE_ONCE_WINDOWS_H

#include <windows.h>

typedef void (*v_closure_init_fn)(void);

#ifndef V_CLOSURE_STATIC_INLINE
# ifdef _MSC_VER
#  define V_CLOSURE_STATIC_INLINE static __inline
# else
#  define V_CLOSURE_STATIC_INLINE static inline
# endif
#endif

/* The parallel-C owner provides the single shared copy used by inline
 * helpers in every generated translation unit. */
#define V_PARALLEL_CC_STATIC_STORAGE_HANDLED 1
#if defined(V_PARALLEL_CC)
# if defined(V_PARALLEL_CC_OUT_0)
SRWLOCK v_closure_once_lock = SRWLOCK_INIT;
int v_closure_once_done = 0;
# else
extern SRWLOCK v_closure_once_lock;
extern int v_closure_once_done;
# endif
#else
static SRWLOCK v_closure_once_lock = SRWLOCK_INIT;
static int v_closure_once_done = 0;
#endif

V_CLOSURE_STATIC_INLINE void v_closure_init_once(v_closure_init_fn init_fn) {
	AcquireSRWLockExclusive(&v_closure_once_lock);
	if (!v_closure_once_done) {
		init_fn();
		v_closure_once_done = 1;
	}
	ReleaseSRWLockExclusive(&v_closure_once_lock);
}

#endif
