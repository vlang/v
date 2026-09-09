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

static SRWLOCK v_closure_once_lock;
static int v_closure_once_done = 0;

V_CLOSURE_STATIC_INLINE void v_closure_init_once(v_closure_init_fn init_fn) {
	AcquireSRWLockExclusive(&v_closure_once_lock);
	if (!v_closure_once_done) {
		init_fn();
		v_closure_once_done = 1;
	}
	ReleaseSRWLockExclusive(&v_closure_once_lock);
}

#endif
