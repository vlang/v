#ifndef V_CLOSURE_ONCE_NIX_H
#define V_CLOSURE_ONCE_NIX_H

#include <pthread.h>

typedef void (*v_closure_init_fn)(void);

#ifndef V_CLOSURE_STATIC_INLINE
# ifdef _MSC_VER
#  define V_CLOSURE_STATIC_INLINE static __inline
# else
#  define V_CLOSURE_STATIC_INLINE static inline
# endif
#endif

static pthread_mutex_t v_closure_once_mutex = PTHREAD_MUTEX_INITIALIZER;
static int v_closure_once_done = 0;

V_CLOSURE_STATIC_INLINE void v_closure_init_once(v_closure_init_fn init_fn) {
	pthread_mutex_lock(&v_closure_once_mutex);
	if (!v_closure_once_done) {
		init_fn();
		v_closure_once_done = 1;
	}
	pthread_mutex_unlock(&v_closure_once_mutex);
}

#endif
