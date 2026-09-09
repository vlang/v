#ifndef V_TCC_STDATOMIC_COMPAT_RESTORED
#define V_TCC_STDATOMIC_COMPAT_RESTORED

/*
 * The cleanup header above hides V's compatibility helpers again, so the
 * standard API has to come back for user and third party headers.
 *
 * TCC ships its own self contained <stdatomic.h> since 0.9.28, so prefer that
 * one when it is there. Older TCCs have no such header and would silently pick
 * up GCC's, which is written against __ATOMIC_* builtins that TCC does not
 * provide - either the include is not found at all, or it fails later with
 * '__ATOMIC_RELAXED' undeclared. In that case restore the few names V itself
 * relies on from the bundled compat header instead.
 */
#if defined(__has_include)
#if __has_include(<stdatomic.h>)
#define V_TCC_STDATOMIC_SYSTEM_HEADER 1
#endif
#endif

#ifdef V_TCC_STDATOMIC_SYSTEM_HEADER

#include <stdatomic.h>

#else

#define _Atomic volatile

typedef int memory_order;

#define memory_order_relaxed 0
#define memory_order_consume 1
#define memory_order_acquire 2
#define memory_order_release 3
#define memory_order_acq_rel 4
#define memory_order_seq_cst 5

/* The alias pass renamed the fence declarations of the compat header, so the
 * plain names have to be reintroduced. TinyCC relies on its runtime atomics
 * support here, same as the compat header does. */
extern void __atomic_thread_fence(memory_order order);

#if defined(__APPLE__)
extern void atomic_thread_fence(memory_order order);
#else
#define atomic_thread_fence(order) __atomic_thread_fence(order)
#endif

#endif

#endif
