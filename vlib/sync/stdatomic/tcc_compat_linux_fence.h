/* Use V's private x86_64 fence; libtcc1.a may export either standard name. */
#undef atomic_thread_fence
#undef __atomic_thread_fence
#if defined(__x86_64__)
extern void _V_atomic_thread_fence(int order);
#define atomic_thread_fence(order) _V_atomic_thread_fence(order)
#define __atomic_thread_fence(order) _V_atomic_thread_fence(order)
#else
#define atomic_thread_fence(order) __atomic_thread_fence(order)
#endif
