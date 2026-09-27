/* TCC's Linux runtime exports __atomic_thread_fence, while its standard
 * header maps that symbol in the opposite direction. */
#undef atomic_thread_fence
#undef __atomic_thread_fence
#define atomic_thread_fence(order) __atomic_thread_fence(order)
