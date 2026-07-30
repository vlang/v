#ifndef V_TCC_STDATOMIC_FREEBSD_AMD64_FENCE
#define V_TCC_STDATOMIC_FREEBSD_AMD64_FENCE

/*
 * New TCC runtimes export atomic_thread_fence, while older ones export
 * __atomic_thread_fence. The older strong definition overrides this fallback.
 */
#undef __atomic_thread_fence
__attribute__((weak)) void __atomic_thread_fence(memory_order order)
{
	(void)order;
	__asm__ __volatile__("lock orq $0,(%%rsp)" ::: "memory");
}

#endif
