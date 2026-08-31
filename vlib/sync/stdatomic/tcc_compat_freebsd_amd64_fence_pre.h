#ifndef V_TCC_STDATOMIC_FREEBSD_AMD64_FENCE_PRE
#define V_TCC_STDATOMIC_FREEBSD_AMD64_FENCE_PRE

/*
 * FreeBSD's stdatomic.h calls this runtime helper. Keep the declaration
 * unprototyped until memory_order is available from that header.
 */
void __atomic_thread_fence();

#endif
