#ifndef V_LINUX_BARE_LIBC_ALIGNED_ALLOC_H
#define V_LINUX_BARE_LIBC_ALIGNED_ALLOC_H
#include <stddef.h>
void *v_freestanding_aligned_alloc(size_t alignment, size_t size);
#endif
