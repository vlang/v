#include "heap.h"
#include <stdlib.h>
#if defined(__GLIBC__)
#include <malloc.h>
#endif
size_t syscalls_heap_live(void) {
#if defined(__GLIBC__)
#if __GLIBC_PREREQ(2, 33)
    return mallinfo2().uordblks;
#endif
#endif
    return 0;
}
void *syscalls_c_reallocate(void *pointer, size_t size) { return realloc(pointer, size); }
void syscalls_c_free(void *pointer) { free(pointer); }
