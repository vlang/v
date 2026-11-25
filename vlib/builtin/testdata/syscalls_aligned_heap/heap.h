#include <stddef.h>
size_t syscalls_heap_live(void);
void *syscalls_c_reallocate(void *, size_t);
void syscalls_c_free(void *);
