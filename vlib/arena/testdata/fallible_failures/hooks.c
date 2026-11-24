#include <stdint.h>

static int null_allocations;

void vheap_alloc(void *pointer, uint64_t size) {
    (void)size;
    if (!pointer) null_allocations++;
}

void vheap_free(void *pointer) {
    (void)pointer;
}

int allocation_failure_hooks_clean(void) {
    return null_allocations == 0;
}
