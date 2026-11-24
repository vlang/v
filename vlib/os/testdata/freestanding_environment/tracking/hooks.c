#include <stdint.h>
#include "hooks.h"

static int active, allocations, releases;
void vheap_alloc(void *pointer, uint64_t size) {
    (void)size;
    if (active && pointer) ++allocations;
}
void vheap_free(void *pointer) {
    if (active && pointer) ++releases;
}
void environment_tracking_begin(void) { allocations = releases = 0; active = 1; }
void environment_tracking_end(void) { active = 0; }
int environment_tracking_allocations(void) { return allocations; }
int environment_tracking_frees(void) { return releases; }
