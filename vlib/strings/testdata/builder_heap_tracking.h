#ifndef BUILDER_HEAP_TRACKING_H
#define BUILDER_HEAP_TRACKING_H

#ifndef CUSTOM_DEFINE_track_heap
#error "Builder allocation tests require -d track_heap"
#endif

// Only count the measured operation; the hooks themselves must not allocate.
static int builder_heap_tracking;
static int builder_heap_allocations;
static int builder_heap_frees;

void vheap_alloc(void *pointer, unsigned long long size) {
    (void)size;
    if (builder_heap_tracking && pointer) builder_heap_allocations++;
}

void vheap_free(void *pointer) {
    if (builder_heap_tracking && pointer) builder_heap_frees++;
}

static void builder_heap_start(void) {
    builder_heap_allocations = 0;
    builder_heap_frees = 0;
    builder_heap_tracking = 1;
}

static int builder_heap_stop(void) {
    builder_heap_tracking = 0;
    return builder_heap_allocations - builder_heap_frees;
}

static int builder_heap_allocation_count(void) {
    return builder_heap_allocations;
}

#endif
