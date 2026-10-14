#ifndef JSON5_HEAP_TRACKING_H
#define JSON5_HEAP_TRACKING_H

#include <stdlib.h>

#ifndef CUSTOM_DEFINE_track_heap
#error "JSON5 allocation tests require -d track_heap"
#endif

// Hooks must not allocate, since doing so would recursively call themselves.
static struct {
    void *pointer;
    unsigned long long size;
} json5_heap_blocks[4096];
static unsigned long long json5_heap_bytes;

void vheap_alloc(void *pointer, unsigned long long size) {
    if (!pointer) return;
    for (int i = 0; i < 4096; i++) {
        if (!json5_heap_blocks[i].pointer) {
            json5_heap_blocks[i].pointer = pointer;
            json5_heap_blocks[i].size = size;
            json5_heap_bytes += size;
            return;
        }
    }
    abort();
}

void vheap_free(void *pointer) {
    for (int i = 0; i < 4096; i++) {
        if (pointer && json5_heap_blocks[i].pointer == pointer) {
            json5_heap_bytes -= json5_heap_blocks[i].size;
            json5_heap_blocks[i].pointer = NULL;
            return;
        }
    }
}

static unsigned long long json5_heap_live_bytes(void) {
    return json5_heap_bytes;
}

#endif
