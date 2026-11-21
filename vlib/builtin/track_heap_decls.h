#ifndef V_TRACK_HEAP_DECLS_H
#define V_TRACK_HEAP_DECLS_H

#include <stdint.h>

// Prototypes for the optional `-d track_heap` profiler hooks. Implementations
// are supplied by the user (or by a test stub); V only emits the calls.
// The size argument is uint64_t so the declaration matches the generated
// hook definition on both LP64 and LLP64 targets.
void vheap_alloc(void *p, uint64_t n);
void vheap_free(void *p);

#endif
