#ifndef V_TRACK_HEAP_DECLS_H
#define V_TRACK_HEAP_DECLS_H

// Prototypes for the optional `-d track_heap` profiler hooks. Implementations
// are supplied by the user (or by a test stub); V only emits the calls.
void vheap_alloc(void *p, unsigned long long n);
void vheap_free(void *p);

#endif
