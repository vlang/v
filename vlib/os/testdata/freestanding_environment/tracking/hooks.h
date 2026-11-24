#ifndef V_ENVIRONMENT_TRACKING_H
#define V_ENVIRONMENT_TRACKING_H
#include <stdint.h>

void vheap_alloc(void *pointer, uint64_t size);
void vheap_free(void *pointer);
void environment_tracking_begin(void);
void environment_tracking_end(void);
int environment_tracking_allocations(void);
int environment_tracking_frees(void);
#endif
