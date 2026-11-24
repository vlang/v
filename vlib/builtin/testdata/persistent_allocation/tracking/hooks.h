#include <stdint.h>
void vheap_alloc(void *pointer, uint64_t size);
void vheap_free(void *pointer);
int heap_hooks_balanced(void);
