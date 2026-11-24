#include <stdint.h>
static uintptr_t tracked[2];
static int allocations[2], releases[2];
void vheap_alloc(void *pointer, uint64_t size) {
 if (size == 4093 || size == 4094) {
  unsigned index = (unsigned)(size - 4093);
  tracked[index] = (uintptr_t)pointer;
  allocations[index]++;
 }
}
void vheap_free(void *pointer) {
 for (unsigned i = 0; i < 2; i++)
  if (tracked[i] && tracked[i] == (uintptr_t)pointer) releases[i]++;
}
int heap_hooks_balanced(void) {
 return allocations[0] == 1 && allocations[1] == 1 && releases[0] == 1 && releases[1] == 1;
}
