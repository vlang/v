#include <stdint.h>

void* c_mut_voidptr_id(void* p) {
  return p;
}

void c_mut_voidptr_clear(void** pp) {
  *pp = 0;
}
