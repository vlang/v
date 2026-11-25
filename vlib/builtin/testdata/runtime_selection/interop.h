#ifndef V_RUNTIME_SELECTION_INTEROP_H
#define V_RUNTIME_SELECTION_INTEROP_H
#include <stdint.h>
void *runtime_c_allocate(void);
void *runtime_c_resize(void *pointer, uintptr_t size);
void runtime_c_release(void *pointer);
uintptr_t runtime_c_errno_address(void);
int runtime_c_interop(void);
void runtime_c_buffered_output(void);
#endif
