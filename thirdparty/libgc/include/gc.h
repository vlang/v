/* This file is installed for backward compatibility. */
#include "gc/gc.h"

__attribute__ ((weak)) GC_API void GC_CALL GC_noop1_ptr(volatile void *p) {
   GC_noop1((u64)p);
}
