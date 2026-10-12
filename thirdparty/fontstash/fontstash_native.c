#include <stdlib.h>
// Keep the native allocator paired with V's font-data ownership transfer.
#ifdef V_FONTSTASH_GC
#include <stddef.h>
extern void* GC_malloc(size_t);
extern void* GC_malloc_atomic(size_t);
extern void* GC_realloc(void*, size_t);
extern void GC_free(void*);
#define FONTSTASH_MALLOC GC_malloc
#define FONTSTASH_MALLOC_ATOMIC GC_malloc_atomic
#define FONTSTASH_REALLOC GC_realloc
#define FONTSTASH_FREE GC_free
#endif
#define FONTSTASH_IMPLEMENTATION
#include "fontstash.h"
