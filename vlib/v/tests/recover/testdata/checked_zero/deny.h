#include <stdlib.h>
static int refuse_zero_storage;
static void set_refuse_zero_storage(int enabled) { refuse_zero_storage = enabled; }

static void *checked_calloc(size_t count, size_t size) {
	if (refuse_zero_storage && count == 1 && size == 513) return NULL;
	return calloc(count, size);
}
#define calloc(count, size) checked_calloc(count, size)
#ifdef _WIN32
#include <malloc.h>
static void *checked_aligned_malloc(size_t size, size_t alignment) {
	if (refuse_zero_storage && size == 513) return NULL;
	return _aligned_malloc(size, alignment);
}
#define _aligned_malloc(size, alignment) checked_aligned_malloc(size, alignment)
#endif
#if defined(_VGCBOEHM) || defined(CUSTOM_DEFINE_gcboehm)
#include <gc.h>
static void *checked_gc_malloc(size_t size) {
	if (refuse_zero_storage && size == 513) return NULL;
	return GC_MALLOC(size);
}
static void *checked_gc_malloc_atomic(size_t size) {
	if (refuse_zero_storage && size == 513) return NULL;
	return GC_MALLOC_ATOMIC(size);
}
#undef GC_MALLOC
#define GC_MALLOC(size) checked_gc_malloc(size)
#undef GC_MALLOC_ATOMIC
#define GC_MALLOC_ATOMIC(size) checked_gc_malloc_atomic(size)
#endif
