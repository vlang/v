#include <stdlib.h>
static int arena_chunk_malloc_count;
static void fail_arena_malloc_at(int request) { arena_chunk_malloc_count = request; }
static void *arena_chunk_test_malloc(size_t size) {
	if (arena_chunk_malloc_count && --arena_chunk_malloc_count == 0) return NULL;
	return malloc(size);
}
#define malloc(size) arena_chunk_test_malloc(size)
