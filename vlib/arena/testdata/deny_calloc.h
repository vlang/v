#include <stdlib.h>

static int arena_deny_next_calloc;
static void arena_deny_calloc_once(void) { arena_deny_next_calloc = 1; }

static void *arena_test_calloc(size_t count, size_t size) {
	if (arena_deny_next_calloc) {
		arena_deny_next_calloc = 0;
		return NULL;
	}
	return calloc(count, size);
}

#define calloc(count, size) arena_test_calloc(count, size)
