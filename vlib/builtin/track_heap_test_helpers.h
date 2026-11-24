#ifndef V_PREALLOC_TRACK_HEAP_TEST_HELPERS_H
#define V_PREALLOC_TRACK_HEAP_TEST_HELPERS_H

#include <stdint.h>
#include "track_heap_decls.h"

static int v_prealloc_test_heap_allocs = 0;
static int v_prealloc_test_heap_frees = 0;

void vheap_alloc(void *p, uint64_t n) {
	(void)p;
	(void)n;
	v_prealloc_test_heap_allocs++;
}

void vheap_free(void *p) {
	(void)p;
	v_prealloc_test_heap_frees++;
}

int v_prealloc_test_heap_counts(int *allocs, int *frees) {
	*allocs = v_prealloc_test_heap_allocs;
	*frees = v_prealloc_test_heap_frees;
	return 0;
}

#endif
