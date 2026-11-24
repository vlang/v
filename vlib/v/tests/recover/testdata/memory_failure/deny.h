#include <stdlib.h>

#ifdef _WIN32
#include <malloc.h>
#endif

static int deny_allocations;
static void test_deny_allocations(int enabled) { deny_allocations = enabled; }
static void *test_malloc(size_t n) { return deny_allocations ? NULL : malloc(n); }
static void *test_realloc(void *p, size_t n) { return deny_allocations ? NULL : realloc(p, n); }
#define malloc(n) test_malloc(n)
#define realloc(p, n) test_realloc(p, n)

#ifdef _WIN32
static void *test_aligned_malloc(size_t n, size_t alignment) {
	return deny_allocations ? NULL : _aligned_malloc(n, alignment);
}
#define _aligned_malloc(n, a) test_aligned_malloc(n, a)
#endif
