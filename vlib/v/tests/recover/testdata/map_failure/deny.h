#include <stdlib.h>
#ifdef _WIN32
#include <malloc.h>
#endif

// Fail one allocation; the panic unwinder and subsequent assertions may allocate.
static int map_failure_countdown;
static void map_fail_on(int n) { map_failure_countdown = n; }
static int map_should_fail(void) {
	return map_failure_countdown > 0 && --map_failure_countdown == 0;
}
static void *map_test_malloc(size_t n) { return map_should_fail() ? NULL : malloc(n); }
static void *map_test_calloc(size_t n, size_t size) { return map_should_fail() ? NULL : calloc(n, size); }
static void *map_test_realloc(void *p, size_t n) { return map_should_fail() ? NULL : realloc(p, n); }
#define malloc(n) map_test_malloc(n)
#define calloc(n, size) map_test_calloc(n, size)
#define realloc(p, n) map_test_realloc(p, n)
#ifdef _WIN32
static void *map_test_aligned_malloc(size_t n, size_t a) {
	return map_should_fail() ? NULL : _aligned_malloc(n, a);
}
static void *map_test_aligned_realloc(void *p, size_t n, size_t a) {
	return map_should_fail() ? NULL : _aligned_realloc(p, n, a);
}
#define _aligned_malloc(n, a) map_test_aligned_malloc(n, a)
#define _aligned_realloc(p, n, a) map_test_aligned_realloc(p, n, a)
#endif
