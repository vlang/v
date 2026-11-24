#include <stdlib.h>
#ifdef _WIN32
#include <malloc.h>
#endif

static int map_delete_armed;
static int map_delete_denied;
static int map_delete_countdown;
static void map_delete_arm_on(int n) {
	map_delete_denied = 0;
	map_delete_armed = 1;
	map_delete_countdown = n;
}
static int map_delete_should_fail(void) {
	if (!map_delete_armed || --map_delete_countdown != 0) return 0;
	map_delete_armed = 0;
	map_delete_denied = 1;
	return 1;
}
static int map_delete_disarm(void) {
	map_delete_armed = 0;
	return map_delete_denied;
}
static void *map_delete_malloc(size_t n) {
	return map_delete_should_fail() ? NULL : malloc(n);
}
static void *map_delete_calloc(size_t n, size_t s) {
	return map_delete_should_fail() ? NULL : calloc(n, s);
}
static void *map_delete_realloc(void *p, size_t n) {
	return map_delete_should_fail() ? NULL : realloc(p, n);
}
#define malloc(n) map_delete_malloc(n)
#define calloc(n, s) map_delete_calloc(n, s)
#define realloc(p, n) map_delete_realloc(p, n)
#ifdef _WIN32
static void *map_delete_aligned_malloc(size_t n, size_t a) {
	return map_delete_should_fail() ? NULL : _aligned_malloc(n, a);
}
static void *map_delete_aligned_realloc(void *p, size_t n, size_t a) {
	return map_delete_should_fail() ? NULL : _aligned_realloc(p, n, a);
}
#define _aligned_malloc(n, a) map_delete_aligned_malloc(n, a)
#define _aligned_realloc(p, n, a) map_delete_aligned_realloc(p, n, a)
#endif
