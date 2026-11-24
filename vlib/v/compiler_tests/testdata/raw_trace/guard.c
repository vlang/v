#include "guard.h"
#include <errno.h>
#include <inttypes.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

static int forbidden;
static int failing_write;
static void (*nested)(void);
static unsigned char captured[16384];
static char expected[16384];
static size_t captured_len;
static size_t expected_len;

/* Fail immediately instead of hanging on a simulated allocator's held lock. */
static void reject_allocation(void) {
    if (forbidden) {
        static const char text[] = "allocation/free in locked raw trace stage ";
        char stage = (char)('0' + forbidden);
        (void)write(2, text, sizeof text - 1);
        (void)write(2, &stage, 1);
        (void)write(2, "\n", 1);
        _exit(94);
    }
}
void *__real_malloc(size_t);
void *__real_calloc(size_t, size_t);
void *__real_realloc(void *, size_t);
void __real_free(void *);
void *__wrap_malloc(size_t size) { reject_allocation(); return __real_malloc(size); }
void *__wrap_calloc(size_t count, size_t size) { reject_allocation(); return __real_calloc(count, size); }
void *__wrap_realloc(void *area, size_t size) { reject_allocation(); return __real_realloc(area, size); }
void __wrap_free(void *area) { reject_allocation(); __real_free(area); }

void trace_test_forbid(int stage) { forbidden = stage; }
void trace_test_set_nested(void (*callback)(void)) { nested = callback; }
void trace_test_fail_writes(int fail) { failing_write = fail; }
void trace_test_reset(void) { captured_len = 0; captured[0] = 0; }
void trace_test_sink(const uint8_t *text, size_t length) {
    if (nested) nested();
    if (failing_write) { (void)write(-1, text, length); return; }
    if (length >= sizeof captured - captured_len) _exit(95);
    memcpy(captured + captured_len, text, length);
    captured_len += length;
    captured[captured_len] = 0;
}
void trace_test_expected(uint32_t tid, int64_t ns, int64_t stack, const uint8_t *name) {
    int count = snprintf(expected, sizeof expected,
        "> trace %8" PRIu32 " %8" PRId64 " %8" PRId64 " %s\n", tid, ns, stack, (const char *)name);
    if (count < 0 || (size_t)count >= sizeof expected) _exit(96);
    expected_len = (size_t)count;
}
int trace_test_matches(void) {
    return captured_len == expected_len && memcmp(captured, expected, expected_len) == 0;
}
int trace_test_contains(const uint8_t *text) {
    return strstr((const char *)captured, (const char *)text) != NULL;
}
