#include <stddef.h>
#include <stdint.h>
void trace_test_forbid(int);
void trace_test_sink(const uint8_t *, size_t);
void trace_test_reset(void);
void trace_test_expected(uint32_t, int64_t, int64_t, const uint8_t *);
int trace_test_matches(void);
int trace_test_contains(const uint8_t *);
void trace_test_set_nested(void (*)(void));
void trace_test_fail_writes(int);
