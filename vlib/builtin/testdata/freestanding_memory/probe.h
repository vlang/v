#ifndef V_FREESTANDING_MEMORY_PROBE_H
#define V_FREESTANDING_MEMORY_PROBE_H
#include <stddef.h>
#include <string.h>

static void *(*volatile v_test_memchr_call)(const void *, int, size_t) = memchr;

static int v_test_memchr(void) {
    unsigned char bytes[768];
    const size_t lengths[] = {0, 1, 2, 3, 7, 15, 16, 17, 31, 32, 33,
                             63, 64, 65, 127, 128, 129, 255, 256, 257, 511};
    for (size_t i = 0; i < sizeof bytes; ++i) bytes[i] = (unsigned char)(i * 41);
    if (v_test_memchr_call(NULL, 123, 0) != NULL) return 1;
    for (size_t start = 0; start < 32; ++start) {
        for (size_t li = 0; li < sizeof lengths / sizeof *lengths; ++li) {
            size_t length = lengths[li];
            for (int needle = -1; needle <= 511; ++needle) {
                const unsigned char *expected = NULL;
                for (size_t i = 0; i < length; ++i) {
                    if (bytes[start + i] == (unsigned char)needle) {
                        expected = bytes + start + i;
                        break;
                    }
                }
                if (v_test_memchr_call(bytes + start, needle, length) != expected) return 2;
            }
        }
    }
    return 0;
}

static int v_test_memchr_boundary(unsigned char *end) {
    for (size_t i = 0; i < 32; ++i) end[-32 + (ptrdiff_t)i] = 1;
    if (v_test_memchr_call(end - 32, 2, 32) != NULL) return 3;
    end[-1] = 2;
    if (v_test_memchr_call(end - 32, 2, 32) != end - 1) return 4;
    if (v_test_memchr_call(end - 1, 2, (size_t)-1) != end - 1) return 5;
    return 0;
}
#endif
