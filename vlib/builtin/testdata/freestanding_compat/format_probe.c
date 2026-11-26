#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "probe.h"
#include <errno.h>
#include <fcntl.h>
#include <float.h>
#include <inttypes.h>
#include <limits.h>
#include <stdint.h>
#include <stddef.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <wchar.h>

/* Serialize the return value, errno on failure, %n, and all output bytes.
 * The same fixture is compiled against the hosted C library and the bare
 * runtime. Raw descriptor writes keep the comparison independent of printf. */
static int (*volatile format_output)(char *, size_t, const char *, ...) = snprintf;
static uint32_t record_number;
static void number(unsigned char *out, uint32_t value) {
    for (int i = 0; i < 4; ++i) out[i] = (unsigned char)(value >> (i * 8));
}
static int record(int fd, const unsigned char *buffer, size_t capacity, int result, int count, int error) {
    unsigned char bytes[276];
    number(bytes, record_number++);
    number(bytes + 4, (uint32_t)capacity);
    number(bytes + 8, (uint32_t)result);
    number(bytes + 12, (uint32_t)count);
    number(bytes + 16, result < 0 ? (uint32_t)error : 0);
    memcpy(bytes + 20, buffer, 256);
    size_t done = 0;
    while (done < sizeof bytes) {
        ssize_t size = write(fd, bytes + done, sizeof bytes - done);
        if (size < 0 && errno == EINTR) continue;
        if (size <= 0) return -1;
        done += (size_t)size;
    }
    return 0;
}

static void rounding(unsigned int mode, uint64_t saved[2], int restore) {
#if defined(__x86_64__)
    unsigned short control;
    unsigned int simd;
    if (!restore) {
        __asm__ volatile("fnstcw %0" : "=m"(control));
        __asm__ volatile("stmxcsr %0" : "=m"(simd));
        saved[0] = control; saved[1] = simd;
        control = (unsigned short)((control & ~0xc00) | (mode << 10));
        simd = (simd & ~0x6000) | (mode << 13);
    } else { control = (unsigned short)saved[0]; simd = (unsigned int)saved[1]; }
    __asm__ volatile("fldcw %0" : : "m"(control));
    __asm__ volatile("ldmxcsr %0" : : "m"(simd));
#elif defined(__aarch64__)
    uint64_t value;
    if (!restore) {
        static const unsigned int encoded[] = {0, 2, 1, 3};
        __asm__ volatile("mrs %0, fpcr" : "=r"(value));
        saved[0] = value;
        value = (value & ~(UINT64_C(3) << 22)) | ((uint64_t)encoded[mode] << 22);
    } else value = saved[0];
    __asm__ volatile("msr fpcr, %0" : : "r"(value));
#endif
}

#define EMIT(expression) do { \
    memset(buffer, 0xa5, sizeof buffer); count = -1; errno = 77; \
    int result = (expression); int error = errno; \
    if (record(fd, buffer, capacity, result, count, error)) { close(fd); return __LINE__; } \
} while (0)

int v_freestanding_format_oracle(const char *path) {
    int fd = open(path, O_WRONLY | O_CREAT | O_TRUNC, 0600);
    if (fd < 0) return __LINE__;
    unsigned char buffer[256];
    int count;
    const size_t capacities[] = {0, 1, 2, 5, 31, 256};
    for (unsigned int c = 0; c < sizeof capacities / sizeof *capacities; ++c) {
        size_t capacity = capacities[c];
        EMIT(format_output((char *)buffer, capacity, "%+08d %#x %.0u %zu %lld", -42, 255u, 0u, (size_t)123, LLONG_MIN));
        EMIT(format_output((char *)buffer, capacity, "%hhd %hd %ld %lld %jd %zd %td", (int)SCHAR_MIN, (int)SHRT_MIN,
                           LONG_MIN, LLONG_MIN, INTMAX_MIN, (ssize_t)-1, (ptrdiff_t)-1));
        EMIT(format_output((char *)buffer, capacity, "%hhu %hu %lu %llu %ju %zu", (unsigned int)UCHAR_MAX,
                           (unsigned int)USHRT_MAX, ULONG_MAX, ULLONG_MAX, UINTMAX_MAX, SIZE_MAX));
        EMIT(format_output((char *)buffer, capacity, "[%*.*s][%-9c][%#08X][%#.0o]", -9, 3, "abcdef", 'x', 0xabcdu, 0u));
        EMIT(format_output((char *)buffer, capacity, "abc%n:%08d", &count, -42));
        EMIT(format_output((char *)buffer, capacity, "%3$*1$.*2$f:%4$d", 15, 7, 1.0 / 7.0, -42));
        EMIT(format_output((char *)buffer, capacity, "%2$d %1$s %2$d", "repeated", 123));
        EMIT(format_output((char *)buffer, capacity, "[%8.3ls][%lc][%lc]", L"abcdef", (wint_t)L'A', (wint_t)0));
        EMIT(format_output((char *)buffer, capacity, "%ls", L"\u00e9"));
        EMIT(format_output((char *)buffer, capacity, "%.*s", -1, "negative precision"));
        EMIT(format_output((char *)buffer, capacity, "%2147483647sX", ""));
        const uint64_t special[] = {0, UINT64_C(0x8000000000000000), 1, UINT64_C(0x000fffffffffffff),
            UINT64_C(0x0010000000000000), UINT64_C(0x7fefffffffffffff), UINT64_C(0x7ff0000000000000),
            UINT64_C(0xfff0000000000000), UINT64_C(0x7ff8000000000001), UINT64_C(0xfff8000000000001),
            UINT64_C(0x3fe0000000000000), UINT64_C(0x3ff8000000000000)};
        const char *formats[] = {"%f", "%e", "%g", "%a", "%A", "%+#025.17g", "% .0f", "%#.0g", "%.1075f"};
        for (unsigned int i = 0; i < sizeof special / sizeof *special; ++i) {
            union { uint64_t bits; double value; } input = {.bits = special[i]};
            for (unsigned int j = 0; j < sizeof formats / sizeof *formats; ++j) {
                /* C permits different hexadecimal normalizations of subnormals. */
                if ((j == 3 || j == 4) && (special[i] & UINT64_C(0x7ff0000000000000)) == 0 &&
                    (special[i] & UINT64_C(0x000fffffffffffff))) continue;
                EMIT(format_output((char *)buffer, capacity, formats[j], input.value));
            }
        }
#ifndef V_FORMAT_ORACLE_NO_EXTENDED
        const long double extended[] = {0.0L, -0.0L, 0.5L, 1.5L, LDBL_MIN, LDBL_MAX, LDBL_EPSILON, 1.0L / 7.0L};
        for (unsigned int i = 0; i < sizeof extended / sizeof *extended; ++i) {
            EMIT(format_output((char *)buffer, capacity, "%+#.36Lg", extended[i]));
            EMIT(format_output((char *)buffer, capacity, "%.36Le", extended[i]));
            EMIT(format_output((char *)buffer, capacity, "%.36Lf", extended[i]));
        }
#endif
    }
    uint64_t random = UINT64_C(0x4d595df4d0f33173);
    for (unsigned int mode = 0; mode < 4; ++mode) {
        uint64_t saved[2];
        rounding(mode, saved, 0);
        size_t capacity = sizeof buffer;
        for (unsigned int i = 0; i < 2500; ++i) {
            random ^= random >> 12; random ^= random << 25; random ^= random >> 27;
            union { uint64_t bits; double value; } input = {.bits = random * UINT64_C(2685821657736338717)};
            int precision = (int)(i % 21);
            EMIT(format_output((char *)buffer, capacity, "%.*g", precision, input.value));
            EMIT(format_output((char *)buffer, capacity, "%.*e", precision, input.value));
            EMIT(format_output((char *)buffer, capacity, "%.*f", precision, input.value));
            if ((input.bits & UINT64_C(0x7ff0000000000000)) || !(input.bits & UINT64_C(0x000fffffffffffff)))
                EMIT(format_output((char *)buffer, capacity, "%.*a", precision % 9, input.value));
        }
        rounding(mode, saved, 1);
    }
    if (close(fd) < 0) return __LINE__;
    return 0;
}

static int hexadecimal_value(const char *text, uint64_t bits) {
    int negative = *text == '-';
    if (negative) ++text;
    if (negative != (int)(bits >> 63) || text[0] != '0' || text[1] != 'x') return 0;
    text += 2;
    uint64_t mantissa = 0;
    int fraction = 0, after_point = 0;
    while (*text && *text != 'p') {
        if (*text == '.') { if (after_point) return 0; after_point = 1; ++text; continue; }
        int digit = *text >= '0' && *text <= '9' ? *text - '0' : *text >= 'a' && *text <= 'f' ? *text - 'a' + 10 : -1;
        if (digit < 0 || mantissa > UINT64_MAX / 16) return 0;
        mantissa = mantissa * 16 + (unsigned int)digit;
        if (after_point) fraction += 4;
        ++text;
    }
    if (*text++ != 'p') return 0;
    int sign = *text == '-';
    if (*text == '-' || *text == '+') ++text;
    int exponent = 0;
    while (*text >= '0' && *text <= '9') { if (exponent > 10000) return 0; exponent = exponent * 10 + *text++ - '0'; }
    if (*text || !mantissa) return 0;
    exponent = (sign ? -exponent : exponent) - fraction;
    uint64_t expected = bits & UINT64_C(0x000fffffffffffff);
    int expected_exponent = -1074;
    while (!(mantissa & 1)) { mantissa >>= 1; ++exponent; }
    while (!(expected & 1)) { expected >>= 1; ++expected_exponent; }
    return mantissa == expected && exponent == expected_exponent;
}

int v_freestanding_format_contracts(void) {
    char buffer[128];
    if (format_output(buffer, sizeof buffer, "%#08b|%#.0b|%-#6.3hhB", 10u, 0u, 258u) != 16 ||
        strcmp(buffer, "0b001010||0B010 ")) return __LINE__;
    if (format_output(buffer, sizeof buffer, "%#llb", ULLONG_MAX) != 66 ||
        strncmp(buffer, "0b", 2) || strspn(buffer + 2, "1") != 64) return __LINE__;
    if (format_output(buffer, 5, "%#.64jb", (uintmax_t)1) != 66 ||
        strcmp(buffer, "0b00")) return __LINE__;
    /* INT_MIN's implied positive width exceeds the return type. glibc's empty
     * string case returns zero here; require the POSIX EOVERFLOW contract. */
    errno = 0;
    if (format_output(buffer, sizeof buffer, "%*s", INT_MIN, "") != -1 || errno != EOVERFLOW) return __LINE__;
    if (format_output(buffer, sizeof buffer, "%.*f", INT_MAX - 2, 0.0) != INT_MAX) return __LINE__;
    if (buffer[0] != '0' || buffer[1] != '.' || buffer[sizeof buffer - 1]) return __LINE__;
    for (unsigned int i = 2; i < sizeof buffer - 1; ++i) if (buffer[i] != '0') return __LINE__;
    if (format_output(buffer, sizeof buffer, "%.*e", INT_MAX - 6, 0.0) != INT_MAX) return __LINE__;
    if (format_output(buffer, sizeof buffer, "%.*g", INT_MAX, 0.0) != 1 || strcmp(buffer, "0")) return __LINE__;
    errno = 0;
    if (format_output(buffer, sizeof buffer, "%#.*g", INT_MAX, 0.0) != -1 || errno != EOVERFLOW) return __LINE__;
    signed char a = 0; short b = 0; int c = 0; long d = 0; long long e = 0;
    intmax_t f = 0; ssize_t g = 0; ptrdiff_t h = 0;
    int result = format_output(buffer, 5, "a%hhnb%hnc%nd%lne%llnf%jng%znh%tn", &a, &b, &c, &d, &e, &f, &g, &h);
    if (result != 8 || strcmp(buffer, "abcd") || a != 1 || b != 2 || c != 3 || d != 4 || e != 5 || f != 6 || g != 7 || h != 8)
        return __LINE__;
    const uint64_t subnormals[] = {1, 7, UINT64_C(0x000fffffffffffff), UINT64_C(0x8000000000000001)};
    for (unsigned int i = 0; i < sizeof subnormals / sizeof *subnormals; ++i) {
        union { uint64_t bits; double value; } input = {.bits = subnormals[i]};
        if (format_output(buffer, sizeof buffer, "%a", input.value) < 0 || !hexadecimal_value(buffer, input.bits)) return __LINE__;
    }
    return 0;
}
