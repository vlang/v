#include "libc_scan.h"
#include "libc_compat.h"
#include <inttypes.h>
#include <limits.h>
#include <stdlib.h>
#include <string.h>

void v_scan_limit(struct v_scan *s, off_t limit) {
    s->count = 0;
    s->limit = limit;
    s->stopped = 0;
}
int v_scan_get(struct v_scan *s) {
    if (s->stopped)
        return EOF;
    if (s->limit && s->count >= s->limit) {
        s->stopped = 1;
        return EOF;
    }
    int ch;
    if (!s->stream) {
        if (s->position == s->length) {
            s->stopped = 1;
            return EOF;
        }
        ch = s->memory[s->position++];
    } else if (s->history_at < s->history_end)
        ch = s->history[s->history_at++];
    else {
        ch = fgetc_unlocked(s->stream);
        if (ch == EOF) {
            s->stopped = 1;
            return EOF;
        }
        if (s->history_end == sizeof s->history) {
            memmove(s->history, s->history + 1, sizeof s->history - 1);
            --s->history_at;
            --s->history_end;
        }
        s->history[s->history_end++] = ch;
        ++s->history_at;
    }
    ++s->count;
    return ch;
}
void v_scan_unget(struct v_scan *s) {
    if (s->stopped)
        return;
    if (s->stream) {
        if (!s->history_at)
            return;
        --s->history_at;
    } else {
        if (!s->position)
            return;
        --s->position;
    }
    --s->count;
}
static int v_scan_string(const char *text, const char *format, va_list args, int binary) {
    struct v_scan s = {
        .memory = (const unsigned char *)text, .length = strlen(text), .binary = binary};
    return v_scan_core(&s, format, args);
}
static int v_scan_stream(FILE *stream, const char *format, va_list args, int binary) {
    struct v_scan s = {.stream = stream, .binary = binary};
    flockfile(stream);
    int result = v_scan_core(&s, format, args);
    while (s.history_end > s.history_at)
        ungetc(s.history[--s.history_end], stream);
    funlockfile(stream);
    return result;
}
/* Explicit assembler names avoid glibc headers redirecting definitions to the
 * C99/C23 entry points. Each mode retains its own prefix/conversion rules. */
#define V_SCAN_WRAPPERS(tag, symbol, binary)                                                       \
    int tag##_vsscanf(const char *s, const char *fmt, va_list a) __asm__(symbol "vsscanf");        \
    int tag##_vsscanf(const char *s, const char *fmt, va_list a) {                                 \
        return v_scan_string(s, fmt, a, binary);                                                   \
    }                                                                                              \
    int tag##_sscanf(const char *s, const char *fmt, ...) __asm__(symbol "sscanf");                \
    int tag##_sscanf(const char *s, const char *fmt, ...) {                                        \
        va_list a;                                                                                 \
        va_start(a, fmt);                                                                          \
        int r = v_scan_string(s, fmt, a, binary);                                                  \
        va_end(a);                                                                                 \
        return r;                                                                                  \
    }                                                                                              \
    int tag##_vfscanf(FILE *s, const char *fmt, va_list a) __asm__(symbol "vfscanf");              \
    int tag##_vfscanf(FILE *s, const char *fmt, va_list a) {                                       \
        return v_scan_stream(s, fmt, a, binary);                                                   \
    }                                                                                              \
    int tag##_fscanf(FILE *s, const char *fmt, ...) __asm__(symbol "fscanf");                      \
    int tag##_fscanf(FILE *s, const char *fmt, ...) {                                              \
        va_list a;                                                                                 \
        va_start(a, fmt);                                                                          \
        int r = v_scan_stream(s, fmt, a, binary);                                                  \
        va_end(a);                                                                                 \
        return r;                                                                                  \
    }                                                                                              \
    int tag##_vscanf(const char *fmt, va_list a) __asm__(symbol "vscanf");                         \
    int tag##_vscanf(const char *fmt, va_list a) { return v_scan_stream(stdin, fmt, a, binary); }  \
    int tag##_scanf(const char *fmt, ...) __asm__(symbol "scanf");                                 \
    int tag##_scanf(const char *fmt, ...) {                                                        \
        va_list a;                                                                                 \
        va_start(a, fmt);                                                                          \
        int r = v_scan_stream(stdin, fmt, a, binary);                                              \
        va_end(a);                                                                                 \
        return r;                                                                                  \
    }
V_SCAN_WRAPPERS(v_legacy, "", 0)
V_SCAN_WRAPPERS(v_c99, "__isoc99_", 0)
V_SCAN_WRAPPERS(v_c23, "__isoc23_", 1)

static unsigned long long v_string_integer(const char *text, char **end, int base,
                                           unsigned long long limit, int binary) {
    struct v_scan s = {
        .memory = (const unsigned char *)text, .length = strlen(text) + 1, .binary = binary};
    unsigned long long result = v_intscan(&s, base, 1, limit);
    if (end)
        *end = (char *)text + s.count;
    return result;
}
#define V_STRTOS(tag, prefix, binary)                                                              \
    long tag##_strtol(const char *s, char **e, int b) __asm__(prefix "strtol");                    \
    long tag##_strtol(const char *s, char **e, int b) {                                            \
        return v_string_integer(s, e, b, 0UL - LONG_MIN, binary);                                  \
    }                                                                                              \
    unsigned long tag##_strtoul(const char *s, char **e, int b) __asm__(prefix "strtoul");         \
    unsigned long tag##_strtoul(const char *s, char **e, int b) {                                  \
        return v_string_integer(s, e, b, ULONG_MAX, binary);                                       \
    }                                                                                              \
    long long tag##_strtoll(const char *s, char **e, int b) __asm__(prefix "strtoll");             \
    long long tag##_strtoll(const char *s, char **e, int b) {                                      \
        return v_string_integer(s, e, b, 0ULL - LLONG_MIN, binary);                                \
    }                                                                                              \
    unsigned long long tag##_strtoull(const char *s, char **e, int b) __asm__(prefix "strtoull");  \
    unsigned long long tag##_strtoull(const char *s, char **e, int b) {                            \
        return v_string_integer(s, e, b, ULLONG_MAX, binary);                                      \
    }
V_STRTOS(v_old, "", 0)
V_STRTOS(v_new, "__isoc23_", 1)
int atoi(const char *s) { return (int)v_old_strtol(s, NULL, 10); }
long atol(const char *s) { return v_old_strtol(s, NULL, 10); }
long long atoll(const char *s) { return v_old_strtoll(s, NULL, 10); }
#define V_STRTOMAX(tag, prefix, binary)                                                            \
    intmax_t tag##_imax(const char *s, char **e, int b) __asm__(prefix "strtoimax");               \
    intmax_t tag##_imax(const char *s, char **e, int b) {                                          \
        return v_string_integer(s, e, b, 0ULL - LLONG_MIN, binary);                                \
    }                                                                                              \
    uintmax_t tag##_umax(const char *s, char **e, int b) __asm__(prefix "strtoumax");              \
    uintmax_t tag##_umax(const char *s, char **e, int b) {                                         \
        return v_string_integer(s, e, b, ULLONG_MAX, binary);                                      \
    }
V_STRTOMAX(v_oldmax, "", 0)
V_STRTOMAX(v_newmax, "__isoc23_", 1)
static long double v_string_float(const char *text, char **end, int precision) {
    struct v_scan s = {.memory = (const unsigned char *)text, .length = strlen(text) + 1};
    long double result = v_floatscan(&s, precision, 1);
    if (end)
        *end = (char *)text + s.count;
    return result;
}
float strtof(const char *s, char **end) { return v_string_float(s, end, 0); }
double strtod(const char *s, char **end) { return v_string_float(s, end, 1); }
long double strtold(const char *s, char **end) { return v_string_float(s, end, 2); }
double atof(const char *s) { return strtod(s, NULL); }
