#include "probe.h"
#include <stdio.h>
#include <stdarg.h>
#include <stdint.h>
#include <string.h>
#include <time.h>
#include <errno.h>
#include <sched.h>

static int notes;
static int gate;
void trace_note(void) { __atomic_add_fetch(&notes, 1, __ATOMIC_SEQ_CST); }
int trace_notes(void) { return __atomic_load_n(&notes, __ATOMIC_SEQ_CST); }
int trace_gate(void) { return __atomic_load_n(&gate, __ATOMIC_SEQ_CST); }

/* Deliberately call V from stdio, as a V-backed formatter can do. */
int fprintf(FILE *stream, const char *format, ...) {
    va_list values;
    va_start(values, format);
    if (format[0] == '>') {
        va_list inspect;
        va_copy(inspect, values);
        (void)va_arg(inspect, unsigned);
        (void)va_arg(inspect, uint64_t);
        (void)va_arg(inspect, uint64_t);
        const char *name = va_arg(inspect, const char *);
        va_end(inspect);
        if (strstr(name, "main.work/")) {
            __atomic_add_fetch(&gate, 1, __ATOMIC_SEQ_CST);
            struct timespec start, now;
            clock_gettime(CLOCK_MONOTONIC, &start);
            do {
                if (trace_gate() == 2) break;
                sched_yield();
                clock_gettime(CLOCK_MONOTONIC, &now);
            } while (now.tv_sec - start.tv_sec < 2);
        }
    }
    trace_reenter();
    int result = vfprintf(stream, format, values);
    va_end(values);
    errno = ERANGE;
    return result;
}
