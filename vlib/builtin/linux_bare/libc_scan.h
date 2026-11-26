#ifndef V_LINUX_BARE_SCAN_H
#define V_LINUX_BARE_SCAN_H
#include <stdarg.h>
#include <stddef.h>
#include <stdio.h>
#include <sys/types.h>
/* Numeric scanners share a cursor, never the native toolchain's private FILE.
 * Memory input can rewind freely; stream input keeps sixteen bytes of history. */
struct v_scan {
    FILE *stream;
    const unsigned char *memory;
    size_t position, length, history_at, history_end;
    unsigned char history[16];
    off_t count, limit;
    int stopped, binary;
};
int v_scan_get(struct v_scan *);
void v_scan_unget(struct v_scan *);
void v_scan_limit(struct v_scan *, off_t);
#define shgetc(f) v_scan_get(f)
#define shunget(f) v_scan_unget(f)
#define shlim(f, n) v_scan_limit(f, n)
#define shcnt(f) ((f)->count)
long double v_floatscan(struct v_scan *, int, int);
unsigned long long v_intscan(struct v_scan *, unsigned, int, unsigned long long);
int v_scan_core(struct v_scan *, const char *, va_list);
#endif
