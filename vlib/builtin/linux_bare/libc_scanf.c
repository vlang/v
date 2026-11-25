/* Adapted from musl; see COPYRIGHT.musl for the MIT license. */
#include <ctype.h>
#include <limits.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <wchar.h>
#include <wctype.h>

#include "libc_scan.h"

#define SIZE_hh -2
#define SIZE_h -1
#define SIZE_def 0
#define SIZE_l 1
#define SIZE_L 2
#define SIZE_ll 3

static void store_int(void *dest, int size, unsigned long long i) {
    if (!dest)
        return;
    switch (size) {
    case SIZE_hh:
        *(char *)dest = i;
        break;
    case SIZE_h:
        *(short *)dest = i;
        break;
    case SIZE_def:
        *(int *)dest = i;
        break;
    case SIZE_l:
        *(long *)dest = i;
        break;
    case SIZE_ll:
        *(long long *)dest = i;
        break;
    }
}

static void *arg_n(va_list ap, unsigned int n) {
    void *p;
    unsigned int i;
    va_list ap2;
    va_copy(ap2, ap);
    for (i = n; i > 1; i--)
        va_arg(ap2, void *);
    p = va_arg(ap2, void *);
    va_end(ap2);
    return p;
}

int v_scan_core(struct v_scan *restrict f, const char *restrict fmt, va_list ap) {
    int width;
    int size;
    int alloc = 0;
    int base;
    const unsigned char *p;
    int c, t;
    char *s;
    wchar_t *wcs;
    mbstate_t st;
    void *dest = NULL;
    int invert;
    int matches = 0;
    unsigned long long x;
    long double y;
    off_t pos = 0;
    unsigned char scanset[257];
    size_t i, k;
    wchar_t wc;

    for (p = (const unsigned char *)fmt; *p; p++) {

        alloc = 0;

        if (isspace(*p)) {
            while (isspace(p[1]))
                p++;
            shlim(f, 0);
            while (isspace(shgetc(f)))
                ;
            shunget(f);
            pos += shcnt(f);
            continue;
        }
        if (*p != '%' || p[1] == '%') {
            shlim(f, 0);
            if (*p == '%') {
                p++;
                while (isspace((c = shgetc(f))))
                    ;
            } else {
                c = shgetc(f);
            }
            if (c != *p) {
                shunget(f);
                if (c < 0)
                    goto input_fail;
                goto match_fail;
            }
            pos += shcnt(f);
            continue;
        }

        p++;
        if (*p == '*') {
            dest = 0;
            p++;
        } else if (isdigit(*p) && p[1] == '$') {
            dest = arg_n(ap, *p - '0');
            p += 2;
        } else {
            dest = va_arg(ap, void *);
        }

        for (width = 0; isdigit(*p); p++) {
            if (width > (INT_MAX - (*p - '0')) / 10)
                goto fmt_fail;
            width = 10 * width + *p - '0';
        }

        if (*p == 'm') {
            wcs = 0;
            s = 0;
            alloc = !!dest;
            p++;
        } else {
            alloc = 0;
        }

        size = SIZE_def;
        switch (*p++) {
        case 'h':
            if (*p == 'h')
                p++, size = SIZE_hh;
            else
                size = SIZE_h;
            break;
        case 'l':
            if (*p == 'l')
                p++, size = SIZE_ll;
            else
                size = SIZE_l;
            break;
        case 'j':
            size = SIZE_ll;
            break;
        case 'z':
        case 't':
            size = SIZE_l;
            break;
        case 'L':
            size = SIZE_L;
            break;
        case 'b':
        case 'B':
        case 'd':
        case 'i':
        case 'o':
        case 'u':
        case 'x':
        case 'a':
        case 'e':
        case 'f':
        case 'g':
        case 'A':
        case 'E':
        case 'F':
        case 'G':
        case 'X':
        case 's':
        case 'c':
        case '[':
        case 'S':
        case 'C':
        case 'p':
        case 'n':
            p--;
            break;
        default:
            goto fmt_fail;
        }

        t = *p;

        /* C or S */
        if ((t & 0x2f) == 3) {
            t |= 32;
            size = SIZE_l;
        }

        switch (t) {
        case 'c':
            if (width < 1)
                width = 1;
        case '[':
            break;
        case 'n':
            store_int(dest, size, pos);
            /* do not increment match count, etc! */
            continue;
        default:
            shlim(f, 0);
            while (isspace(shgetc(f)))
                ;
            shunget(f);
            pos += shcnt(f);
        }

        /* The cursor counts bytes for %n. Wide text limits count converted characters. */
        /* A suppressed scanset examines input bytes without converting its result. */
        int wide_text = size == SIZE_l && (t == 's' || t == 'c' || (t == '[' && dest));
        shlim(f, wide_text ? 0 : width);
        if (shgetc(f) < 0)
            goto input_fail;
        shunget(f);

        switch (t) {
        case 's':
        case 'c':
        case '[':
            if (t == 'c' || t == 's') {
                memset(scanset, -1, sizeof scanset);
                scanset[0] = 0;
                if (t == 's') {
                    scanset[1 + '\t'] = 0;
                    scanset[1 + '\n'] = 0;
                    scanset[1 + '\v'] = 0;
                    scanset[1 + '\f'] = 0;
                    scanset[1 + '\r'] = 0;
                    scanset[1 + ' '] = 0;
                }
            } else {
                if (*++p == '^')
                    p++, invert = 1;
                else
                    invert = 0;
                memset(scanset, invert, sizeof scanset);
                scanset[0] = 0;
                if (*p == '-')
                    p++, scanset[1 + '-'] = 1 - invert;
                else if (*p == ']')
                    p++, scanset[1 + ']'] = 1 - invert;
                for (; *p != ']'; p++) {
                    if (!*p)
                        goto fmt_fail;
                    if (*p == '-' && p[1] && p[1] != ']')
                        for (c = p++ [-1]; c < *p; c++)
                            scanset[1 + c] = 1 - invert;
                    scanset[1 + *p] = 1 - invert;
                }
            }
            wcs = 0;
            s = 0;
            i = 0;
            k = t == 'c' ? width + 1U : 31;
            if (wide_text) {
                if (alloc) {
                    wcs = malloc(k * sizeof(wchar_t));
                    if (!wcs)
                        goto alloc_fail;
                } else {
                    wcs = dest;
                }
                st = (mbstate_t){0};
                while ((!width || i < (size_t)width) && scanset[(c = shgetc(f)) + 1]) {
                    switch (mbrtowc(&wc, &(char){c}, 1, &st)) {
                    case -1:
                        goto input_fail;
                    case -2:
                        continue;
                    }
                    if (wcs)
                        wcs[i] = wc;
                    ++i;
                    if (alloc && i == k) {
                        k += k + 1;
                        wchar_t *tmp = realloc(wcs, k * sizeof(wchar_t));
                        if (!tmp)
                            goto alloc_fail;
                        wcs = tmp;
                    }
                }
                if (!mbsinit(&st))
                    goto input_fail;
            } else if (alloc) {
                s = malloc(k);
                if (!s)
                    goto alloc_fail;
                while (scanset[(c = shgetc(f)) + 1]) {
                    s[i++] = c;
                    if (i == k) {
                        k += k + 1;
                        char *tmp = realloc(s, k);
                        if (!tmp)
                            goto alloc_fail;
                        s = tmp;
                    }
                }
            } else if ((s = dest)) {
                while (scanset[(c = shgetc(f)) + 1])
                    s[i++] = c;
            } else {
                while (scanset[(c = shgetc(f)) + 1])
                    ;
            }
            if (!wide_text || !width || i < (size_t)width)
                shunget(f);
            if (!shcnt(f))
                goto match_fail;
            if (t == 'c' && (wide_text ? i != (size_t)width : shcnt(f) != width))
                goto match_fail;
            if (alloc) {
                if (size == SIZE_l)
                    *(wchar_t **)dest = wcs;
                else
                    *(char **)dest = s;
            }
            if (t != 'c') {
                if (wcs)
                    wcs[i] = 0;
                if (s)
                    s[i] = 0;
            }
            break;
        case 'b':
        case 'B':
            if (!f->binary)
                goto fmt_fail;
            base = 2;
            goto int_common;
        case 'p':
        case 'X':
        case 'x':
            base = 16;
            goto int_common;
        case 'o':
            base = 8;
            goto int_common;
        case 'd':
        case 'u':
            base = 10;
            goto int_common;
        case 'i':
            base = 0;
        int_common:
            x = v_intscan(f, base, 0, ULLONG_MAX);
            if (!shcnt(f))
                goto match_fail;
            if (t == 'p' && dest)
                *(void **)dest = (void *)(uintptr_t)x;
            else
                store_int(dest, size, x);
            break;
        case 'a':
        case 'A':
        case 'e':
        case 'E':
        case 'f':
        case 'F':
        case 'g':
        case 'G':
            y = v_floatscan(f, size, 0);
            if (!shcnt(f))
                goto match_fail;
            if (dest)
                switch (size) {
                case SIZE_def:
                    *(float *)dest = y;
                    break;
                case SIZE_l:
                    *(double *)dest = y;
                    break;
                case SIZE_L:
                    *(long double *)dest = y;
                    break;
                }
            break;
        }

        pos += shcnt(f);
        if (dest)
            matches++;
    }
    if (0) {
    fmt_fail:
    alloc_fail:
    input_fail:
        if (!matches)
            matches--;
    match_fail:
        if (alloc) {
            free(s);
            free(wcs);
        }
    }

    return matches;
}
