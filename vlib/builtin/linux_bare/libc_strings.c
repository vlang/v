#include "libc_compat.h"
#include "libc_sort.h"
#include <limits.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#undef strrchr
#undef bsearch
int strncmp(const char *a, const char *b, size_t n) {
    while (n && *a && *a == *b) {
        ++a;
        ++b;
        --n;
    }
    return n ? (unsigned char)*a - (unsigned char)*b : 0;
}

char *strcpy(char *d, const char *s) {
    char *r = d;
    while ((*d++ = *s++))
        ;
    return r;
}
char *stpcpy(char *d, const char *s) {
    while ((*d = *s)) {
        ++d;
        ++s;
    }
    return d;
}
char *strncpy(char *d, const char *s, size_t n) {
    char *r = d;
    while (n && *s) {
        *d++ = *s++;
        --n;
    }
    memset(d, 0, n);
    return r;
}
char *strcat(char *d, const char *s) {
    strcpy(d + strlen(d), s);
    return d;
}
char *strrchr(const char *s, int c) {
    const char *r = NULL;
    do {
        if ((unsigned char)*s == (unsigned char)c)
            r = s;
    } while (*s++);
    return (char *)r;
}
char *strdup(const char *s) {
    size_t n = strlen(s) + 1;
    char *d = malloc(n);
    if (d)
        memcpy(d, s, n);
    else
        errno = ENOMEM;
    return d;
}
char *strndup(const char *s, size_t n) {
    size_t len = 0;
    while (len < n && s[len])
        ++len;
    if (len == SIZE_MAX) {
        errno = ENOMEM;
        return NULL;
    }
    char *d = malloc(len + 1);
    if (!d) {
        errno = ENOMEM;
        return NULL;
    }
    memcpy(d, s, len);
    d[len] = 0;
    return d;
}
static unsigned char v_lower(unsigned char c) { return c >= 'A' && c <= 'Z' ? c + 32 : c; }
int strcasecmp(const char *a, const char *b) {
    for (;; ++a, ++b) {
        int x = v_lower(*a), y = v_lower(*b);
        if (x != y || !x)
            return x - y;
    }
}
int strncasecmp(const char *a, const char *b, size_t n) {
    for (; n; --n, ++a, ++b) {
        int x = v_lower(*a), y = v_lower(*b);
        if (x != y || !x)
            return x - y;
    }
    return 0;
}
void *reallocarray(void *p, size_t n, size_t size) {
    if (size && n > SIZE_MAX / size) {
        errno = ENOMEM;
        return NULL;
    }
    void *r = realloc(p, n * size);
    if (!r && n && size)
        errno = ENOMEM;
    return r;
}

void qsort_r(void *base, size_t n, size_t size, int (*compare)(const void *, const void *, void *),
             void *context) {
    v_freestanding_qsort_r(base, n, size, compare, context);
}
void *bsearch(const void *key, const void *base, size_t n, size_t size,
              int (*compare)(const void *, const void *)) {
    while (n) {
        size_t mid = n / 2;
        const unsigned char *p = (const unsigned char *)base + mid * size;
        int c = compare(key, p);
        if (!c)
            return (void *)p;
        if (c < 0)
            n = mid;
        else {
            base = p + size;
            n -= mid + 1;
        }
    }
    return NULL;
}
size_t strspn(const char *s, const char *set) {
    size_t n = 0;
    while (s[n] && strchr(set, (unsigned char)s[n]))
        ++n;
    return n;
}
size_t strcspn(const char *s, const char *set) {
    size_t n = 0;
    while (s[n] && !strchr(set, (unsigned char)s[n]))
        ++n;
    return n;
}

char *(strpbrk)(const char *text, const char *accepted) {
    for (; *text; text++)
        if (strchr(accepted, (unsigned char)*text)) return (char *)text;
    return NULL;
}
