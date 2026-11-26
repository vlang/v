#include "libc_sort.h"
#include <stdint.h>

/* Iterative heapsort uses bounded stack space, with no allocation or callback
 * casts. It also sorts when the process is too low on memory for mergesort. */
static void v_swap(unsigned char *a, unsigned char *b, size_t size) {
    while (size--) {
        unsigned char c = *a;
        *a++ = *b;
        *b++ = c;
    }
}
typedef int (*v_compare)(const void *, const void *, void *);
static void v_sift(unsigned char *base, size_t root, size_t n, size_t size, v_compare compare,
                   void *arg) {
    while (root < n / 2) {
        size_t child = root * 2 + 1;
        if (child + 1 < n && compare(base + child * size, base + (child + 1) * size, arg) < 0)
            ++child;
        if (compare(base + root * size, base + child * size, arg) >= 0)
            break;
        v_swap(base + root * size, base + child * size, size);
        root = child;
    }
}
void v_freestanding_qsort_r(void *base, size_t n, size_t size, v_compare compare, void *arg) {
    if (n < 2 || !size || n > SIZE_MAX / size)
        return;
    for (size_t i = n / 2; i; i--)
        v_sift(base, i - 1, n, size, compare, arg);
    for (size_t i = n - 1; i; i--) {
        v_swap(base, (unsigned char *)base + i * size, size);
        v_sift(base, 0, i, size, compare, arg);
    }
}
struct v_compare_plain {
    int (*call)(const void *, const void *);
};
static int v_plain(const void *a, const void *b, void *arg) {
    return ((struct v_compare_plain *)arg)->call(a, b);
}
void v_freestanding_qsort(void *base, size_t n, size_t size,
                          int (*compare)(const void *, const void *)) {
    struct v_compare_plain cb = {compare};
    v_freestanding_qsort_r(base, n, size, v_plain, &cb);
}
