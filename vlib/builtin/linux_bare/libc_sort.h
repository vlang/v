#ifndef V_LINUX_BARE_SORT_H
#define V_LINUX_BARE_SORT_H
#include <stddef.h>
void v_freestanding_qsort(void *, size_t, size_t, int (*)(const void *, const void *));
void v_freestanding_qsort_r(void *, size_t, size_t,
                           int (*)(const void *, const void *, void *), void *);
#endif
