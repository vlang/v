#ifndef V_REALPATH_ALLOCATIONS_H
#define V_REALPATH_ALLOCATIONS_H
#ifdef REALPATH_TRACK_ALLOC
#include <stdlib.h>
#include <stddef.h>
/* Linker wrappers observe actual V scratch ownership without replacing the
 * allocator. The fixed table and callbacks never allocate themselves. */
void *__real_malloc(size_t);
void *__real_calloc(size_t, size_t);
void *__real_realloc(void *, size_t);
void __real_free(void *);
static void *realpath_allocations[128];
static int realpath_tracking;
static void realpath_record(void *p) {
    if (!realpath_tracking || !p) return;
    for (int i = 0; i < 128; ++i) {
        if (!realpath_allocations[i]) { realpath_allocations[i] = p; return; }
    }
    _Exit(94);
}
static void realpath_forget(void *p) {
    if (!realpath_tracking || !p) return;
    for (int i = 0; i < 128; ++i) {
        if (realpath_allocations[i] == p) { realpath_allocations[i] = NULL; return; }
    }
}
void *__wrap_malloc(size_t n) { void *p = __real_malloc(n); realpath_record(p); return p; }
void *__wrap_calloc(size_t n, size_t s) { void *p = __real_calloc(n, s); realpath_record(p); return p; }
void *__wrap_realloc(void *p, size_t n) {
    void *r = __real_realloc(p, n);
    if (r || !n) { realpath_forget(p); realpath_record(r); }
    return r;
}
void __wrap_free(void *p) { realpath_forget(p); __real_free(p); }
static void realpath_alloc_begin(void) { realpath_tracking = 1; }
static int realpath_alloc_live(void) {
    int n = 0;
    for (int i = 0; i < 128; ++i) if (realpath_allocations[i]) ++n;
    return n;
}
static void realpath_alloc_end(void) { realpath_tracking = 0; }
#endif
#endif
