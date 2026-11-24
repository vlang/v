/* Test-only finalizer observation. Target addresses never reach the collector
 * thread. The V creating/checking workers exit before collection. */
#include "probe.h"
#include <gc.h>
#include <stdint.h>
#include <stdatomic.h>

static void *volatile strong_root;
static _Atomic int finalized;
static uint64_t non_gc;

static void target_finalizer(void *object, void *data) {
    (void)object;
    (void)data;
    atomic_fetch_add_explicit(&finalized, 1, memory_order_relaxed);
}

void weak_test_initialize(void) { GC_set_finalize_on_demand(1); }

int weak_test_track(void *object) {
    if (GC_base(object) != object) return 1;
    GC_register_finalizer_no_order(object, target_finalizer, NULL, NULL, NULL);
    return 0;
}

void weak_test_keep_strong(void *object) { strong_root = object; }
void weak_test_clear_strong(void) { strong_root = NULL; }
void *weak_test_non_gc(void) { return &non_gc; }

__attribute__((noinline)) static void scrub_and_collect(void) {
    volatile uintptr_t cleared[8192];
    for (unsigned i = 0; i < sizeof cleared / sizeof cleared[0]; ++i)
        cleared[i] = 0;
    GC_gcollect();
    GC_invoke_finalizers();
}

int weak_test_collect(void) {
    for (int i = 0; i < 20; ++i) scrub_and_collect();
    return atomic_load_explicit(&finalized, memory_order_relaxed);
}
