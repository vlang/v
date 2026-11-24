/* Linux test-only linker interposition around standard public collector APIs.
 * No production fault flag or private collector implementation is involved. */
#include "wrap.h"
#include <gc.h>

static int fault;
static int atomic_calls;
static int registration_calls;
static int free_calls;

void *__real_GC_malloc_atomic(size_t);
int __real_GC_general_register_disappearing_link(void **, const void *);
void __real_GC_free(void *);

void weak_fault_begin(int kind) {
    atomic_calls = registration_calls = free_calls = 0;
    fault = kind;
}
void weak_fault_end(void) { fault = 0; }
int weak_fault_atomic_calls(void) { return atomic_calls; }
int weak_fault_registration_calls(void) { return registration_calls; }
int weak_fault_free_calls(void) { return free_calls; }

void *__wrap_GC_malloc_atomic(size_t size) {
    if (fault) {
        ++atomic_calls;
        if (fault == 1) return NULL;
    }
    return __real_GC_malloc_atomic(size);
}

int __wrap_GC_general_register_disappearing_link(void **link, const void *object) {
    if (fault) {
        ++registration_calls;
        return fault == 3 ? GC_UNIMPLEMENTED : GC_NO_MEMORY;
    }
    return __real_GC_general_register_disappearing_link(link, object);
}

void __wrap_GC_free(void *object) {
    if (fault) ++free_calls;
    __real_GC_free(object);
}
