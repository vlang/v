#ifndef VGC_STACK_ROOTS_TEST_H
#define VGC_STACK_ROOTS_TEST_H

#include "vgc_scan.h"

// Keep the root in a C local so V's automatic heap promotion cannot move it.
// Under ASan, give the visitor only a reference to the fake frame; discovering
// the object then requires visiting that frame, not just shading the reference.
static int vgc_test_visit_stack_root(void* object, int (*visit)(uintptr_t, uintptr_t)) {
    volatile uintptr_t root = (uintptr_t)object;
    uintptr_t reference = (uintptr_t)&root;
    void* fake_stack = vgc_current_fake_stack();
    if (vgc_real_stack_address(fake_stack, reference) != reference) {
        return visit((uintptr_t)&reference, (uintptr_t)&reference + sizeof(reference));
    }
    return visit((uintptr_t)&root, (uintptr_t)&root + sizeof(root));
}

#endif
