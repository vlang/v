#ifndef VGC_STACK_ROOTS_TEST_H
#define VGC_STACK_ROOTS_TEST_H

#include "vgc_scan.h"
#include "vgc_platform.h"

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

#if defined(__linux__) && defined(__x86_64__) && !defined(__TINYC__)
struct vgc_test_register_context {
    int (*visit)(uintptr_t, uintptr_t);
    int result;
};

static void vgc_test_visit_register_window(void* arg, void* regs) {
    struct vgc_test_register_context* context = (struct vgc_test_register_context*)arg;
#ifdef VGC_SCAN_MSAN
    if (__msan_test_shadow(regs, sizeof(jmp_buf)) != -1) {
        context->result = 0;
        return;
    }
#endif
    context->result = context->visit((uintptr_t)regs, (uintptr_t)regs + sizeof(jmp_buf));
}

// Keep the only scanned copy in callee-saved registers. The visitor sees only
// the production spill buffer, so another stack/global copy cannot hide a miss.
static int vgc_test_visit_register_root(void* object, int (*visit)(uintptr_t, uintptr_t)) {
    register uintptr_t root12 __asm__("r12") = (uintptr_t)object;
    register uintptr_t root13 __asm__("r13") = (uintptr_t)object;
    register uintptr_t root14 __asm__("r14") = (uintptr_t)object;
    register uintptr_t root15 __asm__("r15") = (uintptr_t)object;
    struct vgc_test_register_context context = { visit, 0 };
    // Keep the ordinary helper ABI instead of specializing it for this visitor.
    void (* volatile spill)(vgc_spill_fn, void*) = vgc_call_with_registers_spilled;
    __asm__ volatile("" : "+r"(root12), "+r"(root13), "+r"(root14), "+r"(root15) : : "memory");
    spill(vgc_test_visit_register_window, &context);
    __asm__ volatile("" : "+r"(root12), "+r"(root13), "+r"(root14), "+r"(root15) : : "memory");
    return context.result;
}
#endif

#endif
