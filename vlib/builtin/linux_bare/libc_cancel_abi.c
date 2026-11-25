#include "libc_compat.h"
#include "libc_cancel_abi.h"
#include <setjmp.h>
#include <ucontext.h>
#include <unwind.h>

/* Native TLS storage, context registers, GCC unwind types, public function
 * signatures and the syscall instruction window. All cancellation policy is V. */
static __thread _Alignas(16) unsigned char v_cancel_initial[512];
uintptr_t v_cancel_implicit(void) { return (uintptr_t)v_cancel_initial; }
uintptr_t v_cancel_implicit_capacity(void) { return sizeof(v_cancel_initial); }
extern void v_cancel_signal(int32_t, uintptr_t, uintptr_t);
uintptr_t v_cancel_handler_address(void) { return (uintptr_t)v_cancel_signal; }
uintptr_t v_cancel_context_pc(uintptr_t input, uintptr_t replacement) {
    ucontext_t *context = (void *)input;
#if defined(__x86_64__)
    uintptr_t old = context->uc_mcontext.gregs[REG_RIP];
    if (replacement) context->uc_mcontext.gregs[REG_RIP] = replacement;
#else
    uintptr_t old = context->uc_mcontext.pc;
    if (replacement) context->uc_mcontext.pc = replacement;
#endif
    return old;
}
uintptr_t v_cancel_jump_sp(uintptr_t target) {
    const uintptr_t *registers = (const void *)target;
#if defined(__x86_64__)
    return registers[6];
#else
    return registers[13];
#endif
}
#if defined(__GLIBC__)
uintptr_t v_cancel_buffer_get(uintptr_t target, uint32_t index) {
    return (uintptr_t)((__pthread_unwind_buf_t *)target)->__pad[index];
}
void v_cancel_buffer_set(uintptr_t target, uint32_t index, uintptr_t value) {
    ((__pthread_unwind_buf_t *)target)->__pad[index] = (void *)value;
}
#else
/* GNU cancellation buffers are not part of a non-GNU native ABI. */
uintptr_t v_cancel_buffer_get(uintptr_t target, uint32_t index) { (void)target; (void)index; return 0; }
void v_cancel_buffer_set(uintptr_t target, uint32_t index, uintptr_t value) { (void)target; (void)index; (void)value; }
#endif

#pragma weak _Unwind_ForcedUnwind
#pragma weak _Unwind_GetCFA
static __thread struct _Unwind_Exception v_cancel_exception;
extern int32_t v_cancel_unwind_stop(int32_t, uintptr_t, uintptr_t);
extern _Noreturn void v_cancel_unwind_discard(void);
static _Unwind_Reason_Code v_cancel_stop(int version, _Unwind_Action actions,
    _Unwind_Exception_Class kind, struct _Unwind_Exception *exception,
    struct _Unwind_Context *context, void *parameter) {
    (void)version; (void)kind; (void)exception;
    return (_Unwind_Reason_Code)v_cancel_unwind_stop((int32_t)actions,
        (uintptr_t)_Unwind_GetCFA(context), (uintptr_t)parameter);
}
static void v_cancel_exception_cleanup(_Unwind_Reason_Code reason, struct _Unwind_Exception *exception) {
    (void)reason; (void)exception; v_cancel_unwind_discard();
}
int32_t v_cancel_unwinder_available(void) { return _Unwind_ForcedUnwind && _Unwind_GetCFA; }
void v_cancel_force_unwind(uintptr_t parameter) {
    v_cancel_exception = (struct _Unwind_Exception){.exception_class = 0,
        .exception_cleanup = v_cancel_exception_cleanup};
    _Unwind_ForcedUnwind(&v_cancel_exception, v_cancel_stop, (void *)parameter);
}

/* A request between the pending check and the kernel entry must redirect
 * execution away from that entry. A blocked syscall instead returns EINTR.
 * Both paths return to V before executing thread cleanup. */
extern const unsigned char v_cancel_cp_begin[], v_cancel_cp_end[], v_cancel_cp_cancel[];
void v_cancel_window(uintptr_t output) {
    uintptr_t *v = (void *)output;
    v[0] = (uintptr_t)v_cancel_cp_begin; v[1] = (uintptr_t)v_cancel_cp_end;
    v[2] = (uintptr_t)v_cancel_cp_cancel;
}
#if defined(__x86_64__)
__asm__(".pushsection .text\n.global v_cancel_asm\n.type v_cancel_asm,@function\n"
        "v_cancel_asm:\n.cfi_startproc\nendbr64\n"
        ".global v_cancel_cp_begin\nv_cancel_cp_begin:\n"
        "mov (%rdi),%eax\nand $11,%eax\ncmp $2,%eax\nje v_cancel_cp_cancel\n"
        "mov %rsi,%rax\nmov %rdx,%rdi\nmov %rcx,%rsi\nmov %r8,%rdx\n"
        "mov %r9,%r10\nmov 8(%rsp),%r8\nmov 16(%rsp),%r9\nsyscall\n"
        ".global v_cancel_cp_end\nv_cancel_cp_end:\nret\n"
        ".global v_cancel_cp_cancel\nv_cancel_cp_cancel:\nendbr64\nmov $-125,%rax\nret\n"
        ".cfi_endproc\n.size v_cancel_asm,.-v_cancel_asm\n.popsection\n");
#elif defined(__aarch64__)
__asm__(".pushsection .text\n.global v_cancel_asm\n.type v_cancel_asm,%function\n"
        "v_cancel_asm:\n.cfi_startproc\nbti c\n"
        ".global v_cancel_cp_begin\nv_cancel_cp_begin:\n"
        "ldr w9,[x0]\nmov w10,#11\nand w9,w9,w10\ncmp w9,#2\nb.eq v_cancel_cp_cancel\n"
        "mov x8,x1\nmov x0,x2\nmov x1,x3\nmov x2,x4\nmov x3,x5\n"
        "mov x4,x6\nmov x5,x7\nsvc #0\n"
        ".global v_cancel_cp_end\nv_cancel_cp_end:\nret\n"
        ".global v_cancel_cp_cancel\nv_cancel_cp_cancel:\nbti c\nmov x0,#-125\nret\n"
        ".cfi_endproc\n.size v_cancel_asm,.-v_cancel_asm\n.popsection\n");
#endif

extern int32_t v_pthread_cancel(uintptr_t);
extern int32_t v_pthread_setcancelstate(int32_t, uintptr_t);
extern int32_t v_pthread_setcanceltype(int32_t, uintptr_t);
extern void v_pthread_testcancel(void);
int pthread_cancel(pthread_t target) { return v_pthread_cancel((uintptr_t)target); }
int pthread_setcancelstate(int state, int *previous) { return v_pthread_setcancelstate(state, (uintptr_t)previous); }
int pthread_setcanceltype(int type, int *previous) { return v_pthread_setcanceltype(type, (uintptr_t)previous); }
void pthread_testcancel(void) { v_pthread_testcancel(); }
#if defined(__GLIBC__)
extern void v_cancel_register(uintptr_t, int32_t);
extern void v_cancel_unregister(uintptr_t, int32_t);
extern _Noreturn void v_cancel_unwind_next(uintptr_t);
void __pthread_register_cancel(__pthread_unwind_buf_t *b) { v_cancel_register((uintptr_t)b, 0); }
void __pthread_unregister_cancel(__pthread_unwind_buf_t *b) { v_cancel_unregister((uintptr_t)b, 0); }
void __pthread_register_cancel_defer(__pthread_unwind_buf_t *b) { v_cancel_register((uintptr_t)b, 1); }
void __pthread_unregister_cancel_restore(__pthread_unwind_buf_t *b) { v_cancel_unregister((uintptr_t)b, 1); }
_Noreturn void __pthread_unwind_next(__pthread_unwind_buf_t *b) { v_cancel_unwind_next((uintptr_t)b); }
#endif
