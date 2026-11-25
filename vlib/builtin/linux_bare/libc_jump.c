/* Copyright 2011-2012 Nicholas J. Kain, MIT license (COPYRIGHT.musl).
 * Register saves follow musl's amd64/arm64 assembly (COPYRIGHT.musl), with
 * signal-mask storage taken from the native target headers. No host TLS or
 * libc pointer mangling is involved in a standalone bare executable. */
#include "libc_compat.h"
#include <setjmp.h>
#include <signal.h>
#undef setjmp
#if defined(__GLIBC__)
#define V_JUMP_MASK_SAVED(e) ((e)[0].__mask_was_saved)
#define V_JUMP_MASK(e) (&(e)[0].__saved_mask)
#define V_JUMP_REGISTERS(e) ((e)[0].__jmpbuf)
#else
#define V_JUMP_MASK_SAVED(e) ((e)[0].__fl)
#define V_JUMP_MASK(e) (&(e)[0].__ss)
#define V_JUMP_REGISTERS(e) ((e)[0].__jb)
#endif
#if defined(__x86_64__)
_Static_assert(sizeof V_JUMP_REGISTERS((jmp_buf){0}) == 64, "amd64 jmp registers");
#elif defined(__aarch64__)
_Static_assert(sizeof V_JUMP_REGISTERS((jmp_buf){0}) == 176, "arm64 jmp registers");
#endif
int v_freestanding_save_jump_mask(jmp_buf env, int save) {
    V_JUMP_MASK_SAVED(env) = 0;
    if (save && V_RAW4(SYS_rt_sigprocmask, SIG_BLOCK, NULL, V_JUMP_MASK(env), 8) >= 0)
        V_JUMP_MASK_SAVED(env) = 1;
    return 0;
}
_Noreturn void v_freestanding_restore_jump(jmp_buf, int);
_Noreturn void longjmp(jmp_buf env, int result) {
    if (V_JUMP_MASK_SAVED(env))
        V_RAW4(SYS_rt_sigprocmask, SIG_SETMASK, V_JUMP_MASK(env), NULL, 8);
    v_freestanding_restore_jump(env, result);
}
extern __typeof__(longjmp) _longjmp __attribute__((alias("longjmp")));
extern __typeof__(siglongjmp) siglongjmp __attribute__((alias("longjmp")));
#if defined(__x86_64__)
__asm__(".pushsection .text\n"
        ".global setjmp\n.global _setjmp\n.global __sigsetjmp\n.global sigsetjmp\n"
        ".type setjmp,@function\n.type _setjmp,@function\n.type __sigsetjmp,@function\n"
        "setjmp:\n_setjmp:\nxor %esi,%esi\njmp __sigsetjmp\n"
        "sigsetjmp:\n__sigsetjmp:\n"
        "mov %rbx,0(%rdi)\nmov %rbp,8(%rdi)\nmov %r12,16(%rdi)\nmov %r13,24(%rdi)\nmov "
        "%r14,32(%rdi)\nmov %r15,40(%rdi)\n"
        "lea 8(%rsp),%rdx\nmov %rdx,48(%rdi)\nmov (%rsp),%rdx\nmov %rdx,56(%rdi)\n"
        "jmp v_freestanding_save_jump_mask\n"
        ".global v_freestanding_restore_jump\n.type v_freestanding_restore_jump,@function\n"
        "v_freestanding_restore_jump:\nxor %eax,%eax\ncmp $1,%esi\nadc %esi,%eax\n"
        "mov 0(%rdi),%rbx\nmov 8(%rdi),%rbp\nmov 16(%rdi),%r12\nmov 24(%rdi),%r13\nmov "
        "32(%rdi),%r14\nmov 40(%rdi),%r15\n"
        "mov 48(%rdi),%rsp\njmp *56(%rdi)\n.popsection\n");
#elif defined(__aarch64__)
__asm__(".pushsection .text\n"
        ".global setjmp\n.global _setjmp\n.global __sigsetjmp\n.global sigsetjmp\n"
        ".type setjmp,%function\n.type _setjmp,%function\n.type __sigsetjmp,%function\n"
        "setjmp:\n_setjmp:\nmov w1,0\nb __sigsetjmp\n"
        "sigsetjmp:\n__sigsetjmp:\n"
        "stp x19,x20,[x0,0]\nstp x21,x22,[x0,16]\nstp x23,x24,[x0,32]\nstp x25,x26,[x0,48]\nstp "
        "x27,x28,[x0,64]\nstp x29,x30,[x0,80]\n"
        "mov x2,sp\nstr x2,[x0,104]\nstp d8,d9,[x0,112]\nstp d10,d11,[x0,128]\nstp "
        "d12,d13,[x0,144]\nstp d14,d15,[x0,160]\n"
        "b v_freestanding_save_jump_mask\n"
        ".global v_freestanding_restore_jump\n.type v_freestanding_restore_jump,%function\n"
        "v_freestanding_restore_jump:\n"
        "ldp x19,x20,[x0,0]\nldp x21,x22,[x0,16]\nldp x23,x24,[x0,32]\nldp x25,x26,[x0,48]\nldp "
        "x27,x28,[x0,64]\nldp x29,x30,[x0,80]\n"
        "ldr x2,[x0,104]\nmov sp,x2\nldp d8,d9,[x0,112]\nldp d10,d11,[x0,128]\nldp "
        "d12,d13,[x0,144]\nldp d14,d15,[x0,160]\n"
        "cmp w1,0\ncsinc w0,w1,wzr,ne\nbr x30\n.popsection\n");
#endif
