// Freestanding panic frames do not save signal masks. They only return to a
// live V defer in the same thread. Keep the buffer and both operations private
// to this translation unit so split module builds can emit the same preamble.
#ifndef V_FREESTANDING_PANIC_JUMP_H
#define V_FREESTANDING_PANIC_JUMP_H

#if !defined(__GNUC__) || defined(__TINYC__)
#error "freestanding panic recovery requires GCC or Clang"
#elif defined(__x86_64__)
// GCC and Clang implement these nonlocal-goto builtins inline on x86-64.
// Five pointer-sized slots are part of their interface. Both sides must use
// the compiler builtins; their buffer layout differs from a libc jmp_buf.
typedef void *v_bare_unwind_buf[5];
#define v_unwind_setjmp(buf) __builtin_setjmp(buf)
#define v_unwind_longjmp(buf) __builtin_longjmp(buf, 1)
#elif defined(__aarch64__)
// Clang does not implement __builtin_setjmp on AArch64. Save the AAPCS64
// callee-saved integer registers, frame and return address, stack pointer,
// and the preserved low halves of v8-v15. x18 belongs to the platform.
typedef unsigned long long v_bare_unwind_buf[21];
#if defined(__APPLE__)
#define V_BARE_SAVE "Lv_bare_panic_save"
#define V_BARE_RESTORE "Lv_bare_panic_restore"
#else
#define V_BARE_SAVE ".Lv_bare_panic_save"
#define V_BARE_RESTORE ".Lv_bare_panic_restore"
#endif
extern int v_bare_panic_save(v_bare_unwind_buf) __asm__(V_BARE_SAVE)
    __attribute__((returns_twice));
extern void v_bare_panic_restore(v_bare_unwind_buf) __asm__(V_BARE_RESTORE)
    __attribute__((noreturn));

__asm__(".text\n.p2align 2\n"
    V_BARE_SAVE ":\n"
    "stp x19, x20, [x0, #0]\n"
    "stp x21, x22, [x0, #16]\n"
    "stp x23, x24, [x0, #32]\n"
    "stp x25, x26, [x0, #48]\n"
    "stp x27, x28, [x0, #64]\n"
    "stp x29, x30, [x0, #80]\n"
    "mov x9, sp\n"
    "str x9, [x0, #96]\n"
    "stp d8, d9, [x0, #104]\n"
    "stp d10, d11, [x0, #120]\n"
    "stp d12, d13, [x0, #136]\n"
    "stp d14, d15, [x0, #152]\n"
    "mov w0, #0\n"
    "ret\n"
    V_BARE_RESTORE ":\n"
    "ldp x19, x20, [x0, #0]\n"
    "ldp x21, x22, [x0, #16]\n"
    "ldp x23, x24, [x0, #32]\n"
    "ldp x25, x26, [x0, #48]\n"
    "ldp x27, x28, [x0, #64]\n"
    "ldp x29, x30, [x0, #80]\n"
    "ldr x9, [x0, #96]\n"
    "ldp d8, d9, [x0, #104]\n"
    "ldp d10, d11, [x0, #120]\n"
    "ldp d12, d13, [x0, #136]\n"
    "ldp d14, d15, [x0, #152]\n"
    "mov sp, x9\n"
    "mov w0, #1\n"
    "ret\n");

#define v_unwind_setjmp(buf) v_bare_panic_save(buf)
#define v_unwind_longjmp(buf) v_bare_panic_restore(buf)
#undef V_BARE_SAVE
#undef V_BARE_RESTORE
#else
#error "unsupported architecture for freestanding panic recovery"
#endif
#endif
