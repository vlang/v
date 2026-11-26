#ifndef V_LINUX_BARE_LIBC_COMPAT_H
#define V_LINUX_BARE_LIBC_COMPAT_H

#ifndef _LARGEFILE64_SOURCE
#define _LARGEFILE64_SOURCE 1
#endif
#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <errno.h>
#include <stdint.h>
#include <stddef.h>
#include <sys/syscall.h>

/* Native C types and syscall numbers belong to the target C toolchain. These
 * adapters are for the Linux amd64/arm64 user-space runtime, never a hosted
 * build. Keep negative kernel errors until the public POSIX boundary. */
static inline long v_linux_raw(long number, uintptr_t a, uintptr_t b, uintptr_t c,
                               uintptr_t d, uintptr_t e, uintptr_t f) {
#if defined(__x86_64__)
    register uintptr_t r10 __asm__("r10") = d;
    register uintptr_t r8 __asm__("r8") = e;
    register uintptr_t r9 __asm__("r9") = f;
    long result;
    __asm__ volatile("syscall" : "=a"(result)
                     : "a"(number), "D"(a), "S"(b), "d"(c),
                       "r"(r10), "r"(r8), "r"(r9) : "rcx", "r11", "memory");
    return result;
#elif defined(__aarch64__)
    register uintptr_t x0 __asm__("x0") = a;
    register uintptr_t x1 __asm__("x1") = b;
    register uintptr_t x2 __asm__("x2") = c;
    register uintptr_t x3 __asm__("x3") = d;
    register uintptr_t x4 __asm__("x4") = e;
    register uintptr_t x5 __asm__("x5") = f;
    register long x8 __asm__("x8") = number;
    __asm__ volatile("svc #0" : "+r"(x0)
                     : "r"(x1), "r"(x2), "r"(x3), "r"(x4), "r"(x5), "r"(x8)
                     : "memory", "cc");
    return (long)x0;
#else
#error unsupported Linux freestanding C ABI
#endif
}

static inline long v_linux_result(long result) {
    if ((unsigned long)result >= (unsigned long)-4095) {
        errno = (int)-result;
        return -1;
    }
    return result;
}

#define V_RAW0(n) v_linux_raw((n), 0, 0, 0, 0, 0, 0)
#define V_RAW1(n,a) v_linux_raw((n), (uintptr_t)(a), 0, 0, 0, 0, 0)
#define V_RAW2(n,a,b) v_linux_raw((n), (uintptr_t)(a), (uintptr_t)(b), 0, 0, 0, 0)
#define V_RAW3(n,a,b,c) v_linux_raw((n), (uintptr_t)(a), (uintptr_t)(b), (uintptr_t)(c), 0, 0, 0)
#define V_RAW4(n,a,b,c,d) v_linux_raw((n), (uintptr_t)(a), (uintptr_t)(b), (uintptr_t)(c), (uintptr_t)(d), 0, 0)
#define V_RAW5(n,a,b,c,d,e) v_linux_raw((n), (uintptr_t)(a), (uintptr_t)(b), (uintptr_t)(c), (uintptr_t)(d), (uintptr_t)(e), 0)
#define V_RAW6(n,a,b,c,d,e,f) v_linux_raw((n), (uintptr_t)(a), (uintptr_t)(b), (uintptr_t)(c), (uintptr_t)(d), (uintptr_t)(e), (uintptr_t)(f))

/* The first word of our private stream/mutex storage is a futex. 0 is free,
 * 1 is held without known waiters, 2 is held with possible waiters. */
static inline void v_linux_lock(int *word) {
    int expected = 0;
    if (__atomic_compare_exchange_n(word, &expected, 1, 0, __ATOMIC_ACQUIRE,
                                     __ATOMIC_RELAXED)) return;
    while (__atomic_exchange_n(word, 2, __ATOMIC_ACQUIRE) != 0)
        (void)V_RAW6(SYS_futex, word, 128, 2, 0, 0, 0); /* FUTEX_WAIT_PRIVATE */
}

static inline void v_linux_unlock(int *word) {
    if (__atomic_exchange_n(word, 0, __ATOMIC_RELEASE) == 2)
        (void)V_RAW6(SYS_futex, word, 129, 1, 0, 0, 0); /* FUTEX_WAKE_PRIVATE */
}

#endif
