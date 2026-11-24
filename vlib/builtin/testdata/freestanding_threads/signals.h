/* Copyright (c) 2026 the V contributors. MIT license, see the root LICENSE. */
#ifndef V_FREESTANDING_THREAD_SIGNALS_H
#define V_FREESTANDING_THREAD_SIGNALS_H

#include <stddef.h>
#include <stdint.h>

#if !defined(__linux__) || (!defined(__x86_64__) && !defined(__aarch64__))
#error "this signal teardown fixture requires Linux amd64 or arm64"
#endif

/* This is the Linux kernel ABI, not libc's larger sigaction/sigset_t layout.
 * Both supported 64-bit targets use an eight-byte kernel signal mask. */
struct v_thread_kernel_sigaction {
    void (*handler)(int);
    unsigned long flags;
    void (*restorer)(void);
    unsigned long mask;
};
_Static_assert(sizeof(struct v_thread_kernel_sigaction) == 32, "kernel sigaction ABI");
_Static_assert(sizeof(unsigned long) == 8, "kernel signal mask ABI");

#if defined(__x86_64__)
enum {
    v_thread_nr_getpid = 39,
    v_thread_nr_gettid = 186,
    v_thread_nr_tgkill = 234,
    v_thread_nr_sigaction = 13,
    v_thread_nr_sigprocmask = 14
};

static inline long v_thread_signal_syscall(long number, long a, long b, long c, long d) {
    register long fourth __asm__("r10") = d;
    long result;
    __asm__ volatile("syscall" : "=a"(result)
                     : "a"(number), "D"(a), "S"(b), "d"(c), "r"(fourth)
                     : "rcx", "r11", "cc", "memory");
    return result;
}

/* A C prologue would move SP away from the kernel's signal frame. Define the
 * restorer entirely in assembly; a local symbol also avoids cross-unit clashes. */
void v_thread_signal_restore(void);
__asm__(".pushsection .text\n"
        ".local v_thread_signal_restore\n"
        ".type v_thread_signal_restore,@function\n"
        "v_thread_signal_restore:\n"
        "mov $15, %rax\n"
        "syscall\n"
        "ud2\n"
        ".size v_thread_signal_restore, .-v_thread_signal_restore\n"
        ".popsection\n");
#else
enum {
    v_thread_nr_getpid = 172,
    v_thread_nr_gettid = 178,
    v_thread_nr_tgkill = 131,
    v_thread_nr_sigaction = 134,
    v_thread_nr_sigprocmask = 135
};

static inline long v_thread_signal_syscall(long number, long a, long b, long c, long d) {
    register long x0 __asm__("x0") = a;
    register long x1 __asm__("x1") = b;
    register long x2 __asm__("x2") = c;
    register long x3 __asm__("x3") = d;
    register long x8 __asm__("x8") = number;
    __asm__ volatile("svc #0" : "+r"(x0)
                     : "r"(x1), "r"(x2), "r"(x3), "r"(x8)
                     : "cc", "memory");
    return x0;
}
#endif

enum { v_thread_signal_number = 10 }; /* SIGUSR1 on both targets. */
enum { v_thread_signal_initial_cookie = 0x19780309, v_thread_signal_active_cookie = 0x24681357 };
static _Thread_local volatile unsigned v_thread_signal_cookie = v_thread_signal_initial_cookie;
static _Thread_local unsigned v_thread_signal_received;
static unsigned v_thread_signal_total_hits;
static unsigned v_thread_signal_bad_cookie;
static long v_thread_signal_process;

static void v_thread_signal_handler(int signal_number) {
    if (signal_number != v_thread_signal_number ||
        v_thread_signal_cookie != v_thread_signal_active_cookie) {
        __atomic_store_n(&v_thread_signal_bad_cookie, 1, __ATOMIC_RELAXED);
    }
    __atomic_store_n(&v_thread_signal_received, 1, __ATOMIC_RELAXED);
    __atomic_fetch_add(&v_thread_signal_total_hits, 1, __ATOMIC_RELAXED);
}

/* Call once in the standalone test process, before creating workers. Unblock
 * SIGUSR1 explicitly so an inherited shell/runner mask cannot hide the race. */
static inline int v_thread_signal_install(void) {
    v_thread_signal_process = v_thread_signal_syscall(v_thread_nr_getpid, 0, 0, 0, 0);
    if (v_thread_signal_process <= 0) return -22;
    struct v_thread_kernel_sigaction action = {0};
    action.handler = v_thread_signal_handler;
#if defined(__x86_64__)
    action.flags = 0x04000000UL; /* SA_RESTORER */
    action.restorer = v_thread_signal_restore;
#endif
    /* AArch64 supplies its vDSO rt_sigreturn trampoline when no restorer is set. */
    long code = v_thread_signal_syscall(v_thread_nr_sigaction, v_thread_signal_number,
                                       (long)&action, 0, 8);
    if (code < 0) return (int)code;
    const unsigned long mask = 1UL << (v_thread_signal_number - 1);
    code = v_thread_signal_syscall(v_thread_nr_sigprocmask, 1 /* SIG_UNBLOCK */,
                                   (long)&mask, 0, 8);
    return (int)code;
}

/* Each fresh target calls this before publishing its TID to the sender. This
 * checks .tdata initialization; the receipt flag additionally exercises .tbss. */
static inline int v_thread_signal_prepare_target(void) {
    if (v_thread_signal_cookie != v_thread_signal_initial_cookie ||
        __atomic_load_n(&v_thread_signal_received, __ATOMIC_RELAXED) != 0) {
        __atomic_store_n(&v_thread_signal_bad_cookie, 1, __ATOMIC_RELAXED);
        return -22;
    }
    v_thread_signal_cookie = v_thread_signal_active_cookie;
    const long tid = v_thread_signal_syscall(v_thread_nr_gettid, 0, 0, 0, 0);
    return tid > 0 && tid <= 2147483647L ? (int)tid : -22;
}

/* The target must observe this before returning, ensuring a real handler ran
 * while its TLS/stack were live. The sender continues through its teardown. */
static inline int v_thread_signal_target_was_hit(void) {
    return __atomic_load_n(&v_thread_signal_received, __ATOMIC_RELAXED) != 0;
}

/* Return only once the target has exited, not when its V body announces that it
 * is done. Continuing until ESRCH covers the TLS/stack self-unmap interval.
 * The external process watchdog bounds the whole regression as well. */
static inline int v_thread_signal_hammer(int tid) {
    if (tid <= 0) return -22;
    for (unsigned attempt = 0; attempt < 1000000; ++attempt) {
        const long code = v_thread_signal_syscall(v_thread_nr_tgkill,
                                                 v_thread_signal_process, tid,
                                                 v_thread_signal_number, 0);
        if (code == -3) return 0; /* ESRCH: kernel has removed this thread. */
        if (code < 0 && code != -4) return (int)code;
    }
    return -110;
}

static inline unsigned v_thread_signal_hits(void) {
    return __atomic_load_n(&v_thread_signal_total_hits, __ATOMIC_RELAXED);
}

static inline int v_thread_signal_failed(void) {
    return __atomic_load_n(&v_thread_signal_bad_cookie, __ATOMIC_RELAXED) != 0;
}

#endif
