/* ---------------------------------------------- */
#if defined __i386__

#define endbr32

        .text

        .global _V_atomic_thread_fence
        .type   _V_atomic_thread_fence, %function
_V_atomic_thread_fence:
        endbr32
        lock orl $0x0,(%esp)
        ret

#endif //__i386__

/* ---------------------------------------------- */
#if defined __x86_64__ && !defined _WIN32
#define endbr64
        .text
        .global __atomic_thread_fence
        .type   __atomic_thread_fence, %function
__atomic_thread_fence:
        endbr64
        lock orq $0x0,(%rsp)
        ret
#endif //__x86_64__ && !_WIN32

/* ---------------------------------------------- */
#if defined __x86_64__ && defined _WIN32
        .text

        .global _V_atomic_thread_fence
        .type   _V_atomic_thread_fence, %function
_V_atomic_thread_fence:
        lock orq $0x0,(%rsp)
        ret

#endif //__x86_64__ && _WIN32

/* ---------------------------------------------- */
#if defined __arm__
        .text
#ifndef __TINYC__
        .arch   armv6k
        .syntax unified
#endif

        .global _V_atomic_thread_fence
        .type   _V_atomic_thread_fence, %function
_V_atomic_thread_fence:
#ifdef __TINYC__
        .int 0xee070fba
        .int 0xe12fff1e
#else
        mcr     p15, #0, r0, c7, c10, #5
        bx  lr

#endif
#endif //__arm__

/* ---------------------------------------------- */
#if defined __aarch64__
        .text

        .global _V_atomic_thread_fence
        .type   _V_atomic_thread_fence, %function
_V_atomic_thread_fence:
#ifdef __TINYC__
        .int 0xd5033bbf
        .int 0xd65f03c0
#else
        dmb ish
        ret

#endif
#endif //__aarch64__

/* ---------------------------------------------- */
#if defined __riscv
        .text

        .global _V_atomic_thread_fence
        .type   _V_atomic_thread_fence, %function
_V_atomic_thread_fence:
#ifdef __TINYC__
        .int 0x0330000f
        .short 0x8082
#else
        fence   rw,rw
        ret

#endif
#endif //__riscv
