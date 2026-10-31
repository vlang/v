/* V's fence must not overlap with libtcc1.a's atomic runtime symbols. */
        .text
        .global _V_atomic_thread_fence
        .type   _V_atomic_thread_fence, %function
_V_atomic_thread_fence:
        lock orq $0x0,(%rsp)
        ret
