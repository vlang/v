#include <sys/resource.h>
#include <sys/syscall.h>
#include "limits.h"

_Static_assert(sizeof(struct rlimit) == 16, "fixture needs the LP64 rlimit ABI");
static struct rlimit saved_limit;

/* Independent SDK ABI oracle; this test must also link without libc. */
static long kernel_limit(const struct rlimit *next, struct rlimit *previous) {
#if defined(__x86_64__)
    register long fourth __asm__("r10") = (long)previous;
    long result;
    __asm__ volatile("syscall" : "=a"(result)
                     : "a"((long)SYS_prlimit64), "D"(0L), "S"((long)RLIMIT_AS),
                       "d"((long)next), "r"(fourth)
                     : "rcx", "r11", "memory");
    return result;
#elif defined(__aarch64__)
    register long x0 __asm__("x0") = 0;
    register long x1 __asm__("x1") = RLIMIT_AS;
    register long x2 __asm__("x2") = (long)next;
    register long x3 __asm__("x3") = (long)previous;
    register long x8 __asm__("x8") = SYS_prlimit64;
    __asm__ volatile("svc 0" : "+r"(x0) : "r"(x1), "r"(x2), "r"(x3), "r"(x8) : "memory");
    return x0;
#else
#error Unsupported fixture architecture
#endif
}

int environment_fail_allocations(void) {
    if (kernel_limit(0, &saved_limit) != 0) return -1;
    struct rlimit limited = saved_limit;
    limited.rlim_cur = 0;
    return (int)kernel_limit(&limited, 0);
}
int environment_restore_allocations(void) { return (int)kernel_limit(&saved_limit, 0); }
