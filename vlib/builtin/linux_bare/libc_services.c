#include "libc_compat.h"
#include "libc_aligned_alloc.h"
#include <fcntl.h>
#include <limits.h>
#include <pthread.h>
#include <sched.h>
#include <signal.h>
#include <string.h>
#include <sys/auxv.h>
#include <sys/resource.h>
#include <sys/sysinfo.h>
#include <time.h>
#include <unistd.h>

char *program_invocation_name;
char *program_invocation_short_name;
const uintptr_t *v_freestanding_auxiliary_vector;

void v_freestanding_program_names_set(uintptr_t full, uintptr_t short_name, uintptr_t auxiliary) {
    program_invocation_name = (char *)full;
    program_invocation_short_name = (char *)short_name;
    v_freestanding_auxiliary_vector = (const uintptr_t *)auxiliary;
}
uintptr_t v_freestanding_program_short_name(void) { return (uintptr_t)program_invocation_short_name; }
uintptr_t v_freestanding_program_auxiliary(void) { return (uintptr_t)v_freestanding_auxiliary_vector; }

char *strerror(int code) {
    switch (code) {
    case 0: return "Success";
#define E(number, message) case number: return message;
#include "libc_errno_messages.h"
#undef E
    default: return "Unknown error";
    }
}

int posix_memalign(void **pointer, size_t alignment, size_t size) {
    if (alignment < sizeof(void *) || (alignment & (alignment - 1))) return EINVAL;
    int saved_errno = errno;
    void *result = v_freestanding_aligned_alloc(alignment, size);
    errno = saved_errno;
    if (!result) return ENOMEM;
    *pointer = result;
    return 0;
}

/* A variadic syscall entry must capture machine argument registers directly:
 * reading six va_args in C is undefined for calls that supply fewer arguments.
 * Linux ignores argument registers not used by the selected syscall. */
#if defined(__x86_64__)
__asm__(".text\n.global syscall\n.type syscall,@function\nsyscall:\n"
        "mov %rdi,%rax\nmov %rsi,%rdi\nmov %rdx,%rsi\nmov %rcx,%rdx\n"
        "mov %r8,%r10\nmov %r9,%r8\nmov 8(%rsp),%r9\nsyscall\n"
        "cmp $-4095,%rax\njae 1f\nret\n1:\npush %rax\n"
        "call __errno_location\npop %rdx\nneg %edx\nmov %edx,(%rax)\n"
        "mov $-1,%rax\nret\n.size syscall,.-syscall\n");
#elif defined(__aarch64__)
__asm__(".text\n.global syscall\n.type syscall,%function\nsyscall:\n"
        "mov x8,x0\nmov x0,x1\nmov x1,x2\nmov x2,x3\nmov x3,x4\n"
        "mov x4,x5\nmov x5,x6\nsvc #0\ncmn x0,#4095\nb.cs 1f\nret\n"
        "1:\nneg x0,x0\nstp x0,x30,[sp,#-16]!\nbl __errno_location\n"
        "ldp x1,x30,[sp],#16\nstr w1,[x0]\nmov x0,#-1\nret\n"
        ".size syscall,.-syscall\n");
#endif

extern int64_t v_native_scheduler(int32_t, int32_t, int32_t, uintptr_t, uintptr_t);
int sched_getscheduler(pid_t pid) { return (int)v_native_scheduler(0, pid, 0, 0, 0); }
int sched_setscheduler(pid_t pid, int policy, const struct sched_param *p) { return (int)v_native_scheduler(1, pid, policy, (uintptr_t)p, 0); }
int sched_getparam(pid_t pid, struct sched_param *p) { return (int)v_native_scheduler(2, pid, 0, (uintptr_t)p, 0); }
int sched_setparam(pid_t pid, const struct sched_param *p) { return (int)v_native_scheduler(3, pid, 0, (uintptr_t)p, 0); }
int sched_get_priority_min(int policy) { return (int)v_native_scheduler(4, policy, 0, 0, 0); }
int sched_get_priority_max(int policy) { return (int)v_native_scheduler(5, policy, 0, 0, 0); }
int sched_rr_get_interval(pid_t pid, struct timespec *time) { return (int)v_native_scheduler(6, pid, 0, (uintptr_t)time, 0); }
int sched_getaffinity(pid_t pid, size_t size, cpu_set_t *set) { return (int)v_native_scheduler(7, pid, 0, (uintptr_t)set, size); }
int sched_setaffinity(pid_t pid, size_t size, const cpu_set_t *set) { return (int)v_native_scheduler(8, pid, 0, (uintptr_t)set, size); }
extern int32_t v_native_priority(int32_t, uint32_t, int32_t, int32_t);
extern int32_t v_native_nice(int32_t);
#if defined(__GLIBC__)
typedef __priority_which_t v_priority_which;
#else
typedef int v_priority_which;
#endif
int getpriority(v_priority_which which, id_t who) { return v_native_priority(which, who, 0, 0); }
int setpriority(v_priority_which which, id_t who, int priority) { return v_native_priority(which, who, priority, 1); }
int nice(int increment) { return v_native_nice(increment); }

/* glibc's CPU_COUNT[_S] macros call this ABI helper even in unoptimized C. */
int __sched_cpucount(size_t size, const cpu_set_t *set) {
    const unsigned char *bytes = (const unsigned char *)set;
    int count = 0;
    for (size_t i = 0; i < size; ++i) {
        unsigned char value = bytes[i];
        while (value) { value &= (unsigned char)(value - 1); ++count; }
    }
    return count;
}

int sigemptyset(sigset_t *set) { memset(set, 0, sizeof *set); return 0; }
int sigfillset(sigset_t *set) { memset(set, 255, sizeof *set); return 0; }
static int v_signal_member(sigset_t *set, int number, int operation) {
    if (number <= 0 || number >= 65) { errno = EINVAL; return -1; }
    uint64_t bits;
    memcpy(&bits, set, sizeof bits);
    uint64_t mask = UINT64_C(1) << (number - 1);
    if (!operation) return !!(bits & mask);
    if (operation > 0) bits |= mask; else bits &= ~mask;
    memcpy(set, &bits, sizeof bits);
    return 0;
}
int sigaddset(sigset_t *set, int number) { return v_signal_member(set, number, 1); }
int sigdelset(sigset_t *set, int number) { return v_signal_member(set, number, -1); }
int sigismember(const sigset_t *set, int number) { return v_signal_member((sigset_t *)set, number, 0); }
int pthread_sigmask(int how, const sigset_t *set, sigset_t *previous) {
    uint64_t input, output;
    if (set) { memcpy(&input, set, sizeof input); input &= ~(UINT64_C(1) << 31); }
    long result = V_RAW4(SYS_rt_sigprocmask, how, set ? &input : NULL, previous ? &output : NULL, sizeof input);
    if (result < 0) return (int)-result;
    if (previous) { memset(previous, 0, sizeof *previous); memcpy(previous, &output, sizeof output); }
    return 0;
}
int sigprocmask(int how, const sigset_t *set, sigset_t *previous) {
    int result = pthread_sigmask(how, set, previous);
    if (!result) return 0;
    errno = result;
    return -1;
}

static long v_auxiliary_value(unsigned long key) {
    int fd = open("/proc/self/auxv", O_RDONLY | O_CLOEXEC);
    if (fd < 0) return -1;
    unsigned long pair[2];
    long value = -1;
    for (;;) {
        size_t done = 0;
        while (done < sizeof pair) {
            long count = V_RAW3(SYS_read, fd, (unsigned char *)pair + done, sizeof pair - done);
            if (count == -EINTR) continue;
            if (count <= 0) break;
            done += (size_t)count;
        }
        if (done != sizeof pair || !pair[0]) break;
        if (pair[0] == key) { value = (long)pair[1]; break; }
    }
    v_linux_result(V_RAW1(SYS_close, fd));
    if (value < 0) errno = ENOSYS;
    return value;
}

static long v_cpu_count(const char *path) {
    int fd = open(path, O_RDONLY | O_CLOEXEC);
    if (fd < 0) return -1;
    char buffer[4096];
    long size;
    do { size = V_RAW3(SYS_read, fd, buffer, sizeof buffer - 1); } while (size == -EINTR);
    v_linux_result(V_RAW1(SYS_close, fd));
    if (size <= 0 || size == (long)sizeof buffer - 1) { errno = ENOSYS; return -1; }
    buffer[size] = 0;
    long total = 0;
    const char *p = buffer;
    while (*p >= '0' && *p <= '9') {
        long first = 0, last;
        do {
            if (first > (INT_MAX - 9) / 10) { errno = EOVERFLOW; return -1; }
            first = first * 10 + (*p++ - '0');
        } while (*p >= '0' && *p <= '9');
        last = first;
        if (*p == '-') {
            ++p;
            if (*p < '0' || *p > '9') { errno = EINVAL; return -1; }
            last = 0;
            do {
                if (last > (INT_MAX - 9) / 10) { errno = EOVERFLOW; return -1; }
                last = last * 10 + (*p++ - '0');
            } while (*p >= '0' && *p <= '9');
        }
        if (last < first || total > INT_MAX - (last - first + 1)) { errno = EOVERFLOW; return -1; }
        total += last - first + 1;
        if (*p != ',') break;
        ++p;
    }
    return total > 0 ? total : -1;
}

#ifdef _SC_MINSIGSTKSZ
/* GNU signal.h exposes sysconf calls in place of the legacy ABI constants. */
#if defined(__aarch64__)
enum { v_signal_stack_floor = 5120 };
#else
enum { v_signal_stack_floor = 2048 };
#endif

static long v_signal_stack_minimum(void) {
    unsigned long size = getauxval(AT_MINSIGSTKSZ);
    if (size) return (long)size;
#if defined(__x86_64__)
    /* Older kernels omit the auxv entry. Account for the signal frame, its
     * alignment, the enabled XSAVE area, and the trailing 32-bit marker. */
    unsigned int a, b, c, d;
    __asm__("cpuid" : "=a"(a), "=b"(b), "=c"(c), "=d"(d) : "0"(0), "2"(0));
    if (a >= 13) {
        __asm__("cpuid" : "=a"(a), "=b"(b), "=c"(c), "=d"(d) : "0"(1), "2"(0));
        if (c & (1U << 27)) {
            __asm__("cpuid" : "=a"(a), "=b"(b), "=c"(c), "=d"(d) : "0"(13), "2"(0));
            return 512L + b + sizeof(uint32_t);
        }
    }
#endif
    return v_signal_stack_floor;
}
#endif

long sysconf(int name) {
    int saved_errno = errno;
    long result;
    switch (name) {
    case _SC_PAGESIZE: result = v_auxiliary_value(6); break; /* AT_PAGESZ */
    case _SC_CLK_TCK: result = v_auxiliary_value(17); break; /* AT_CLKTCK */
#ifdef _SC_MINSIGSTKSZ
    case _SC_MINSIGSTKSZ: result = v_signal_stack_minimum(); break;
    case _SC_SIGSTKSZ:
        result = v_signal_stack_minimum();
        if (result < v_signal_stack_floor) result = v_signal_stack_floor;
        result *= 4;
        break;
#endif
    case _SC_THREAD_STACK_MIN: result = 16384; break;
    case _SC_THREAD_KEYS_MAX: result = 1024; break;
    case _SC_THREAD_DESTRUCTOR_ITERATIONS: result = 4; break;
    case _SC_THREAD_THREADS_MAX: result = -1; break;
    case _SC_NPROCESSORS_ONLN: result = v_cpu_count("/sys/devices/system/cpu/online"); break;
    case _SC_NPROCESSORS_CONF: result = v_cpu_count("/sys/devices/system/cpu/present"); break;
    case _SC_OPEN_MAX: {
        struct { uint64_t current, maximum; } limit;
        result = V_RAW4(SYS_prlimit64, 0, RLIMIT_NOFILE, 0, &limit);
        if (result < 0) return v_linux_result(result);
        result = limit.current > LONG_MAX ? LONG_MAX : (long)limit.current;
        break;
    }
    case _SC_PHYS_PAGES: case _SC_AVPHYS_PAGES: {
        long page = v_auxiliary_value(6);
        if (page < 0) return -1;
        struct sysinfo info;
        result = V_RAW1(SYS_sysinfo, &info);
        if (result < 0) return v_linux_result(result);
        uint64_t bytes = (uint64_t)(name == _SC_PHYS_PAGES ? info.totalram : info.freeram) * info.mem_unit;
        result = (long)(bytes / (unsigned long)page);
        break;
    }
    default: errno = EINVAL; return -1;
    }
    if (result >= 0) errno = saved_errno;
    return result;
}

#if defined(__GLIBC__)
extern __typeof__(sysconf) __sysconf __attribute__((alias("sysconf")));
#endif


unsigned int sleep(unsigned int seconds) {
    struct timespec request = {.tv_sec = seconds}, remaining = {0};
    if (!nanosleep(&request, &remaining)) return 0;
    if (errno != EINTR) return seconds;
    return (unsigned int)remaining.tv_sec + (remaining.tv_nsec != 0);
}
