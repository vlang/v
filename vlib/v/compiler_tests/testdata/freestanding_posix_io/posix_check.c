#include <errno.h>
#include <fcntl.h>
#include <signal.h>
#include <stdint.h>
#include <sys/ioctl.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>
#include "posix_check.h"

/* Test-only setup, independent of the POSIX functions under test. Syscall IDs
 * come from the target system headers, including for cross compilation. */
static int64_t v_freestanding_posix_syscall(uint64_t number, uint64_t a, uint64_t b,
                                           uint64_t c, uint64_t d, uint64_t e, uint64_t f) {
#if defined(__x86_64__)
    register uint64_t arg4 __asm__("r10") = d;
    register uint64_t arg5 __asm__("r8") = e;
    register uint64_t arg6 __asm__("r9") = f;
    int64_t result;
    __asm__ volatile("syscall" : "=a"(result)
                     : "a"(number), "D"(a), "S"(b), "d"(c),
                       "r"(arg4), "r"(arg5), "r"(arg6)
                     : "rcx", "r11", "memory");
    return result;
#elif defined(__aarch64__)
    register uint64_t arg0 __asm__("x0") = a;
    register uint64_t arg1 __asm__("x1") = b;
    register uint64_t arg2 __asm__("x2") = c;
    register uint64_t arg3 __asm__("x3") = d;
    register uint64_t arg4 __asm__("x4") = e;
    register uint64_t arg5 __asm__("x5") = f;
    register uint64_t syscall_number __asm__("x8") = number;
    __asm__ volatile("svc #0" : "+r"(arg0)
                     : "r"(arg1), "r"(arg2), "r"(arg3), "r"(arg4),
                       "r"(arg5), "r"(syscall_number) : "memory", "cc");
    return (int64_t)arg0;
#else
#error unsupported test architecture
#endif
}

#define CHECK(condition) do { if (!(condition)) return __LINE__; } while (0)
#define RAW0(n) v_freestanding_posix_syscall((n), 0, 0, 0, 0, 0, 0)
#define RAW1(n,a) v_freestanding_posix_syscall((n), (uint64_t)(a), 0, 0, 0, 0, 0)
#define RAW2(n,a,b) v_freestanding_posix_syscall((n), (uint64_t)(a), (uint64_t)(b), 0, 0, 0, 0)
#define RAW3(n,a,b,c) v_freestanding_posix_syscall((n), (uint64_t)(a), (uint64_t)(b), (uint64_t)(c), 0, 0, 0)
#define RAW4(n,a,b,c,d) v_freestanding_posix_syscall((n), (uint64_t)(a), (uint64_t)(b), (uint64_t)(c), (uint64_t)(d), 0, 0)

static volatile sig_atomic_t interrupted;
static void signal_handler(int signal) {
    (void)signal;
    interrupted = 1;
}

/* rt_sigaction consumes the kernel layout, not libc's larger sigset_t. */
struct kernel_action {
    void (*handler)(int);
    unsigned long flags;
    void (*restorer)(void);
    unsigned long mask;
};
#if defined(__x86_64__)
#define STRINGIFY_INNER(x) #x
#define STRINGIFY(x) STRINGIFY_INNER(x)
__attribute__((naked)) static void signal_return(void) {
    __asm__ volatile("mov $" STRINGIFY(SYS_rt_sigreturn) ", %rax; syscall");
}
#endif

static int interrupt_operation(int sleep_operation) {
    struct kernel_action previous;
    struct kernel_action action = { signal_handler, 0, 0, 0 };
#if defined(__x86_64__)
    action.flags = 0x04000000; /* Linux SA_RESTORER, absent from libc headers. */
    action.restorer = signal_return;
#endif
    CHECK(RAW4(SYS_rt_sigaction, SIGUSR1, &action, &previous, sizeof action.mask) == 0);
    int pipefd[2];
    CHECK(RAW2(SYS_pipe2, pipefd, 0) == 0);
    int64_t parent = RAW0(SYS_getpid);
    int64_t child = RAW4(SYS_clone, SIGCHLD, 0, 0, 0);
    CHECK(child >= 0);
    if (child == 0) {
        const struct timespec pause = { 0, 2000000 };
        /* Repeated signals avoid a race with entering the blocked operation.
         * Closing the child's pipe endpoint bounds a mistaken read retry. */
        for (int i = 0; i < 200; ++i) {
            (void)RAW2(SYS_nanosleep, &pause, 0);
            (void)RAW2(SYS_kill, parent, SIGUSR1);
        }
        (void)RAW1(SYS_exit_group, 0);
        __builtin_unreachable();
    }
    CHECK(RAW1(SYS_close, pipefd[1]) == 0);
    interrupted = 0;
    errno = 123;
    int result;
    struct timespec remaining = { -1, -1 };
    if (sleep_operation) {
        const struct timespec request = { 5, 0 };
        result = nanosleep(&request, &remaining);
        CHECK(request.tv_sec == 5 && request.tv_nsec == 0);
    } else {
        unsigned char byte = 0;
        result = (int)read(pipefd[0], &byte, 1);
    }
    int saved_errno = errno;
    /* Reap the child before restoring the old handler so no late test signal
     * can escape into another test or terminate the process. */
    CHECK(RAW2(SYS_kill, child, SIGKILL) == 0);
    int status;
    int64_t waited;
    do { waited = RAW4(SYS_wait4, child, &status, 0, 0); } while (waited == -EINTR);
    CHECK(waited == child);
    CHECK(RAW1(SYS_close, pipefd[0]) == 0);
    CHECK(RAW4(SYS_rt_sigaction, SIGUSR1, &previous, 0, sizeof previous.mask) == 0);
    CHECK(interrupted != 0);
    CHECK(result == -1 && saved_errno == EINTR);
    if (sleep_operation) {
        CHECK(remaining.tv_sec >= 0 && remaining.tv_sec <= 5);
        CHECK(remaining.tv_nsec >= 0 && remaining.tv_nsec < 1000000000);
        CHECK(remaining.tv_sec != 0 || remaining.tv_nsec != 0);
    }
    return 0;
}

int v_freestanding_posix_check(void) {
    ssize_t (*const reader)(int, void *, size_t) = read;
    int (*const clock_reader)(clockid_t, struct timespec *) = clock_gettime;
    int (*const sleeper)(const struct timespec *, struct timespec *) = nanosleep;
    int (*const terminal)(int) = isatty;
    unsigned char buffer[8] = { 0 };
    errno = 123;
    CHECK(reader(-1, buffer, sizeof buffer) == -1 && errno == EBADF);
    int64_t nullfd = RAW4(SYS_openat, AT_FDCWD, "/dev/null", O_RDONLY, 0);
    CHECK(nullfd >= 0);
    errno = 123;
    CHECK(reader((int)nullfd, buffer, sizeof buffer) == 0 && errno == 123);
    CHECK(reader((int)nullfd, buffer, 0) == 0 && errno == 123);
    CHECK(terminal((int)nullfd) == 0 && errno == ENOTTY);
    CHECK(terminal(-1) == 0 && errno == EBADF);
    CHECK(RAW1(SYS_close, nullfd) == 0);

    int pipefd[2];
    CHECK(RAW2(SYS_pipe2, pipefd, O_NONBLOCK) == 0);
    CHECK(reader(pipefd[0], buffer, sizeof buffer) == -1 && errno == EAGAIN);
    static const unsigned char data[] = { 65, 0, 255 };
    CHECK(write(pipefd[1], data, sizeof data) == (ssize_t)sizeof data);
    errno = 123;
    CHECK(reader(pipefd[0], buffer, sizeof buffer) == (ssize_t)sizeof data && errno == 123);
    CHECK(buffer[0] == 65 && buffer[1] == 0 && buffer[2] == 255);
    CHECK(reader(pipefd[0], buffer, 1) == -1 && errno == EAGAIN);
    CHECK(RAW1(SYS_close, pipefd[1]) == 0);
    errno = 123;
    CHECK(reader(pipefd[0], buffer, sizeof buffer) == 0 && errno == 123);
    CHECK(RAW1(SYS_close, pipefd[0]) == 0);

    int64_t ttyfd = RAW4(SYS_openat, AT_FDCWD, "/dev/ptmx", O_RDWR | O_NOCTTY, 0);
    CHECK(ttyfd >= 0);
    errno = 123;
    CHECK(terminal((int)ttyfd) == 1 && errno == 123);
    CHECK(RAW1(SYS_close, ttyfd) == 0);

    struct timespec real, before, after;
    errno = 123;
    CHECK(clock_reader(CLOCK_REALTIME, &real) == 0 && errno == 123);
    CHECK(real.tv_sec > 0 && real.tv_nsec >= 0 && real.tv_nsec < 1000000000);
    CHECK(clock_reader(CLOCK_MONOTONIC, &before) == 0 && errno == 123);
    CHECK(clock_reader((clockid_t)-123, &after) == -1 && errno == EINVAL);
    const struct timespec invalid = { 0, 1000000000 };
    CHECK(sleeper(&invalid, 0) == -1 && errno == EINVAL);
    CHECK(invalid.tv_sec == 0 && invalid.tv_nsec == 1000000000);
    const struct timespec duration = { 0, 1000000 };
    errno = 123;
    CHECK(sleeper(&duration, 0) == 0 && errno == 123);
    CHECK(duration.tv_sec == 0 && duration.tv_nsec == 1000000);
    CHECK(clock_reader(CLOCK_MONOTONIC, &after) == 0 && errno == 123);
    CHECK(after.tv_sec > before.tv_sec ||
          (after.tv_sec == before.tv_sec && after.tv_nsec > before.tv_nsec));
    int interrupted_read = interrupt_operation(0);
    if (interrupted_read != 0) return interrupted_read;
    int interrupted_sleep = interrupt_operation(1);
    if (interrupted_sleep != 0) return interrupted_sleep;
    return 0;
}
