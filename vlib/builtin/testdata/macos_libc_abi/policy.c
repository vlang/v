/* Linux C-SDK oracle for the exact helpers generated from the production V.
 * Only kernel/clock transport is doubled. This file is never production code. */
#define _GNU_SOURCE 1
#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <errno.h>
#include <limits.h>
#include <signal.h>
#include <pthread.h>
#include <sys/mman.h>
#include <sys/syscall.h>
#include <sys/wait.h>

typedef uint8_t u8;
typedef uint32_t u32;
typedef uint64_t u64;
typedef int32_t i32;
typedef int64_t i64;
#define VNORETURN __attribute__((noreturn))

enum { host_setmask = SIG_SETMASK, host_block = SIG_BLOCK, host_bus = SIGBUS,
       host_abrt = SIGABRT, host_ill = SIGILL, host_trap = SIGTRAP, host_segv = SIGSEGV };
/* Independent Darwin SDK model; the real native fixture uses Apple's headers. */
#undef SIG_SETMASK
#define SIG_SETMASK 3
#undef SIGBUS
#define SIGBUS 10
#define FIODTYPE 0x4004667aUL
#define TIOCGETA 0x40487413UL
#define D_TTY 3
struct termios { u64 flags[4]; u8 cc[20]; u32 padding; u64 speeds[2]; };
_Static_assert(sizeof(struct termios) == 72, "Darwin termios output size");
_Static_assert(sizeof(ssize_t) == 8 && sizeof(size_t) == 8 && sizeof(int) == 4, "LP64 SDK ABI");

static _Thread_local i32 native_error;
static _Thread_local i32 bf_error;
static _Thread_local bool forbid_errno;
static int observation_fd = -1;
static VNORETURN void fail(const char *message) {
    (void)syscall(SYS_write, STDERR_FILENO, message, strlen(message));
    _exit(200);
}
#define require(condition) do { if (!(condition)) fail("libc ABI oracle failed: " #condition "\n"); } while (0)
i32 *oracle_errno_address(void) {
    require(!forbid_errno);
    return &native_error;
}
#define bf_native_errno_cell (*oracle_errno_address())

/* Rename symbols only AFTER V has chosen their actual C export ABI. Native
 * libc remains available for the hosted test controller and its assertions. */
#define read probe_read
#define write probe_write
#define isatty probe_isatty
#undef memchr
#define memchr probe_memchr
#define abort probe_abort
#define __assert_rtn probe_assert
#define __error probe_error
VNORETURN void probe_abort(void);
VNORETURN void probe_assert(const char *, const char *, int, const char *);
#include "generated_libc.h"
#undef read
#undef write
#undef isatty
#undef memchr
#undef abort
#undef __assert_rtn
#undef __error

_Static_assert(sizeof(BareMacSignalAction) == 24, "kernel sigaction input ABI");
_Static_assert(offsetof(BareMacSignalAction, mask) == 16 && offsetof(BareMacSignalAction, flags) == 20, "signal mask offsets");

enum { mode_io, mode_tty, mode_abort };
static _Thread_local struct {
    int mode, calls;
    u32 expected_nr;
    i32 fd, code, fallback_code, kind;
    const void *buffer;
    size_t count;
    u64 result;
    bool signal_failure;
} mock;

static void observe(char byte) {
    if (observation_fd >= 0) require(syscall(SYS_write, observation_fd, &byte, 1) == 1);
}

u32 mac_kernel_thread_self(void) { return (u32)syscall(SYS_gettid); }

static void apply_mask(u32 mask) {
    sigset_t native;
    require(sigfillset(&native) == 0);
    const int darwin[] = {6, 11, 10, 4, 5};
    const int signals[] = {host_abrt, host_segv, host_bus, host_ill, host_trap};
    for (size_t i = 0; i < sizeof darwin / sizeof *darwin; ++i)
        if (!(mask & (1U << (darwin[i] - 1)))) require(sigdelset(&native, signals[i]) == 0);
    require(pthread_sigmask(host_setmask, &native, NULL) == 0);
}

i32 mac_kernel_bsd_call6(u32 nr, u64 a1, u64 a2, u64 a3, u64 a4, u64 a5, u64 a6, u64 *value) {
    require(a4 == 0 && a5 == 0 && a6 == 0);
    mock.calls++;
    if (value) *value = 0;
    if (mock.mode == mode_abort) {
        if (nr == 329) {
            require(a1 == 3 && a2 != 0 && a3 == 0 && value == NULL);
            apply_mask(*(const u32 *)(uintptr_t)a2);
            return 0;
        }
        if (nr == 328) {
            require(a1 == mac_kernel_thread_self() && a2 == 6 && a3 == 0 && value == NULL);
            if (mock.signal_failure) return 22;
            require(pthread_kill(pthread_self(), host_abrt) == 0);
            return 0;
        }
        if (nr == 46) {
            const BareMacSignalAction *action = (const void *)(uintptr_t)a2;
            require(a1 == 6 && a3 == 0 && value == NULL && action);
            require(action->handler == NULL && action->trampoline == NULL && action->flags == 0 && action->mask == UINT32_MAX);
            struct sigaction native;
            memset(&native, 0, sizeof native);
            native.sa_handler = SIG_DFL;
            require(sigfillset(&native.sa_mask) == 0 && sigaction(host_abrt, &native, NULL) == 0);
            return 0;
        }
        if (nr == 4) {
            require(a1 == STDERR_FILENO && value);
            ssize_t written = syscall(SYS_write, STDERR_FILENO, (const void *)(uintptr_t)a2, (size_t)a3);
            if (written < 0) return (i32)errno;
            *value = (u64)written;
            return 0;
        }
        fail("unexpected abort transport\n");
    }
    require(a1 == (u64)(i64)mock.fd);
    if (mock.mode == mode_tty) {
        require(nr == 54 && a3 != 0 && value == NULL);
        if (mock.calls == 1) {
            require(a2 == FIODTYPE);
            if (!mock.code) *(i32 *)(uintptr_t)a3 = mock.kind;
            return mock.code;
        }
        require(mock.calls == 2 && a2 == TIOCGETA);
        if (!mock.fallback_code) memset((void *)(uintptr_t)a3, 0xa5, sizeof(struct termios));
        return mock.fallback_code;
    }
    require(nr == mock.expected_nr && a2 == (u64)(uintptr_t)mock.buffer && a3 == mock.count && value);
    if (!mock.code) *value = mock.result;
    return mock.code;
}

i64 mac_clock_sleep_native(FSysClockStamp request, FSysClockStamp *remaining) {
    require(mock.mode == mode_abort && request.seconds == 0 && request.nanoseconds == 10000000 && remaining);
    require(forbid_errno);
    *remaining = (FSysClockStamp){0};
    return 0;
}

static ssize_t (*volatile read_call)(int, void *, size_t) = probe_read;
static ssize_t (*volatile write_call)(int, const void *, size_t) = probe_write;
static int (*volatile tty_call)(int) = probe_isatty;
static void *(*volatile find_call)(const void *, int, size_t) = probe_memchr;
static void (*volatile abort_call)(void) = probe_abort;
static void (*volatile assert_call)(const char *, const char *, int, const char *) = probe_assert;

static void check_io(void) {
    char buffer[8] = "hello";
    const int errors[] = {0, 4, 9, 35};
    for (u32 nr = 3; nr <= 4; ++nr) {
        for (size_t i = 0; i < sizeof errors / sizeof *errors; ++i) {
            memset(&mock, 0, sizeof mock);
            mock.expected_nr = nr; mock.fd = -7; mock.buffer = buffer;
            mock.count = (size_t)UINT32_MAX + 19; mock.result = (u64)UINT32_MAX + 9;
            mock.code = errors[i];
            native_error = 91; bf_error = 77;
            forbid_errno = errors[i] == 0;
            ssize_t result = nr == 3 ? read_call(-7, buffer, mock.count) : write_call(-7, buffer, mock.count);
            forbid_errno = false;
            require(result == (errors[i] ? (ssize_t)-1 : (ssize_t)mock.result));
            require(mock.calls == 1 && native_error == (errors[i] ? errors[i] : 91) && bf_error == 77);
        }
        for (size_t result = 0; result <= 3; ++result) {
            memset(&mock, 0, sizeof mock);
            mock.expected_nr = nr; mock.fd = 42; mock.buffer = buffer; mock.count = 8; mock.result = result;
            native_error = 92; bf_error = 78; forbid_errno = true;
            ssize_t actual = nr == 3 ? read_call(42, buffer, 8) : write_call(42, buffer, 8);
            forbid_errno = false;
            require(actual == (ssize_t)result && mock.calls == 1 && native_error == 92 && bf_error == 78);
        }
    }
}

static void check_tty(void) {
    const struct { i32 code, kind, fallback, result, error, calls; } cases[] = {
        {0, 3, 0, 1, 93, 1}, {0, 2, 0, 0, 25, 1},
        {45, 0, 0, 1, 93, 2}, {45, 0, 25, 0, 25, 2}, {9, 0, 9, 0, 9, 2}
    };
    for (size_t i = 0; i < sizeof cases / sizeof *cases; ++i) {
        memset(&mock, 0, sizeof mock);
        mock.mode = mode_tty; mock.fd = -3; mock.code = cases[i].code;
        mock.kind = cases[i].kind; mock.fallback_code = cases[i].fallback;
        native_error = 93; bf_error = 79;
        require(tty_call(-3) == cases[i].result && mock.calls == cases[i].calls);
        require(native_error == cases[i].error && bf_error == 79);
    }
}

static void check_find(void) {
    unsigned char bytes[768];
    for (size_t i = 0; i < sizeof bytes; ++i) bytes[i] = (unsigned char)(i * 41);
    require(find_call(NULL, 123, 0) == NULL);
    for (size_t start = 0; start < 32; ++start) {
        for (size_t count = 0; count <= 511; ++count) {
            for (int needle = -1; needle <= 511; needle += 17) {
                const unsigned char *expected = NULL;
                for (size_t j = 0; j < count; ++j)
                    if (bytes[start + j] == (unsigned char)needle) { expected = bytes + start + j; break; }
                require(find_call(bytes + start, needle, count) == expected);
            }
        }
    }
    size_t page = (size_t)sysconf(_SC_PAGESIZE);
    unsigned char *mapping = mmap(NULL, page * 2, PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    require(mapping != MAP_FAILED && mprotect(mapping + page, page, PROT_NONE) == 0);
    memset(mapping + page - 32, 1, 32);
    require(find_call(mapping + page - 32, 2, 32) == NULL);
    mapping[page - 1] = 255;
    require(find_call(mapping + page - 1, -1, SIZE_MAX) == mapping + page - 1);
    require(find_call(mapping + page - 1, 511, 1) == mapping + page - 1);
    require(munmap(mapping, page * 2) == 0);
}

static void exit_marker(void) { observe('A'); }
static void abort_handler(int signal) {
    require(signal == host_abrt && native_error == 94 && bf_error == 80);
    observe('H');
}

static void perform_abort(int mode) {
    memset(&mock, 0, sizeof mock);
    mock.mode = mode_abort;
    mock.signal_failure = mode == 6;
    native_error = 94; bf_error = 80; forbid_errno = true;
    if (mode >= 3 && mode <= 5) {
        const char *function = mode == 3 ? "my_function" : mode == 4 ? NULL : (const char *)(uintptr_t)UINTPTR_MAX;
        assert_call(function, "fixture.c", mode == 5 ? INT32_MIN : 123, "value != 0");
    } else abort_call();
    observe('R');
    _exit(201);
}

static void *abort_worker(void *unused) { (void)unused; observe('W'); perform_abort(2); return NULL; }

static void check_abort(int mode) {
    int events[2], diagnostic[2];
    require(pipe(events) == 0 && pipe(diagnostic) == 0);
    pid_t child = fork();
    require(child >= 0);
    if (child == 0) {
        close(events[0]); close(diagnostic[0]);
        require(dup2(diagnostic[1], STDERR_FILENO) == STDERR_FILENO);
        close(diagnostic[1]); observation_fd = events[1];
        struct sigaction action;
        memset(&action, 0, sizeof action); sigemptyset(&action.sa_mask);
        action.sa_handler = mode == 1 ? SIG_IGN : mode == 2 || mode == 7 ? abort_handler : SIG_DFL;
        require(sigaction(host_abrt, &action, NULL) == 0);
        action.sa_handler = SIG_DFL; require(sigaction(host_ill, &action, NULL) == 0);
        sigset_t blocked; sigemptyset(&blocked); sigaddset(&blocked, host_abrt);
        require(pthread_sigmask(host_block, &blocked, NULL) == 0 && atexit(exit_marker) == 0);
        if (mode == 7) {
            pthread_t worker; require(pthread_create(&worker, NULL, abort_worker, NULL) == 0);
            pthread_join(worker, NULL); _exit(202);
        }
        perform_abort(mode);
    }
    close(events[1]); close(diagnostic[1]);
    int status; require(waitpid(child, &status, 0) == child);
    require(WIFSIGNALED(status) && WTERMSIG(status) == (mode == 6 ? host_ill : host_abrt));
    char observed[16] = {0}, output[512] = {0};
    ssize_t event_count = read(events[0], observed, sizeof observed - 1);
    ssize_t length = read(diagnostic[0], output, sizeof output - 1);
    require(event_count >= 0 && length >= 0);
    require(strcmp(observed, mode == 2 ? "H" : mode == 7 ? "WH" : "") == 0);
    if (mode == 3) require(strcmp(output, "Assertion failed: (value != 0), function my_function, file fixture.c, line 123.\n") == 0);
    if (mode == 4) require(strcmp(output, "Assertion failed: (value != 0), file fixture.c, line 123.\n") == 0);
    if (mode == 5) require(strcmp(output, "fixture.c:-2147483648: failed assertion `value != 0'\n") == 0);
    close(events[0]); close(diagnostic[0]);
}

int main(void) {
    check_io(); check_tty(); check_find();
    for (int mode = 0; mode < 8; ++mode) check_abort(mode);
    puts("V Darwin libc ABI policy ok");
    return 0;
}
