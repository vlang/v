#define _GNU_SOURCE 1
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <string.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <unistd.h>
#define CHECK(x) do { if (!(x)) return __LINE__; } while (0)
static volatile int child_gate;
static int wait_status(pid_t pid) {
    int status;
    long result;
    do { result = syscall(SYS_wait4, pid, &status, 0, NULL); } while (result < 0 && errno == EINTR);
    if (result != pid || !WIFEXITED(status)) return -1;
    return WEXITSTATUS(status);
}
__attribute__((noinline)) static int spawn_one(void) {
    volatile uint64_t canary[64];
    for (int i=0;i<64;i++) canary[i] = UINT64_C(0x8142891823746980) + i;
    int pair[2];
    CHECK(pipe2(pair, O_CLOEXEC) == 0);
    char *const argv[] = {"sh", "-c", "printf '%s/%s' \"$1\" \"$V_PROCESS_TOKEN\"; exit 42", "probe", "argument", NULL};
    char *const envp[] = {"V_PROCESS_TOKEN=child-environment", NULL};
    child_gate = 0;
    pid_t pid = vfork();
    if (!pid) {
        if (setsid() != getpid()) _exit(121);
        if (dup2(pair[1], 1) != 1) _exit(122);
        close(pair[0]); close(pair[1]);
        child_gate = 41;
        execve("/bin/sh", argv, envp);
        _exit(123);
    }
    CHECK(pid > 0 && child_gate == 41);
    for (int i=0;i<64;i++) CHECK(canary[i] == UINT64_C(0x8142891823746980) + i);
    close(pair[1]);
    char output[128];
    size_t used = 0;
    for (;;) {
        ssize_t n = read(pair[0], output+used, sizeof output-used);
        if (n < 0 && errno == EINTR) continue;
        CHECK(n >= 0);
        if (!n) break;
        used += (size_t)n;
        CHECK(used < sizeof output);
    }
    close(pair[0]);
    CHECK(used == strlen("argument/child-environment") && !memcmp(output, "argument/child-environment", used));
    CHECK(wait_status(pid) == 42);
    return 0;
}
int v_process_probe(void) {
    pid_t parent = getpid();
    long session = syscall(SYS_getsid, 0);
    int fd = open("/dev/null", O_RDWR|O_CLOEXEC);
    CHECK(fd >= 0 && dup2(fd, fd) == fd && (fcntl(fd, F_GETFD) & FD_CLOEXEC));
    int target = fcntl(fd, F_DUPFD_CLOEXEC, 3);
    CHECK(target >= 0 && dup2(fd, target) == target && !(fcntl(target,F_GETFD) & FD_CLOEXEC));
    CHECK(dup2(-1, target) == -1 && errno == EBADF && fcntl(target,F_GETFD) >= 0);
    close(fd); close(target);
    char *const missing[] = {"missing", NULL};
    char *const empty[] = {NULL};
    CHECK(execve("/vlibc-no-such-executable-78541", missing, empty) == -1 && errno == ENOENT);
    for (int i=0;i<64;i++) { int result=spawn_one(); if (result) return result; }
    pid_t failed = vfork();
    if (!failed) {
        execve("/vlibc-no-such-executable-78541", missing, empty);
        _exit(errno == ENOENT ? 127 : 124);
    }
    CHECK(failed > 0 && wait_status(failed) == 127);
    CHECK(getpid() == parent && syscall(SYS_getsid,0) == session);
    return 0;
}
