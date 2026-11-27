#ifndef V_PROCESS_TEST_ORACLES_H
#define V_PROCESS_TEST_ORACLES_H
#ifdef PROCESS_TEST_HOOKS
#include <sys/resource.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
/* Independent descriptor and kernel failure oracles; never used by os. */
static struct rlimit process_saved_files, process_saved_children;
static int process_saved_stdio[3];
static int process_fd_count(void) {
    int n = 0;
    for (int i = 0; i < 256; ++i) if (fcntl(i, F_GETFD) >= 0) ++n;
    return n;
}
/* Keep descriptors supplied by the runner out of the child leak check. */
static int process_isolate_inherited_fds(void) {
    for (int i = 3; i < 256; ++i) {
        int flags = fcntl(i, F_GETFD);
        if (flags >= 0 && fcntl(i, F_SETFD, flags | FD_CLOEXEC) < 0) return -1;
    }
    return 0;
}
static int process_close_stdio(void) {
    for (int i = 0; i < 3; ++i) {
        process_saved_stdio[i] = fcntl(i, F_DUPFD_CLOEXEC, 100);
        if (process_saved_stdio[i] < 0) return -1;
    }
    for (int i = 0; i < 3; ++i) close(i);
    return 0;
}
static void process_restore_stdio(void) {
    for (int i = 0; i < 3; ++i) { dup2(process_saved_stdio[i], i); close(process_saved_stdio[i]); }
}
static int process_limit_files(int room) {
    if (getrlimit(RLIMIT_NOFILE, &process_saved_files)) return -1;
    struct rlimit limit = process_saved_files;
    /* RLIMIT_NOFILE bounds descriptor numbers. Find the first vacant slot
     * beyond the requested room even if the runner left open descriptors. */
    int fd = -1;
    for (int i = 0; i <= room; ++i) {
        fd = fcntl(STDIN_FILENO, F_DUPFD_CLOEXEC, fd + 1);
        if (fd < 0 || close(fd) < 0) return -1;
    }
    limit.rlim_cur = (rlim_t)fd;
    return setrlimit(RLIMIT_NOFILE, &limit);
}
static void process_restore_files(void) { setrlimit(RLIMIT_NOFILE, &process_saved_files); }
static int process_limit_children(void) {
    if (geteuid() == 0) return 1; /* RLIMIT_NPROC does not constrain root. */
    if (getrlimit(RLIMIT_NPROC, &process_saved_children)) return -1;
    struct rlimit limit = process_saved_children;
    limit.rlim_cur = 0;
    return setrlimit(RLIMIT_NPROC, &limit);
}
static void process_restore_children(void) { setrlimit(RLIMIT_NPROC, &process_saved_children); }
#endif
#ifdef PROCESS_TRACK_ALLOC
#include <stddef.h>
#include <unistd.h>
#include <stdlib.h>
void *__real_malloc(size_t);
void *__real_calloc(size_t, size_t);
void *__real_realloc(void *, size_t);
void __real_free(void *);
static pid_t process_parent_pid;
/* exec resets this static state. Before exec, any allocator operation by the
 * forked child terminates it, including allocator calls on failure paths. */
static void process_check_allocator(void) {
    if (process_parent_pid && getpid() != process_parent_pid) _exit(99);
}
void *__wrap_malloc(size_t n) { process_check_allocator(); return __real_malloc(n); }
void *__wrap_calloc(size_t n, size_t s) { process_check_allocator(); return __real_calloc(n, s); }
void *__wrap_realloc(void *p, size_t n) { process_check_allocator(); return __real_realloc(p, n); }
void __wrap_free(void *p) { process_check_allocator(); __real_free(p); }
static void process_track_allocator(void) { process_parent_pid = getpid(); }
#endif
#endif
