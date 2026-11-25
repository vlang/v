#include "libc_compat.h"
#include "libc_cancel_abi.h"
#include <dirent.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/mman.h>
#include <sys/select.h>
#include <sys/shm.h>
#include <sys/stat.h>
#include <sys/statfs.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/utsname.h>
#include <unistd.h>

uid_t getuid(void) { return V_RAW0(SYS_getuid); }
uid_t geteuid(void) { return V_RAW0(SYS_geteuid); }
gid_t getgid(void) { return V_RAW0(SYS_getgid); }
gid_t getegid(void) { return V_RAW0(SYS_getegid); }
int getresuid(uid_t *r, uid_t *e, uid_t *s) {
    return v_linux_result(V_RAW3(SYS_getresuid, r, e, s));
}
int getresgid(gid_t *r, gid_t *e, gid_t *s) {
    return v_linux_result(V_RAW3(SYS_getresgid, r, e, s));
}
int kill(pid_t pid, int signal) { return v_linux_result(V_RAW2(SYS_kill, pid, signal)); }
int uname(struct utsname *out) { return v_linux_result(V_RAW1(SYS_uname, out)); }
int access(const char *p, int m) { return v_linux_result(V_RAW3(SYS_faccessat, AT_FDCWD, p, m)); }
int chmod(const char *p, mode_t m) { return v_linux_result(V_RAW3(SYS_fchmodat, AT_FDCWD, p, m)); }
int mkdir(const char *p, mode_t m) { return v_linux_result(V_RAW3(SYS_mkdirat, AT_FDCWD, p, m)); }
int unlink(const char *p) { return v_linux_result(V_RAW3(SYS_unlinkat, AT_FDCWD, p, 0)); }
int rmdir(const char *p) { return v_linux_result(V_RAW3(SYS_unlinkat, AT_FDCWD, p, AT_REMOVEDIR)); }
int link(const char *a, const char *b) {
    return v_linux_result(V_RAW5(SYS_linkat, AT_FDCWD, a, AT_FDCWD, b, 0));
}
int symlink(const char *a, const char *b) {
    return v_linux_result(V_RAW3(SYS_symlinkat, a, AT_FDCWD, b));
}
ssize_t readlink(const char *p, char *b, size_t n) {
    return v_linux_result(V_RAW4(SYS_readlinkat, AT_FDCWD, p, b, n));
}
ssize_t readv(int fd, const struct iovec *v, int n) {
    return v_linux_result(v_cancel_syscall(SYS_readv, (uintptr_t)(fd), (uintptr_t)(v), (uintptr_t)(n), 0, 0, 0));
}
ssize_t writev(int fd, const struct iovec *v, int n) {
    return v_linux_result(v_cancel_syscall(SYS_writev, (uintptr_t)(fd), (uintptr_t)(v), (uintptr_t)(n), 0, 0, 0));
}
int poll(struct pollfd *fds, nfds_t n, int timeout) {
    struct timespec t = {timeout / 1000, (timeout % 1000) * 1000000L};
    return v_linux_result(v_cancel_syscall(SYS_ppoll, (uintptr_t)(fds), (uintptr_t)(n), (uintptr_t)(timeout < 0 ? NULL : &t), (uintptr_t)(NULL), (uintptr_t)(8), 0));
}
int select(int n, fd_set *r, fd_set *w, fd_set *e, struct timeval *timeout) {
    if (n < 0 ||
        (timeout && (timeout->tv_sec < 0 || timeout->tv_usec < 0 || timeout->tv_usec >= 1000000))) {
        errno = EINVAL;
        return -1;
    }
    struct timespec t;
    if (timeout) {
        t.tv_sec = timeout->tv_sec;
        t.tv_nsec = timeout->tv_usec * 1000;
    }
    long result = v_cancel_syscall(SYS_pselect6, (uintptr_t)(n), (uintptr_t)(r), (uintptr_t)(w), (uintptr_t)(e), (uintptr_t)(timeout ? &t : NULL), (uintptr_t)(NULL));
    if (timeout) {
        timeout->tv_sec = t.tv_sec;
        timeout->tv_usec = t.tv_nsec / 1000;
    }
    return v_linux_result(result);
}
/* Capture ioctl's request-dependent machine word in assembly. Reading a C
 * vararg as uintptr_t would be undefined for pointer or int actual arguments. */
int v_freestanding_ioctl(int fd, unsigned long request, uintptr_t value) {
    return v_linux_result(V_RAW3(SYS_ioctl, fd, request, value));
}
#if defined(__x86_64__)
__asm__(".pushsection .text\n.global ioctl\n.type ioctl,@function\nioctl:\njmp "
        "v_freestanding_ioctl\n.popsection\n");
#elif defined(__aarch64__)
__asm__(".pushsection .text\n.global ioctl\n.type ioctl,%function\nioctl:\nb "
        "v_freestanding_ioctl\n.popsection\n");
#endif
void *mmap(void *p, size_t n, int prot, int flags, int fd, off_t offset) {
    return (void *)v_linux_result(V_RAW6(SYS_mmap, p, n, prot, flags, fd, offset));
}
int munmap(void *p, size_t n) { return v_linux_result(V_RAW2(SYS_munmap, p, n)); }
int mprotect(void *p, size_t n, int prot) {
    return v_linux_result(V_RAW3(SYS_mprotect, p, n, prot));
}
int fstatfs(int fd, struct statfs *s) { return v_linux_result(V_RAW2(SYS_fstatfs, fd, s)); }
int statfs(const char *p, struct statfs *s) { return v_linux_result(V_RAW2(SYS_statfs, p, s)); }
int utimes(const char *p, const struct timeval times[2]) {
    struct timespec t[2];
    if (times)
        for (int i = 0; i < 2; ++i) {
            if (times[i].tv_usec < 0 || times[i].tv_usec >= 1000000) {
                errno = EINVAL;
                return -1;
            }
            t[i].tv_sec = times[i].tv_sec;
            t[i].tv_nsec = times[i].tv_usec * 1000;
        }
    return v_linux_result(V_RAW4(SYS_utimensat, AT_FDCWD, p, times ? t : NULL, 0));
}
char *getcwd(char *buffer, size_t size) {
    if (buffer) {
        if (!size) {
            errno = EINVAL;
            return NULL;
        }
        return v_linux_result(V_RAW2(SYS_getcwd, buffer, size)) < 0 ? NULL : buffer;
    }
    size_t capacity = size ? size : 128;
    for (;;) {
        char *p = malloc(capacity);
        if (!p) {
            errno = ENOMEM;
            return NULL;
        }
        long result = V_RAW2(SYS_getcwd, p, capacity);
        if (result >= 0)
            return p;
        free(p);
        if (result != -ERANGE || size || capacity > SIZE_MAX / 2) {
            v_linux_result(result);
            return NULL;
        }
        capacity *= 2;
    }
}

/* getdents64 records use the native 64-bit Linux dirent ABI on both targets. */
struct v_directory {
    int lock, fd;
    size_t at, end;
    off_t position;
    _Alignas(8) unsigned char buffer[32768];
};
_Static_assert(offsetof(struct dirent, d_name) == 19, "Linux dirent64 layout");
DIR *fdopendir(int fd) {
    struct stat st;
    if (fstat(fd, &st) < 0)
        return NULL;
    if (!S_ISDIR(st.st_mode)) {
        errno = ENOTDIR;
        return NULL;
    }
    int flags = fcntl(fd, F_GETFL);
    if (flags < 0)
        return NULL;
    if ((flags & O_ACCMODE) == O_WRONLY) {
        errno = EBADF;
        return NULL;
    }
    struct v_directory *d = calloc(1, sizeof *d);
    if (!d) {
        errno = ENOMEM;
        return NULL;
    }
    d->fd = fd;
    if (fcntl(fd, F_SETFD, FD_CLOEXEC) < 0) {
        int e = errno;
        free(d);
        errno = e;
        return NULL;
    }
    return (DIR *)d;
}
DIR *opendir(const char *path) {
    int fd = open(path, O_RDONLY | O_DIRECTORY | O_CLOEXEC);
    if (fd < 0)
        return NULL;
    DIR *d = fdopendir(fd);
    if (!d) {
        int e = errno;
        close(fd);
        errno = e;
    }
    return d;
}
struct dirent *readdir(DIR *directory) {
    struct v_directory *d = (void *)directory;
    v_linux_lock(&d->lock);
    if (d->at == d->end) {
        long n = V_RAW3(SYS_getdents64, d->fd, d->buffer, sizeof d->buffer);
        if (n <= 0) {
            if (n < 0)
                v_linux_result(n);
            v_linux_unlock(&d->lock);
            return NULL;
        }
        d->at = 0;
        d->end = n;
    }
    struct dirent *entry = (void *)(d->buffer + d->at);
    size_t remaining = d->end - d->at;
    if (remaining < 20 || entry->d_reclen < 20 || entry->d_reclen > remaining ||
        !memchr(entry->d_name, 0, entry->d_reclen - 19)) {
        errno = EIO;
        v_linux_unlock(&d->lock);
        return NULL;
    }
    d->at += entry->d_reclen;
    d->position = entry->d_off;
    v_linux_unlock(&d->lock);
    return entry;
}
int closedir(DIR *directory) {
    struct v_directory *d = (void *)directory;
    int result = close(d->fd);
    int e = errno;
    free(d);
    errno = e;
    return result;
}
int dirfd(DIR *directory) { return ((struct v_directory *)(void *)directory)->fd; }
void seekdir(DIR *directory, long position) {
    struct v_directory *d = (void *)directory;
    v_linux_lock(&d->lock);
    off_t result = lseek(d->fd, position, SEEK_SET);
    if (result >= 0) {
        d->position = result;
        d->at = d->end = 0;
    }
    v_linux_unlock(&d->lock);
}
void rewinddir(DIR *directory) { seekdir(directory, 0); }
long telldir(DIR *directory) {
    struct v_directory *d = (void *)directory;
    v_linux_lock(&d->lock);
    long p = d->position;
    v_linux_unlock(&d->lock);
    return p;
}
#if defined(__GLIBC__)
extern __typeof__(mmap64) mmap64 __attribute__((alias("mmap")));
_Static_assert(sizeof(struct statfs64) == sizeof(struct statfs), "native statfs64");
int fstatfs64(int fd, struct statfs64 *out) {
    if (!out) {
        errno = EFAULT;
        return -1;
    }
    struct statfs data;
    int result = fstatfs(fd, &data);
    if (!result)
        memcpy(out, &data, sizeof data);
    return result;
}
struct dirent64 *readdir64(DIR *directory) { return (struct dirent64 *)(void *)readdir(directory); }
#endif
int pipe2(int descriptors[2], int flags) {
    return v_linux_result(V_RAW2(SYS_pipe2, descriptors, flags));
}
int pipe(int descriptors[2]) { return pipe2(descriptors, 0); }

/* Portable guaranteed limits, with filesystem-specific name/block limits.
 * The conservative POSIX limits follow musl's fpathconf policy; COPYRIGHT.musl. */
long fpathconf(int fd, int name) {
    struct stat status;
    if (fstat(fd, &status) < 0) return -1;
    struct statfs fs;
    switch (name) {
    case _PC_NAME_MAX:
        if (fstatfs(fd, &fs) < 0) return -1;
        return fs.f_namelen;
    case _PC_REC_INCR_XFER_SIZE:
    case _PC_REC_MAX_XFER_SIZE:
    case _PC_REC_MIN_XFER_SIZE:
    case _PC_REC_XFER_ALIGN:
    case _PC_ALLOC_SIZE_MIN:
        if (fstatfs(fd, &fs) < 0) return -1;
        return fs.f_bsize;
    case _PC_LINK_MAX: return _POSIX_LINK_MAX;
    case _PC_MAX_CANON: return _POSIX_MAX_CANON;
    case _PC_MAX_INPUT: return _POSIX_MAX_INPUT;
    case _PC_PATH_MAX: return PATH_MAX;
    case _PC_PIPE_BUF: return PIPE_BUF;
    case _PC_VDISABLE: return 0;
    case _PC_FILESIZEBITS: return 64;
    case _PC_CHOWN_RESTRICTED:
    case _PC_NO_TRUNC:
    case _PC_SYNC_IO:
    case _PC_2_SYMLINKS: return 1;
    case _PC_ASYNC_IO:
    case _PC_PRIO_IO:
    case _PC_SOCK_MAXBUF:
    case _PC_SYMLINK_MAX: return -1;
    default: errno = EINVAL; return -1;
    }
}
long pathconf(const char *path, int name) {
    int fd = open(path, O_PATH | O_CLOEXEC);
    if (fd < 0) return -1;
    long result = fpathconf(fd, name);
    int error = errno;
    close(fd);
    errno = error;
    return result;
}

pid_t setsid(void) { return (pid_t)v_linux_result(V_RAW0(SYS_setsid)); }
int dup2(int oldfd, int newfd) {
#ifdef SYS_dup2
    return (int)v_linux_result(V_RAW2(SYS_dup2, oldfd, newfd));
#else
    if (oldfd == newfd) return fcntl(oldfd, F_GETFD) < 0 ? -1 : oldfd;
    return (int)v_linux_result(V_RAW3(SYS_dup3, oldfd, newfd, 0));
#endif
}
int execve(const char *path, char *const argv[], char *const envp[]) {
    return (int)v_linux_result(V_RAW3(SYS_execve, path, argv, envp));
}
/* vfork cannot return through a compiler-generated shared stack frame. The
 * AMD64 return PC stays in a syscall-preserved register until each process
 * returns; AArch64 keeps LR in the register bank. No callee-saved register or
 * parent stack word is exposed to child calls while the parent is blocked. */
__attribute__((visibility("hidden"))) long v_bare_vfork_result(long result) {
    return v_linux_result(result);
}
#if defined(__x86_64__)
__asm__(".pushsection .text.vfork,\"ax\",@progbits\n"
        ".global vfork\n.type vfork,@function\nvfork:\n"
        "pop %rdx\nmov $58,%eax\nsyscall\npush %rdx\n"
        "mov %rax,%rdi\njmp v_bare_vfork_result\n"
        ".size vfork,.-vfork\n.popsection\n");
#elif defined(__aarch64__)
__asm__(".pushsection .text.vfork,\"ax\",%progbits\n"
        ".global vfork\n.type vfork,%function\nvfork:\n"
        "mov x8,#220\nmov x0,#0x4111\nmov x1,#0\nmov x2,#0\nmov x3,#0\nmov x4,#0\n"
        "svc #0\nb v_bare_vfork_result\n"
        ".size vfork,.-vfork\n.popsection\n");
#endif
