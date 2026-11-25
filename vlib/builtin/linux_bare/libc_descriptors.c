#include "libc_compat.h"
#include "libc_cancel_abi.h"
#include <fcntl.h>
#include <stdarg.h>
#include <string.h>
#include <sys/stat.h>
#include <unistd.h>

/* C23 headers may expose qualifier-preserving generic macros at these names. */
#undef strchr
#undef strstr

int close(int fd) { return (int)v_linux_result(v_cancel_syscall(SYS_close, (uintptr_t)(fd), 0, 0, 0, 0, 0)); }

int open(const char *path, int flags, ...) {
    mode_t mode = 0;
    if ((flags & O_CREAT) || (flags & O_TMPFILE) == O_TMPFILE) {
        va_list args;
        va_start(args, flags);
        mode = va_arg(args, unsigned int);
        va_end(args);
    }
    return (int)v_linux_result(v_cancel_syscall(SYS_openat, (uintptr_t)(AT_FDCWD), (uintptr_t)(path), (uintptr_t)(flags), (uintptr_t)(mode), 0, 0));
}

extern int32_t v_native_openat(int32_t, uintptr_t, int32_t, uint32_t);
int openat(int fd, const char *path, int flags, ...) {
    mode_t mode = 0;
    if ((flags & O_CREAT) || (flags & O_TMPFILE) == O_TMPFILE) {
        va_list arguments;
        va_start(arguments, flags);
        mode = va_arg(arguments, unsigned int);
        va_end(arguments);
    }
    return v_native_openat(fd, (uintptr_t)path, flags, mode);
}

extern int64_t v_native_pread(int32_t, uintptr_t, uintptr_t, int64_t);
ssize_t pread(int fd, void *buffer, size_t size, off_t offset) {
    return (ssize_t)v_native_pread(fd, (uintptr_t)buffer, size, offset);
}
#if defined(__GLIBC__)
_Static_assert(sizeof(off_t) == sizeof(off64_t), "native 64-bit offsets");
extern __typeof__(pread64) pread64 __attribute__((alias("pread")));
#endif

off_t lseek(int fd, off_t offset, int whence) {
    return (off_t)v_linux_result(V_RAW3(SYS_lseek, fd, offset, whence));
}

int fcntl(int fd, int command, ...) {
    uintptr_t argument = 0;
    va_list args;
    va_start(args, command);
    switch (command) {
    case F_GETFD:
    case F_GETFL:
    case F_GETOWN:
    case F_GETSIG:
    case F_GETLEASE:
    case F_GETPIPE_SZ:
    case F_GET_SEALS:
        break;
    case F_DUPFD:
    case F_DUPFD_CLOEXEC:
    case F_SETFD:
    case F_SETFL:
    case F_SETOWN:
    case F_SETSIG:
    case F_SETLEASE:
    case F_NOTIFY:
    case F_SETPIPE_SZ:
    case F_ADD_SEALS:
        argument = (uintptr_t)va_arg(args, int);
        break;
    case F_GETLK:
    case F_SETLK:
    case F_SETLKW:
    case F_OFD_GETLK:
    case F_OFD_SETLK:
    case F_OFD_SETLKW:
    case F_GETOWN_EX:
    case F_SETOWN_EX:
        argument = (uintptr_t)va_arg(args, void *);
        break;
    default:
        va_end(args);
        errno = EINVAL;
        return -1;
    }
    va_end(args);
    long result = V_RAW3(SYS_fcntl, fd, command, argument);
    /* F_GETOWN's successful negative process-group IDs are not kernel errors.
     * Use F_GETOWN_EX to disambiguate them. */
    if (command == F_GETOWN) {
        struct f_owner_ex owner;
        result = V_RAW3(SYS_fcntl, fd, F_GETOWN_EX, &owner);
        if (result >= 0)
            return owner.type == F_OWNER_PGRP ? -owner.pid : owner.pid;
    }
    return (int)v_linux_result(result);
}

static dev_t v_linux_device(uint32_t major, uint32_t minor) {
    return (dev_t)(((uint64_t)(major & 0xfff) << 8) | ((uint64_t)(major & ~0xfffu) << 32) |
                   (minor & 0xff) | ((uint64_t)(minor & ~0xffu) << 12));
}

static int v_linux_stat(int fd, const char *path, int flags, struct stat *out) {
    if (!out) {
        errno = EFAULT;
        return -1;
    }
    struct statx data;
    long result = V_RAW5(SYS_statx, fd, path, flags, STATX_BASIC_STATS, &data);
    if (result < 0)
        return (int)v_linux_result(result);
    memset(out, 0, sizeof *out);
    out->st_dev = v_linux_device(data.stx_dev_major, data.stx_dev_minor);
    out->st_ino = data.stx_ino;
    out->st_mode = data.stx_mode;
    out->st_nlink = data.stx_nlink;
    out->st_uid = data.stx_uid;
    out->st_gid = data.stx_gid;
    out->st_rdev = v_linux_device(data.stx_rdev_major, data.stx_rdev_minor);
    out->st_size = data.stx_size;
    out->st_blksize = data.stx_blksize;
    out->st_blocks = data.stx_blocks;
    out->st_atim.tv_sec = data.stx_atime.tv_sec;
    out->st_atim.tv_nsec = data.stx_atime.tv_nsec;
    out->st_mtim.tv_sec = data.stx_mtime.tv_sec;
    out->st_mtim.tv_nsec = data.stx_mtime.tv_nsec;
    out->st_ctim.tv_sec = data.stx_ctime.tv_sec;
    out->st_ctim.tv_nsec = data.stx_ctime.tv_nsec;
    return 0;
}

int stat(const char *path, struct stat *out) { return v_linux_stat(AT_FDCWD, path, 0, out); }

int fstat(int fd, struct stat *out) { return v_linux_stat(fd, "", AT_EMPTY_PATH, out); }

int rename(const char *from, const char *to) {
    return (int)v_linux_result(V_RAW4(SYS_renameat, AT_FDCWD, from, AT_FDCWD, to));
}

int remove(const char *path) {
    long result = V_RAW3(SYS_unlinkat, AT_FDCWD, path, 0);
    if (result == -EISDIR)
        result = V_RAW3(SYS_unlinkat, AT_FDCWD, path, AT_REMOVEDIR);
    return (int)v_linux_result(result);
}

pid_t getpid(void) { return (pid_t)V_RAW0(SYS_getpid); }

int strcmp(const char *a, const char *b) {
    while (*a && *a == *b) {
        ++a;
        ++b;
    }
    return (unsigned char)*a - (unsigned char)*b;
}

char *strchr(const char *text, int ch) {
    for (;;) {
        if ((unsigned char)*text == (unsigned char)ch)
            return (char *)text;
        if (!*text++)
            return NULL;
    }
}

char *strstr(const char *text, const char *needle) {
    if (!*needle)
        return (char *)text;
    for (; *text; ++text) {
        const char *a = text, *b = needle;
        while (*a && *a == *b) {
            ++a;
            ++b;
        }
        if (!*b)
            return (char *)text;
    }
    return NULL;
}

void explicit_bzero(void *buffer, size_t size) {
    volatile unsigned char *bytes = buffer;
    while (size--)
        *bytes++ = 0;
}

int lstat(const char *path, struct stat *out) {
    return v_linux_stat(AT_FDCWD, path, AT_SYMLINK_NOFOLLOW, out);
}
int fstatat(int fd, const char *path, struct stat *out, int flags) {
    return v_linux_stat(fd, path, flags, out);
}
#if defined(__GLIBC__)
off64_t lseek64(int fd, off64_t offset, int whence) { return lseek(fd, offset, whence); }
_Static_assert(sizeof(struct stat64) == sizeof(struct stat), "native 64-bit stat");
_Static_assert(offsetof(struct stat64, st_ctim) == offsetof(struct stat, st_ctim),
               "native stat timestamps");
extern __typeof__(open64) open64 __attribute__((alias("open")));
extern __typeof__(openat) openat64 __attribute__((alias("openat")));
extern __typeof__(fcntl) fcntl64 __attribute__((alias("fcntl")));
int stat64(const char *path, struct stat64 *out) {
    if (!out) {
        errno = EFAULT;
        return -1;
    }
    struct stat data;
    int result = stat(path, &data);
    if (!result)
        memcpy(out, &data, sizeof data);
    return result;
}
int fstat64(int fd, struct stat64 *out) {
    if (!out) {
        errno = EFAULT;
        return -1;
    }
    struct stat data;
    int result = fstat(fd, &data);
    if (!result)
        memcpy(out, &data, sizeof data);
    return result;
}
int lstat64(const char *path, struct stat64 *out) {
    if (!out) {
        errno = EFAULT;
        return -1;
    }
    struct stat data;
    int result = lstat(path, &data);
    if (!result)
        memcpy(out, &data, sizeof data);
    return result;
}
#endif
