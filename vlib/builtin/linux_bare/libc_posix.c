#include "libc_posix.h"
#include <errno.h>
#include <sys/ioctl.h>
#include <termios.h>

static int v_posix_status(int64_t result) {
    if (result < 0) {
        errno = (int)-result;
        return -1;
    }
    return 0;
}

ssize_t read(int fd, void *buffer, size_t count) {
    int64_t result = v_freestanding_read((int32_t)fd, (uintptr_t)buffer, count);
    if (result < 0) {
        errno = (int)-result;
        return -1;
    }
    return (ssize_t)result;
}

int clock_gettime(clockid_t clock, struct timespec *stamp) {
    return v_posix_status(v_freestanding_clock_gettime((int32_t)clock, (uintptr_t)stamp));
}

int nanosleep(const struct timespec *request, struct timespec *remaining) {
    return v_posix_status(v_freestanding_nanosleep((uintptr_t)request, (uintptr_t)remaining));
}

int isatty(int fd) {
    struct termios settings;
    int64_t result = v_freestanding_tty_probe((int32_t)fd, (uintptr_t)TCGETS,
                                            (uintptr_t)&settings);
    if (result < 0) {
        errno = (int)-result;
        return 0;
    }
    return 1;
}
