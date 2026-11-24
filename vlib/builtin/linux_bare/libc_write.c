#include "libc_write.h"
#include <errno.h>

ssize_t write(int fd, const void *buffer, size_t count) {
    int64_t result = v_freestanding_write((int32_t)fd, (uintptr_t)buffer, count);
    if (result < 0) {
        errno = (int)-result;
        return -1;
    }
    return (ssize_t)result;
}
