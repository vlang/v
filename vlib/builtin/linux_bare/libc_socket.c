#include "libc_compat.h"
#include "libc_cancel_abi.h"
#include <arpa/inet.h>
#include <netdb.h>
#include <stdlib.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/eventfd.h>
#include <sys/sendfile.h>
#include <sys/socket.h>

int socket(int family, int type, int protocol) {
    return (int)v_linux_result(V_RAW3(SYS_socket, family, type, protocol));
}
int socketpair(int family, int type, int protocol, int pair[2]) {
    return (int)v_linux_result(V_RAW4(SYS_socketpair, family, type, protocol, pair));
}
int bind(int fd, const struct sockaddr *address, socklen_t size) {
    return (int)v_linux_result(V_RAW3(SYS_bind, fd, address, size));
}
int connect(int fd, const struct sockaddr *address, socklen_t size) {
    return (int)v_linux_result(v_cancel_syscall(SYS_connect, (uintptr_t)(fd), (uintptr_t)(address), (uintptr_t)(size), 0, 0, 0));
}
int listen(int fd, int backlog) {
    return (int)v_linux_result(V_RAW2(SYS_listen, fd, backlog));
}
int accept4(int fd, struct sockaddr *address, socklen_t *size, int flags) {
    return (int)v_linux_result(v_cancel_syscall(SYS_accept4, (uintptr_t)(fd), (uintptr_t)(address), (uintptr_t)(size), (uintptr_t)(flags), 0, 0));
}
int getsockname(int fd, struct sockaddr *address, socklen_t *size) {
    return (int)v_linux_result(V_RAW3(SYS_getsockname, fd, address, size));
}
int getsockopt(int fd, int level, int option, void *value, socklen_t *size) {
    return (int)v_linux_result(V_RAW5(SYS_getsockopt, fd, level, option, value, size));
}
int setsockopt(int fd, int level, int option, const void *value, socklen_t size) {
    return (int)v_linux_result(V_RAW5(SYS_setsockopt, fd, level, option, value, size));
}
int shutdown(int fd, int how) {
    return (int)v_linux_result(V_RAW2(SYS_shutdown, fd, how));
}
ssize_t send(int fd, const void *buffer, size_t size, int flags) {
    return (ssize_t)v_linux_result(v_cancel_syscall(SYS_sendto, (uintptr_t)(fd), (uintptr_t)(buffer), (uintptr_t)(size), (uintptr_t)(flags), (uintptr_t)(0), (uintptr_t)(0)));
}
ssize_t recv(int fd, void *buffer, size_t size, int flags) {
    return (ssize_t)v_linux_result(v_cancel_syscall(SYS_recvfrom, (uintptr_t)(fd), (uintptr_t)(buffer), (uintptr_t)(size), (uintptr_t)(flags), (uintptr_t)(0), (uintptr_t)(0)));
}
ssize_t sendfile(int out, int in, off_t *offset, size_t count) {
    return (ssize_t)v_linux_result(v_cancel_syscall(SYS_sendfile, (uintptr_t)(out), (uintptr_t)(in), (uintptr_t)(offset), (uintptr_t)(count), 0, 0));
}
#if defined(__GLIBC__)
_Static_assert(sizeof(off_t) == sizeof(off64_t), "native 64-bit sendfile offsets");
extern __typeof__(sendfile64) sendfile64 __attribute__((alias("sendfile")));
#endif
int eventfd(unsigned int initial, int flags) {
    return (int)v_linux_result(V_RAW2(SYS_eventfd2, initial, flags));
}
int epoll_create1(int flags) {
    return (int)v_linux_result(V_RAW1(SYS_epoll_create1, flags));
}
int epoll_ctl(int fd, int operation, int target, struct epoll_event *event) {
    return (int)v_linux_result(V_RAW4(SYS_epoll_ctl, fd, operation, target, event));
}
int epoll_wait(int fd, struct epoll_event *events, int max, int timeout) {
    return (int)v_linux_result(v_cancel_syscall(SYS_epoll_pwait, (uintptr_t)(fd), (uintptr_t)(events), (uintptr_t)(max), (uintptr_t)(timeout), (uintptr_t)(0), (uintptr_t)(8)));
}

static int v_ipv4(const char *text, unsigned char out[4]) {
    for (int part = 0; part < 4; ++part) {
        unsigned int value = 0, digits = 0;
        const char *start = text;
        while (*text >= '0' && *text <= '9') {
            value = value * 10 + (unsigned int)(*text++ - '0');
            if (++digits > 3 || value > 255) return 0;
        }
        if (!digits || (digits > 1 && *start == '0')) return 0;
        out[part] = (unsigned char)value;
        if (part != 3 && *text++ != '.') return 0;
    }
    return *text == 0;
}

int inet_pton(int family, const char *text, void *out) {
    unsigned char bytes[16] = {0};
    if (family == AF_INET) {
        if (!v_ipv4(text, bytes)) return 0;
        memcpy(out, bytes, 4);
        return 1;
    }
    if (family != AF_INET6) { errno = EAFNOSUPPORT; return -1; }
    int used = 0, gap = -1;
    if (*text == ':') {
        if (text[1] != ':') return 0;
        gap = 0;
        text += 2;
    }
    while (*text) {
        const char *start = text;
        unsigned int value = 0;
        int digits = 0;
        for (;;) {
            int ch = (unsigned char)*text, digit;
            if (ch >= '0' && ch <= '9') digit = ch - '0';
            else if (ch >= 'a' && ch <= 'f') digit = ch - 'a' + 10;
            else if (ch >= 'A' && ch <= 'F') digit = ch - 'A' + 10;
            else break;
            if (++digits > 4) return 0;
            value = value * 16 + (unsigned int)digit;
            ++text;
        }
        if (*text == '.') {
            if (used > 12 || !v_ipv4(start, bytes + used)) return 0;
            used += 4;
            text += strlen(text);
            break;
        }
        if (!digits || used > 14) return 0;
        bytes[used++] = (unsigned char)(value >> 8);
        bytes[used++] = (unsigned char)value;
        if (!*text) break;
        if (*text++ != ':') return 0;
        if (*text == ':') {
            if (gap >= 0) return 0;
            gap = used;
            ++text;
        } else if (!*text) return 0;
    }
    if (gap >= 0) {
        if (used >= 16) return 0;
        memmove(bytes + gap + 16 - used, bytes + gap, (size_t)(used - gap));
        memset(bytes + gap, 0, (size_t)(16 - used));
    } else if (used != 16) return 0;
    memcpy(out, bytes, 16);
    return 1;
}



ssize_t sendto(int fd, const void *buffer, size_t size, int flags,
               const struct sockaddr *address, socklen_t length) {
    return (ssize_t)v_linux_result(v_cancel_syscall(SYS_sendto, (uintptr_t)(fd), (uintptr_t)(buffer), (uintptr_t)(size), (uintptr_t)(flags), (uintptr_t)(address), (uintptr_t)(length)));
}
ssize_t recvfrom(int fd, void *buffer, size_t size, int flags,
                 struct sockaddr *address, socklen_t *length) {
    return (ssize_t)v_linux_result(v_cancel_syscall(SYS_recvfrom, (uintptr_t)(fd), (uintptr_t)(buffer), (uintptr_t)(size), (uintptr_t)(flags), (uintptr_t)(address), (uintptr_t)(length)));
}
