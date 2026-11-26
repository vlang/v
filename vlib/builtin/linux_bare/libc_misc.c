#include "libc_compat.h"
#include <arpa/inet.h>
#include <fcntl.h>
#include <limits.h>
#include <pwd.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/utsname.h>
#include <unistd.h>

size_t strnlen(const char *s, size_t n) {
    size_t i = 0;
    while (i < n && s[i])
        ++i;
    return i;
}
void perror(const char *text) {
    int error = errno;
    if (text && *text)
        fprintf(stderr, "%s: ", text);
    fprintf(stderr, "%s\n", strerror(error));
    errno = error;
}
extern uintptr_t v_string_error_r(int32_t, uintptr_t, uintptr_t, int32_t);
#if defined(__GLIBC__) && defined(_GNU_SOURCE)
char *strerror_r(int error, char *buffer, size_t size) {
    return (char *)v_string_error_r(error, (uintptr_t)buffer, size, 1);
}
int __xpg_strerror_r(int error, char *buffer, size_t size) {
    return (int)v_string_error_r(error, (uintptr_t)buffer, size, 0);
}
#else
int strerror_r(int error, char *buffer, size_t size) {
    return (int)v_string_error_r(error, (uintptr_t)buffer, size, 0);
}
#endif
_Noreturn void abort(void) {
    uint64_t mask = 1ULL << (SIGABRT - 1);
    V_RAW4(SYS_rt_sigprocmask, SIG_UNBLOCK, &mask, NULL, 8);
    V_RAW3(SYS_tgkill, V_RAW0(SYS_getpid), V_RAW0(SYS_gettid), SIGABRT);
    /* A handler may return. Restore default disposition and raise again. */
    uintptr_t action[4] = {0};
    V_RAW4(SYS_rt_sigaction, SIGABRT, action, NULL, 8);
    V_RAW3(SYS_tgkill, V_RAW0(SYS_getpid), V_RAW0(SYS_gettid), SIGABRT);
    V_RAW1(SYS_exit_group, 127);
    __builtin_unreachable();
}
_Noreturn void __assert_fail(const char *expression, const char *file, unsigned int line,
                             const char *function) {
    fprintf(stderr, "%s:%u: %s: Assertion `%s' failed.\n", file, line, function, expression);
    abort();
}
extern int32_t v_native_temporary(uintptr_t,int32_t,int32_t,int32_t);
extern uintptr_t v_native_mkdtemp(uintptr_t);
int mkstemp(char *pattern) { return v_native_temporary((uintptr_t)pattern,0,0,0); }
int mkostemp(char *pattern,int flags) { return v_native_temporary((uintptr_t)pattern,0,flags,0); }
int mkstemps(char *pattern,int suffix) { return v_native_temporary((uintptr_t)pattern,suffix,0,0); }
int mkostemps(char *pattern,int suffix,int flags) { return v_native_temporary((uintptr_t)pattern,suffix,flags,0); }
char *mkdtemp(char *pattern) { return (char *)v_native_mkdtemp((uintptr_t)pattern); }
#if defined(__GLIBC__)
extern __typeof__(mkstemp64) mkstemp64 __attribute__((alias("mkstemp")));
extern __typeof__(mkostemp64) mkostemp64 __attribute__((alias("mkostemp")));
extern __typeof__(mkstemps64) mkstemps64 __attribute__((alias("mkstemps")));
extern __typeof__(mkostemps64) mkostemps64 __attribute__((alias("mkostemps")));
#endif
const char *inet_ntop(int family, const void *address, char *buffer, socklen_t size) {
    char text[INET6_ADDRSTRLEN];
    size_t used = 0;
    const unsigned char *a = address;
    if (family == AF_INET)
        used = snprintf(text, sizeof text, "%u.%u.%u.%u", a[0], a[1], a[2], a[3]);
    else if (family == AF_INET6) {
        unsigned int words[8];
        int best = -1, length = 0;
        for (int i = 0; i < 8; ++i)
            words[i] = (unsigned int)a[2 * i] * 256 + a[2 * i + 1];
        for (int i = 0; i < 8;) {
            if (words[i]) {
                ++i;
                continue;
            }
            int start = i;
            while (i < 8 && !words[i])
                ++i;
            if (i - start > length) {
                best = start;
                length = i - start;
            }
        }
        if (length < 2)
            best = -1;
        /* IPv4-mapped addresses use the dotted suffix prescribed by inet_ntop. */
        if (best == 0 && length == 5 && words[5] == 0xffff)
            used = snprintf(text, sizeof text, "::ffff:%u.%u.%u.%u", a[12], a[13], a[14], a[15]);
        else
            for (int i = 0; i < 8;) {
                if (i == best) {
                    text[used++] = ':';
                    text[used++] = ':';
                    i += length;
                    continue;
                }
                if (used && text[used - 1] != ':')
                    text[used++] = ':';
                used += snprintf(text + used, sizeof text - used, "%x", words[i++]);
            }
    } else {
        errno = EAFNOSUPPORT;
        return NULL;
    }
    if (used >= size) {
        errno = ENOSPC;
        return NULL;
    }
    memcpy(buffer, text, used);
    buffer[used] = 0;
    return buffer;
}

int gethostname(char *buffer, size_t size) {
    struct utsname out;
    if (uname(&out) < 0)
        return -1;
    size_t n = strlen(out.nodename) + 1;
    if (n > size) {
        if (size)
            memcpy(buffer, out.nodename, size);
        errno = ENAMETOOLONG;
        return -1;
    }
    memcpy(buffer, out.nodename, n);
    return 0;
}
void arc4random_buf(void *buffer, size_t size) {
    unsigned char *p = buffer;
    while (size) {
        long n = V_RAW3(SYS_getrandom, p, size > 33554431 ? 33554431 : size, 0);
        if (n == -EINTR)
            continue;
        /* This API has no error return; failed entropy must never become
         * predictable output used by its caller for secrets or hash keys. */
        if (n <= 0)
            abort();
        p += n;
        size -= n;
    }
}
uint32_t arc4random(void) {
    uint32_t result;
    arc4random_buf(&result, sizeof result);
    return result;
}
uint32_t arc4random_uniform(uint32_t bound) {
    if (bound < 2)
        return 0;
    uint32_t threshold = (0u - bound) % bound, result;
    do {
        result = arc4random();
    } while (result < threshold);
    return result % bound;
}
struct cmsghdr *__cmsg_nxthdr(struct msghdr *message, struct cmsghdr *current) {
    unsigned char *start = message->msg_control;
    uintptr_t at = (uintptr_t)current, first = (uintptr_t)start;
    if (message->msg_controllen > UINTPTR_MAX - first || at < first)
        return NULL;
    uintptr_t end = first + message->msg_controllen;
    if (at > end || end - at < sizeof *current || current->cmsg_len < sizeof *current ||
        current->cmsg_len > end - at)
        return NULL;
    size_t length = current->cmsg_len;
    if (length > SIZE_MAX - (sizeof(size_t) - 1))
        return NULL;
    length = (length + sizeof(size_t) - 1) & ~(sizeof(size_t) - 1);
    if (length > end - at || end - at - length < sizeof *current)
        return NULL;
    /* A sender initializes the next header after obtaining its address. */
    return (struct cmsghdr *)(at + length);
}

ssize_t getrandom(void *buffer, size_t size, unsigned int flags) {
    return v_linux_result(V_RAW3(SYS_getrandom, buffer, size, flags));
}
int getentropy(void *buffer, size_t size) {
    if (size > 256) { errno = EIO; return -1; }
    unsigned char *p = buffer;
    while (size) {
        long n = V_RAW3(SYS_getrandom, p, size, 0);
        if (n == -EINTR) continue;
        if (n < 0) return (int)v_linux_result(n);
        if (!n) { errno = EIO; return -1; }
        p += n;
        size -= (size_t)n;
    }
    return 0;
}
