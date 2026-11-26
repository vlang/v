#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include "probe.h"
#include "../../linux_bare/libc_resolver.h"
#include <arpa/inet.h>
#include <errno.h>
#include <fcntl.h>
#include <netdb.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/syscall.h>
#include <unistd.h>

/* Linker wrapping injects allocation failures into the actual implementation,
 * without product-only test flags, allocator substitutions, or system edits. */
void *__real_malloc(size_t);
void *__real_calloc(size_t, size_t);
void *__real_realloc(void *, size_t);
void __real_free(void *);
static int injecting, fail_at, allocation_number, live, fail_socket;
int __real_socket(int, int, int);
int __wrap_socket(int family, int type, int protocol) {
    if (fail_socket) { errno = EMFILE; return -1; }
    return __real_socket(family, type, protocol);
}
static uintptr_t tracked[512];
static int fail_allocation(void) {
    if (injecting && allocation_number++ == fail_at) { errno = ENOMEM; return 1; }
    return 0;
}
static void remember(void *pointer) {
    if (!injecting || !pointer) return;
    for (unsigned int i = 0; i < sizeof tracked / sizeof *tracked; ++i)
        if (!tracked[i]) { tracked[i] = (uintptr_t)pointer; ++live; return; }
    syscall(SYS_exit_group, 99);
}
static void forget(uintptr_t pointer) {
    if (!pointer) return;
    for (unsigned int i = 0; i < sizeof tracked / sizeof *tracked; ++i)
        if (tracked[i] == pointer) { tracked[i] = 0; --live; return; }
}
void *__wrap_malloc(size_t size) {
    if (fail_allocation()) return NULL;
    void *result = __real_malloc(size); remember(result); return result;
}
void *__wrap_calloc(size_t count, size_t size) {
    if (fail_allocation()) return NULL;
    void *result = __real_calloc(count, size); remember(result); return result;
}
void *__wrap_realloc(void *old, size_t size) {
    if (fail_allocation()) return NULL;
    uintptr_t previous = (uintptr_t)old;
    void *result = __real_realloc(old, size);
    if (result || !size) { forget(previous); remember(result); }
    return result;
}
void __wrap_free(void *pointer) { forget((uintptr_t)pointer); __real_free(pointer); }

static int last_status;
#define CHECK(condition) do { if (!(condition)) { \
    fprintf(stderr, "resolver probe line %d: status %d, errno %d\n", __LINE__, last_status, errno); \
    return __LINE__; } } while (0)

#include "nameinfo_probe.h"

static int descriptor_count(void) {
    int fd = open("/proc/self/fd", O_RDONLY | O_DIRECTORY | O_CLOEXEC), count = 0;
    if (fd < 0) return -1;
    unsigned char buffer[8192];
    for (;;) {
        long size = syscall(SYS_getdents64, fd, buffer, sizeof buffer);
        if (size < 0 && errno == EINTR) continue;
        if (size < 0) { close(fd); return -1; }
        if (!size) break;
        size_t at = 0;
        while (at < (size_t)size) {
            unsigned short length;
            if ((size_t)size - at < 19) { close(fd); return -1; }
            memcpy(&length, buffer + at + 16, sizeof length);
            if (length < 19 || length > (size_t)size - at) { close(fd); return -1; }
            ++count; at += length;
        }
    }
    close(fd);
    return count;
}
static int addresses(const struct addrinfo *result, int family, const char *ip, unsigned short port) {
    unsigned char expected[16];
    if (inet_pton(family, ip, expected) != 1) return 0;
    for (; result; result = result->ai_next) {
        if (result->ai_family != family) continue;
        const void *bytes;
        const unsigned char *service;
        if (family == AF_INET) {
            const struct sockaddr_in *sa = (const void *)result->ai_addr;
            bytes = &sa->sin_addr; service = (const void *)&sa->sin_port;
        } else {
            const struct sockaddr_in6 *sa = (const void *)result->ai_addr;
            bytes = &sa->sin6_addr; service = (const void *)&sa->sin6_port;
        }
        if (!memcmp(bytes, expected, family == AF_INET ? 4 : 16) && service[0] == (port >> 8) && service[1] == (port & 255)) return 1;
    }
    return 0;
}
static int list_size(const struct addrinfo *result) { int count = 0; for (; result; result = result->ai_next) ++count; return count; }

int v_freestanding_resolver_check(const char *directory, unsigned short port, int configured) {
    char hosts[1024], services[1024], resolv[1024];
    CHECK(snprintf(hosts, sizeof hosts, "%s/hosts", directory) > 0);
    CHECK(snprintf(services, sizeof services, "%s/services", directory) > 0);
    CHECK(snprintf(resolv, sizeof resolv, "%s/resolv.conf", directory) > 0);
    struct v_resolver_config paths = {hosts, services, resolv, port};
    int nameinfo_code = nameinfo_numeric_probe();
    if (!nameinfo_code) nameinfo_code = nameinfo_reverse_probe(&paths);
    CHECK(nameinfo_code == 0);
    struct addrinfo hints = {.ai_family = AF_UNSPEC, .ai_socktype = SOCK_STREAM, .ai_flags = AI_CANONNAME}, *result;
#define RESOLVE(name, service) (last_status = v_freestanding_getaddrinfo((name), (service), &hints, &result, &paths))
    CHECK(RESOLVE("MiXeDaLiAs", "webalias") == 0);
    CHECK(list_size(result) == 2 && addresses(result, AF_INET, "192.0.2.9", 4242) && addresses(result, AF_INET6, "2001:db8::9", 4242));
    CHECK(result->ai_canonname && !strcmp(result->ai_canonname, "canonical.host"));
    freeaddrinfo(result);
    CHECK(RESOLVE("many.example.test", "4242") == 0 && list_size(result) == 130);
    freeaddrinfo(result);
    hints.ai_family = AF_INET;
    CHECK(RESOLVE("a.example.test", "4242") == 0 && addresses(result, AF_INET, "192.0.2.11", 4242));
    freeaddrinfo(result);
    CHECK(RESOLVE("a.example.test.", "4242") == 0 && addresses(result, AF_INET, "192.0.2.11", 4242));
    freeaddrinfo(result);
    CHECK(RESOLVE("search", "4242") == 0 && addresses(result, AF_INET, "192.0.2.13", 4242));
    freeaddrinfo(result);
    hints.ai_family = AF_UNSPEC;
    CHECK(RESOLVE("dual.example.test", "4242") == 0 && list_size(result) == 2);
    CHECK(addresses(result, AF_INET, "192.0.2.12", 4242) && addresses(result, AF_INET6, "2001:db8::12", 4242));
    freeaddrinfo(result);
    hints.ai_family = AF_INET;
    CHECK(RESOLVE("cname.example.test", "4242") == 0 && addresses(result, AF_INET, "192.0.2.14", 4242));
    CHECK(result->ai_canonname && !strcmp(result->ai_canonname, "canonical.example.test"));
    freeaddrinfo(result);
    CHECK(RESOLVE("chain.example.test", "4242") == 0 && addresses(result, AF_INET, "192.0.2.15", 4242));
    freeaddrinfo(result);
    CHECK(RESOLVE("truncated.example.test", "4242") == 0 && addresses(result, AF_INET, "192.0.2.16", 4242));
    freeaddrinfo(result);
    CHECK(RESOLVE("wrongid.example.test", "4242") == 0 && addresses(result, AF_INET, "192.0.2.17", 4242));
    freeaddrinfo(result);
    CHECK(RESOLVE("wrongquestion.example.test", "4242") == 0 && addresses(result, AF_INET, "192.0.2.18", 4242));
    freeaddrinfo(result);
    CHECK(RESOLVE("nxdomain.example.test", "4242") == EAI_NONAME && result == NULL);
    CHECK(RESOLVE("servfail.example.test", "4242") == EAI_AGAIN && result == NULL);
    CHECK(RESOLVE("malformed.example.test", "4242") == EAI_FAIL && result == NULL);
    CHECK(RESOLVE("cycle.example.test", "4242") == EAI_FAIL && result == NULL);
    CHECK(RESOLVE("badlength.example.test", "4242") == EAI_FAIL && result == NULL);
    CHECK(RESOLVE("timeout.example.test.", "4242") == EAI_AGAIN && result == NULL);
    fail_socket = 1;
    int socket_error = RESOLVE("a.example.test.", "4242");
    fail_socket = 0;
    CHECK(socket_error == EAI_SYSTEM && result == NULL && errno == EMFILE);

    hints.ai_family = AF_INET6; hints.ai_flags = AI_V4MAPPED;
    CHECK(RESOLVE("127.1", "4242") == 0 && addresses(result, AF_INET6, "::ffff:127.0.0.1", 4242));
    freeaddrinfo(result);
    hints.ai_flags = AI_V4MAPPED | AI_ALL;
    CHECK(RESOLVE("dual.example.test", "4242") == 0 && list_size(result) == 2);
    CHECK(addresses(result, AF_INET6, "::ffff:192.0.2.12", 4242) && addresses(result, AF_INET6, "2001:db8::12", 4242));
    freeaddrinfo(result);
    hints.ai_flags = AI_NUMERICHOST;
    CHECK(RESOLVE("fe80::1%3", "4242") == 0 && ((struct sockaddr_in6 *)result->ai_addr)->sin6_scope_id == 3);
    freeaddrinfo(result);
    CHECK(RESOLVE("fe80::1%lo", "4242") == 0 && ((struct sockaddr_in6 *)result->ai_addr)->sin6_scope_id > 0);
    freeaddrinfo(result);
    hints.ai_family = AF_INET;
    CHECK(RESOLVE("127.0.0.1", "udp-only") == EAI_SERVICE && result == NULL);
    hints.ai_flags = AI_NUMERICSERV;
    CHECK(RESOLVE("127.0.0.1", "webalias") == EAI_NONAME && result == NULL);
    hints.ai_flags = AI_ADDRCONFIG;
    int expected = configured & 1 ? 0 : EAI_NONAME;
    CHECK(RESOLVE("127.0.0.1", "4242") == expected);
    freeaddrinfo(result);
    hints.ai_family = AF_INET6;
    expected = configured & 2 ? 0 : EAI_NONAME;
    CHECK(RESOLVE("::1", "4242") == expected);
    freeaddrinfo(result);

    hints = (struct addrinfo){.ai_family = AF_UNSPEC};
    int descriptors = descriptor_count();
    CHECK(descriptors >= 0);
    const struct { const char *name, *service; int count, limits; } cases[] = {
        {"unit-alias", "webtest", 4, 32},
        {"many.example.test", "4242", 260, 320},
        {"a.example.test.", "4242", 2, 32}
    };
    for (unsigned int scenario = 0; scenario < sizeof cases / sizeof *cases; ++scenario) {
        int successes = 0, failures = 0;
        for (int limit = 0; limit < cases[scenario].limits; ++limit) {
            fail_at = limit; allocation_number = live = 0;
            memset(tracked, 0, sizeof tracked);
            injecting = 1;
            int code = RESOLVE(cases[scenario].name, cases[scenario].service);
            injecting = 0;
            if (code) { ++failures; CHECK(code == EAI_MEMORY && result == NULL); }
            else { ++successes; CHECK(list_size(result) == cases[scenario].count); freeaddrinfo(result); }
            CHECK(live == 0 && descriptor_count() == descriptors);
        }
        CHECK(successes > 0 && failures > 0);
    }
    return 0;
}
