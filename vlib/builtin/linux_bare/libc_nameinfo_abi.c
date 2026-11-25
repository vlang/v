#include "libc_compat.h"
#include "libc_nameinfo_abi.h"
#include "libc_resolver.h"
#include <arpa/inet.h>
#include <stddef.h>
#include <string.h>
#include <sys/un.h>
/* Only native socket/configuration layouts are marshalled here. */
struct v_name_address {
    int32_t family;
    uint32_t port, scope, length;
    unsigned char bytes[16];
    char path[109];
};
_Static_assert(sizeof(socklen_t) == sizeof(uint32_t), "Linux socket length");
int32_t v_name_sockaddr(uintptr_t input, uint32_t size, uintptr_t output) {
    const struct sockaddr *sa = (const void *)input;
    struct v_name_address *a = (void *)output;
    memset(a, 0, sizeof(*a));
    if (!sa || size < sizeof(sa->sa_family)) return EAI_FAMILY;
    a->family = sa->sa_family;
    if (sa->sa_family == AF_INET) {
        if (size < sizeof(struct sockaddr_in)) return EAI_FAMILY;
        const struct sockaddr_in *s = (const void *)sa;
        a->port = ntohs(s->sin_port); a->length = 4;
        memcpy(a->bytes, &s->sin_addr, 4);
    } else if (sa->sa_family == AF_INET6) {
        if (size < sizeof(struct sockaddr_in6)) return EAI_FAMILY;
        const struct sockaddr_in6 *s = (const void *)sa;
        a->port = ntohs(s->sin6_port); a->scope = s->sin6_scope_id; a->length = 16;
        memcpy(a->bytes, &s->sin6_addr, 16);
    } else if (sa->sa_family == AF_UNIX) {
        const struct sockaddr_un *s = (const void *)sa;
        if (size < offsetof(struct sockaddr_un, sun_path)) return EAI_FAMILY;
        size_t n = size - offsetof(struct sockaddr_un, sun_path);
        if (n > sizeof(s->sun_path)) n = sizeof(s->sun_path);
        memcpy(a->path, s->sun_path, n);
    } else return EAI_FAMILY;
    return 0;
}
void v_name_paths(uintptr_t input, uintptr_t output) {
    const struct v_resolver_config *p = (const void *)input;
    uintptr_t *fields = (void *)output;
    fields[0] = (uintptr_t)p->hosts_path; fields[1] = (uintptr_t)p->services_path;
}
extern int32_t v_name_getnameinfo(uintptr_t, uint32_t, uintptr_t, uint32_t, uintptr_t, uint32_t, int32_t, uintptr_t);
int getnameinfo(const struct sockaddr *address, socklen_t size, char *host, socklen_t host_size,
                char *service, socklen_t service_size, int flags) {
    return v_name_getnameinfo((uintptr_t)address, size, (uintptr_t)host, host_size,
                             (uintptr_t)service, service_size, flags, 0);
}
int v_freestanding_getnameinfo(const struct sockaddr *address, socklen_t size, char *host,
    socklen_t host_size, char *service, socklen_t service_size, int flags,
    const struct v_resolver_config *paths) {
    return v_name_getnameinfo((uintptr_t)address, size, (uintptr_t)host, host_size,
                             (uintptr_t)service, service_size, flags, (uintptr_t)paths);
}
