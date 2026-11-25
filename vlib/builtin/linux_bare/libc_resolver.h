#ifndef V_LINUX_BARE_RESOLVER_H
#define V_LINUX_BARE_RESOLVER_H
#include <netdb.h>

/* Internal dependency injection: production uses the conventional system
 * files and DNS port 53. Tests supply temporary files and a local DNS server. */
struct v_resolver_config {
    const char *hosts_path;
    const char *services_path;
    const char *resolv_path;
    unsigned short dns_port;
};
int v_freestanding_getaddrinfo(const char *, const char *, const struct addrinfo *,
                              struct addrinfo **, const struct v_resolver_config *);
int v_freestanding_getnameinfo(const struct sockaddr *, socklen_t, char *, socklen_t,
                              char *, socklen_t, int, const struct v_resolver_config *);
#endif
