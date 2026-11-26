#ifndef V_RUNTIME_NAMEINFO_PROBE_H
#define V_RUNTIME_NAMEINFO_PROBE_H
#include <net/if.h>
#include <sys/un.h>
#include <stddef.h>

static int nameinfo_numeric_probe(void) {
    struct sockaddr_in v4 = {.sin_family = AF_INET, .sin_port = htons(4242)};
    struct sockaddr_in6 v6 = {.sin6_family = AF_INET6, .sin6_port = htons(4343)};
    char host[256], service[256], interface[IF_NAMESIZE], expected[256];
    CHECK(inet_pton(AF_INET, "192.0.2.201", &v4.sin_addr) == 1);
    errno = EDOM;
    CHECK(!getnameinfo((struct sockaddr *)&v4, sizeof(v4), host, sizeof(host), service, sizeof(service), NI_NUMERICHOST | NI_NUMERICSERV));
    CHECK(!strcmp(host, "192.0.2.201") && !strcmp(service, "4242") && errno == EDOM);
    memset(host, 'X', sizeof(host));
    CHECK(getnameinfo((struct sockaddr *)&v4, sizeof(v4), host, 4, NULL, 0, NI_NUMERICHOST) == EAI_OVERFLOW && host[4] == 'X');
    CHECK(getnameinfo((struct sockaddr *)&v4, sizeof(v4) - 1, host, sizeof(host), NULL, 0, NI_NUMERICHOST) == EAI_FAMILY);
    CHECK(getnameinfo((struct sockaddr *)&v4, sizeof(v4), host, sizeof(host), NULL, 0, 1 << 30) == EAI_BADFLAGS);
    CHECK(inet_pton(AF_INET6, "fe80::c9", &v6.sin6_addr) == 1);
    unsigned loopback = if_nametoindex("lo"); CHECK(loopback);
    CHECK(if_indextoname(loopback, interface) == interface && !strcmp(interface, "lo"));
    v6.sin6_scope_id = loopback;
    CHECK(!getnameinfo((struct sockaddr *)&v6, sizeof(v6), host, sizeof(host), service, sizeof(service), NI_NUMERICHOST | NI_NUMERICSERV));
    snprintf(expected, sizeof(expected), "fe80::c9%%%s", interface);
    CHECK(!strcmp(host, expected) && !strcmp(service, "4343"));
    CHECK(inet_pton(AF_INET6, "2001:db8::c9", &v6.sin6_addr) == 1);
    v6.sin6_scope_id = 123456789;
    CHECK(!getnameinfo((struct sockaddr *)&v6, sizeof(v6), host, sizeof(host), NULL, 0, NI_NUMERICHOST));
    CHECK(!strcmp(host, "2001:db8::c9%123456789"));
    struct sockaddr_un local = {.sun_family = AF_UNIX};
    strcpy(local.sun_path, "/private/name info socket");
    CHECK(!getnameinfo((struct sockaddr *)&local, sizeof(local), NULL, 0, service, sizeof(service), 0));
    CHECK(!strcmp(service, local.sun_path));
    puts("numeric nameinfo, IPv6 scopes and Unix sockets passed");
    return 0;
}

static int nameinfo_reverse_probe(const struct v_resolver_config *paths) {
    struct sockaddr_in v4 = {.sin_family = AF_INET, .sin_port = htons(4242)};
    struct sockaddr_in6 v6 = {.sin6_family = AF_INET6, .sin6_port = htons(4343)};
    char host[256], service[256];
#define REVERSE(flags) (last_status = v_freestanding_getnameinfo((struct sockaddr *)&v4, sizeof(v4), host, sizeof(host), service, sizeof(service), (flags), paths))
    CHECK(inet_pton(AF_INET, "192.0.2.9", &v4.sin_addr) == 1);
    errno = EDOM;
    CHECK(!REVERSE(0) && !strcmp(host, "canonical.host") && !strcmp(service, "webtest") && errno == EDOM);
    CHECK(!REVERSE(NI_NOFQDN) && !strcmp(host, "canonical.host"));
    CHECK(inet_pton(AF_INET6, "::ffff:192.0.2.9", &v6.sin6_addr) == 1);
    CHECK(!v_freestanding_getnameinfo((struct sockaddr *)&v6, sizeof(v6), host, sizeof(host), NULL, 0, NI_NAMEREQD, paths) && !strcmp(host, "canonical.host"));
    const struct { const char *ip, *name; } cases[] = {
        {"192.0.2.201", "reverse.example.test"}, {"192.0.2.202", "reverse-chain.example.test"},
        {"192.0.2.203", "reverse-tcp.example.test"}
    };
    for (unsigned i = 0; i < sizeof(cases) / sizeof(*cases); ++i) {
        CHECK(inet_pton(AF_INET, cases[i].ip, &v4.sin_addr) == 1);
        CHECK(!REVERSE(NI_NAMEREQD) && !strcmp(host, cases[i].name));
    }
    CHECK(inet_pton(AF_INET6, "2001:db8::201", &v6.sin6_addr) == 1);
    CHECK(!v_freestanding_getnameinfo((struct sockaddr *)&v6, sizeof(v6), host, sizeof(host), service, sizeof(service), NI_NAMEREQD | NI_DGRAM, paths));
    CHECK(!strcmp(host, "reverse6.example.test") && !strcmp(service, "udp-only"));
    const struct { const char *ip; int code; } errors[] = {
        {"192.0.2.204", EAI_FAIL}, {"192.0.2.205", EAI_NONAME},
        {"192.0.2.206", EAI_FAIL}, {"192.0.2.207", EAI_FAIL}, {"192.0.2.208", EAI_AGAIN}
    };
    for (unsigned i = 0; i < sizeof(errors) / sizeof(*errors); ++i) {
        CHECK(inet_pton(AF_INET, errors[i].ip, &v4.sin_addr) == 1);
        CHECK(REVERSE(NI_NAMEREQD) == errors[i].code);
    }
    CHECK(inet_pton(AF_INET, "192.0.2.205", &v4.sin_addr) == 1);
    CHECK(!REVERSE(0) && !strcmp(host, "192.0.2.205"));
    CHECK(inet_pton(AF_INET, "192.0.2.201", &v4.sin_addr) == 1);
    fail_socket = 1; REVERSE(NI_NAMEREQD); fail_socket = 0;
    CHECK(last_status == EAI_SYSTEM && errno == EMFILE);
    puts("reverse nameinfo, hosts, services, PTR, CNAME and truncated TCP passed");
#undef REVERSE
    return 0;
}
#endif
