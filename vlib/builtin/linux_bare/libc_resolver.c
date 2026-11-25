#include "libc_compat.h"
#include "libc_resolver.h"
#include "libc_nameinfo_abi.h"
#include "libc_cancel_abi.h"
#include <arpa/inet.h>
#include <fcntl.h>
#include <limits.h>
#include <net/if.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <time.h>
#include <unistd.h>

#define V_DNS_SERVERS 3
#define V_DNS_SEARCH 6
#define V_DNS_ALIAS 1000
struct v_address { int family; unsigned int scope; unsigned char bytes[16]; };
struct v_address_array { struct v_address *data; int count; size_t capacity; };
struct v_resolver {
    struct v_address servers[V_DNS_SERVERS];
    int server_count, timeout, attempts, ndots, search_count;
    unsigned short port;
    char search[V_DNS_SEARCH][256];
};
struct v_addrinfo { struct addrinfo info; struct sockaddr_storage address; char canonical[256]; };
static int v_hostname(const char *name);

static int v_lower(int ch) { return ch >= 'A' && ch <= 'Z' ? ch + ('a' - 'A') : ch; }
static int v_equal(const char *a, const char *b) {
    while (*a && v_lower((unsigned char)*a) == v_lower((unsigned char)*b)) { ++a; ++b; }
    return *a == *b;
}
static int v_space(int ch) { return ch == ' ' || ch == '\t' || ch == '\r' || ch == '\n' || ch == '\v' || ch == '\f'; }
static char *v_token(char **position) {
    char *start = *position;
    while (v_space((unsigned char)*start)) ++start;
    if (!*start || *start == '#' || *start == ';') return NULL;
    char *end = start;
    while (*end && !v_space((unsigned char)*end) && *end != '#' && *end != ';') ++end;
    if (*end == '#' || *end == ';') { *end = 0; *position = end; }
    else if (*end) { *end = 0; *position = end + 1; }
    else *position = end;
    return start;
}
static int v_decimal(const char *text, unsigned int limit, unsigned int *value) {
    unsigned int result = 0;
    if (!*text) return 0;
    for (; *text; ++text) {
        if (*text < '0' || *text > '9' || (unsigned int)(*text - '0') > limit ||
            result > (limit - (unsigned int)(*text - '0')) / 10) return 0;
        result = result * 10 + (unsigned int)(*text - '0');
        if (result > limit) return 0;
    }
    *value = result;
    return 1;
}
static int v_legacy_ipv4(const char *text, unsigned char out[4]) {
    uint32_t parts[4];
    unsigned int count = 0;
    for (;;) {
        if (count == 4 || *text < '0' || *text > '9') return 0;
        unsigned int base = 10, digits = 0;
        uint64_t value = 0;
        if (*text == '0') {
            base = 8;
            if (text[1] == 'x' || text[1] == 'X') { base = 16; text += 2; }
        }
        while (*text) {
            unsigned int digit = *text >= '0' && *text <= '9' ? (unsigned int)(*text - '0') :
                v_lower((unsigned char)*text) >= 'a' && v_lower((unsigned char)*text) <= 'f' ?
                (unsigned int)(v_lower((unsigned char)*text) - 'a' + 10) : 16;
            if (digit >= base) break;
            value = value * base + digit;
            if (value > UINT32_MAX) return 0;
            ++text; ++digits;
        }
        if (!digits) return 0;
        parts[count++] = (uint32_t)value;
        if (*text != '.') break;
        ++text;
    }
    if (*text) return 0;
    uint32_t value = parts[count - 1];
    if (count > 1 && value >= (UINT32_C(1) << (8 * (5 - count)))) return 0;
    for (unsigned int i = 0; i + 1 < count; ++i) {
        if (parts[i] > 255) return 0;
        value |= parts[i] << (24 - 8 * i);
    }
    for (unsigned int i = 0; i < 4; ++i) out[i] = (unsigned char)(value >> (24 - 8 * i));
    return 1;
}
static int v_ip_literal(const char *name, struct v_address *address) {
    memset(address, 0, sizeof *address);
    if (v_legacy_ipv4(name, address->bytes)) { address->family = AF_INET; return 1; }
    char host[256];
    size_t size = strlen(name);
    if (size >= sizeof host) return 0;
    memcpy(host, name, size + 1);
    char *scope = strchr(host, '%');
    if (scope) *scope++ = 0;
    if (inet_pton(AF_INET6, host, address->bytes) != 1) return 0;
    address->family = AF_INET6;
    if (scope && !v_decimal(scope, UINT32_MAX, &address->scope)) {
        if (!*scope || strlen(scope) >= IFNAMSIZ) return 0;
        int fd = socket(AF_INET, SOCK_DGRAM | SOCK_CLOEXEC, 0);
        if (fd < 0) return 0;
        struct ifreq request;
        memset(&request, 0, sizeof request);
        memcpy(request.ifr_name, scope, strlen(scope) + 1);
        long result = V_RAW3(SYS_ioctl, fd, SIOCGIFINDEX, &request);
        close(fd);
        if (result < 0) return 0;
        address->scope = (unsigned int)request.ifr_ifindex;
    }
    return 1;
}
static int v_add_address(struct v_address_array *addresses, const struct v_address *address) {
    for (int i = 0; i < addresses->count; ++i)
        if (addresses->data[i].family == address->family && addresses->data[i].scope == address->scope &&
            !memcmp(addresses->data[i].bytes, address->bytes, address->family == AF_INET ? 4 : 16)) return 0;
    if ((size_t)addresses->count == addresses->capacity) {
        if (addresses->capacity > (size_t)INT_MAX / 2 || addresses->capacity > SIZE_MAX / 2 / sizeof *addresses->data)
            return EAI_MEMORY;
        size_t capacity = addresses->capacity ? addresses->capacity * 2 : 8;
        struct v_address *replacement = realloc(addresses->data, capacity * sizeof *replacement);
        if (!replacement) return EAI_MEMORY;
        addresses->data = replacement; addresses->capacity = capacity;
    }
    addresses->data[addresses->count++] = *address;
    return 0;
}
static socklen_t v_sockaddr(const struct v_address *address, unsigned short port, struct sockaddr_storage *storage) {
    memset(storage, 0, sizeof *storage);
    unsigned char *destination;
    if (address->family == AF_INET) {
        struct sockaddr_in *sa = (void *)storage;
        sa->sin_family = AF_INET;
        destination = (void *)&sa->sin_port;
        memcpy(&sa->sin_addr, address->bytes, 4);
    } else {
        struct sockaddr_in6 *sa = (void *)storage;
        sa->sin6_family = AF_INET6;
        sa->sin6_scope_id = address->scope;
        destination = (void *)&sa->sin6_port;
        memcpy(&sa->sin6_addr, address->bytes, 16);
    }
    destination[0] = (unsigned char)(port >> 8); destination[1] = (unsigned char)port;
    return address->family == AF_INET ? sizeof(struct sockaddr_in) : sizeof(struct sockaddr_in6);
}
static int v_configure(struct v_resolver *resolver, const struct v_resolver_config *paths) {
    memset(resolver, 0, sizeof *resolver);
    resolver->timeout = 5; resolver->attempts = 2; resolver->ndots = 1;
    resolver->port = paths->dns_port;
    FILE *file = fopen(paths->resolv_path, "r");
    if (!file && errno != ENOENT && errno != ENOTDIR && errno != EACCES) return EAI_SYSTEM;
    char *line = NULL;
    size_t capacity = 0;
    if (file) {
        while (getline(&line, &capacity, file) >= 0) {
            char *cursor = line, *kind = v_token(&cursor), *value = v_token(&cursor);
            if (!kind || !value) continue;
            if (!strcmp(kind, "nameserver") && resolver->server_count < V_DNS_SERVERS) {
                if (v_ip_literal(value, &resolver->servers[resolver->server_count])) ++resolver->server_count;
            } else if (!strcmp(kind, "search") || !strcmp(kind, "domain")) {
                resolver->search_count = 0;
                do {
                    size_t size = strlen(value);
                    if (size && size < 254 && resolver->search_count < V_DNS_SEARCH)
                        memcpy(resolver->search[resolver->search_count++], value, size + 1);
                    if (!strcmp(kind, "domain")) break;
                } while ((value = v_token(&cursor)) != NULL);
            } else if (!strcmp(kind, "options")) {
                do {
                    char *colon = strchr(value, ':');
                    unsigned int number;
                    if (!colon || !v_decimal(colon + 1, UINT32_MAX, &number)) continue;
                    *colon = 0;
                    if (!strcmp(value, "timeout")) resolver->timeout = number ? (number > 30 ? 30 : (int)number) : 1;
                    if (!strcmp(value, "attempts")) resolver->attempts = number ? (number > 5 ? 5 : (int)number) : 1;
                    if (!strcmp(value, "ndots")) resolver->ndots = number > 15 ? 15 : (int)number;
                } while ((value = v_token(&cursor)) != NULL);
            }
        }
        int failed = ferror(file), error = errno;
        free(line); fclose(file);
        if (failed) { errno = error; return EAI_SYSTEM; }
    }
    if (!resolver->server_count) {
        resolver->server_count = 1;
        resolver->servers[0].family = AF_INET;
        resolver->servers[0].bytes[0] = 127; resolver->servers[0].bytes[3] = 1;
    }
    return 0;
}
static int64_t v_dns_now(void) {
    struct timespec time;
    if (V_RAW2(SYS_clock_gettime, CLOCK_MONOTONIC, &time) < 0) return -1;
    return (int64_t)time.tv_sec * 1000 + time.tv_nsec / 1000000;
}
static int v_dns_wait(int fd, short events, int64_t deadline) {
    for (;;) {
        int64_t now = v_dns_now(), remaining = deadline - now;
        if (now < 0) { errno = EIO; return -1; }
        if (remaining <= 0) { errno = ETIMEDOUT; return -1; }
        struct pollfd descriptor = {.fd = fd, .events = events};
        struct timespec timeout = {.tv_sec = remaining / 1000, .tv_nsec = remaining % 1000 * 1000000};
        long result = V_RAW5(SYS_ppoll, &descriptor, 1, &timeout, 0, 8);
        if (result == -EINTR) continue;
        if (result < 0) { v_linux_result(result); return -1; }
        if (!result) { errno = ETIMEDOUT; return -1; }
        return 0;
    }
}
int v_resolver_wait_readable(int fd, int64_t deadline) {
    return v_dns_wait(fd, POLLIN, deadline);
}
static int v_dns_transfer(int fd, unsigned char *data, size_t size, int sending, int64_t deadline) {
    size_t done = 0;
    while (done < size) {
        long result = sending ? send(fd, data + done, size - done, MSG_NOSIGNAL) : recv(fd, data + done, size - done, 0);
        if (result > 0) { done += (size_t)result; continue; }
        if (!result) { errno = ECONNRESET; return -1; }
        if (errno == EINTR) continue;
        if ((errno == EAGAIN || errno == EWOULDBLOCK) && !v_dns_wait(fd, sending ? POLLOUT : POLLIN, deadline)) continue;
        return -1;
    }
    return 0;
}
static unsigned int v_dns_u16(const unsigned char *bytes) { return (unsigned int)bytes[0] * 256 + bytes[1]; }
static int v_dns_name(const unsigned char *packet, size_t size, size_t *cursor, char out[256]) {
    size_t at = *cursor, end = 0, used = 0;
    for (unsigned int steps = 0; steps < 128; ++steps) {
        if (at >= size) return -1;
        unsigned int length = packet[at++];
        if ((length & 192) == 192) {
            if (at >= size) return -1;
            if (!end) end = at + 1;
            at = ((length & 63) << 8) | packet[at];
            continue;
        }
        if (length & 192) return -1;
        if (!length) { out[used] = 0; *cursor = end ? end : at; return 0; }
        if (at + length > size || used + length + !!used > 253) return -1;
        if (used) out[used++] = '.';
        for (unsigned int i = 0; i < length; ++i) {
            unsigned int ch = packet[at++];
            if (!ch || ch == '.') return -1;
            out[used++] = (char)ch;
        }
    }
    return -1;
}
static int v_dns_record(const unsigned char *packet, size_t size, size_t *cursor,
                         char owner[256], unsigned int *type, size_t *data, size_t *length) {
    if (v_dns_name(packet, size, cursor, owner) < 0 || size - *cursor < 10) return -1;
    *type = v_dns_u16(packet + *cursor);
    unsigned int class = v_dns_u16(packet + *cursor + 2);
    *length = v_dns_u16(packet + *cursor + 8);
    *data = *cursor + 10;
    if (*length > size - *data) return -1;
    *cursor = *data + *length;
    if (class != 1) *type = 0;
    return 0;
}
static int v_dns_parse(const unsigned char *packet, size_t size, unsigned int id,
                       const char *query, unsigned int wanted, struct v_address_array *addresses, char canonical[256]) {
    if (size < 12 || v_dns_u16(packet) != id || !(packet[2] & 128) || (packet[2] & 120) ||
        v_dns_u16(packet + 4) != 1) return EAI_FAIL;
    size_t cursor = 12;
    char owner[256];
    if (v_dns_name(packet, size, &cursor, owner) < 0 || size - cursor < 4 || !v_equal(owner, query) ||
        v_dns_u16(packet + cursor) != wanted || v_dns_u16(packet + cursor + 2) != 1) return EAI_FAIL;
    cursor += 4;
    if (packet[2] & 2) return -V_DNS_ALIAS; /* Retry truncated UDP over TCP. */
    unsigned int code = packet[3] & 15;
    if (code == 3) return EAI_NONAME;
    if (code == 2 || code == 5) return EAI_AGAIN;
    if (code) return EAI_FAIL;
    unsigned int answers = v_dns_u16(packet + 6);
    unsigned int all = answers + v_dns_u16(packet + 8) + v_dns_u16(packet + 10);
    size_t answer_start = cursor, data, length;
    unsigned int type;
    for (unsigned int i = 0; i < all; ++i)
        if (v_dns_record(packet, size, &cursor, owner, &type, &data, &length) < 0) return EAI_FAIL;
    memcpy(canonical, query, strlen(query) + 1);
    char seen[16][256];
    int aliases = 0;
    for (;;) {
        int found = 0;
        cursor = answer_start;
        for (unsigned int i = 0; i < answers; ++i) {
            if (v_dns_record(packet, size, &cursor, owner, &type, &data, &length) < 0) return EAI_FAIL;
            if (type != 5 || !v_equal(owner, canonical)) continue;
            if (aliases == 16) return EAI_FAIL;
            memcpy(seen[aliases++], canonical, strlen(canonical) + 1);
            size_t target = data;
            if (v_dns_name(packet, size, &target, canonical) < 0 || target != data + length || !v_hostname(canonical)) return EAI_FAIL;
            for (int j = 0; j < aliases; ++j) if (v_equal(seen[j], canonical)) return EAI_FAIL;
            found = 1; break;
        }
        if (!found) break;
    }
    int before = addresses->count;
    cursor = answer_start;
    for (unsigned int i = 0; i < answers; ++i) {
        if (v_dns_record(packet, size, &cursor, owner, &type, &data, &length) < 0) { addresses->count = before; return EAI_FAIL; }
        if (type != wanted || !v_equal(owner, canonical)) continue;
        if (length != (wanted == 1 ? 4 : 16)) { addresses->count = before; return EAI_FAIL; }
        struct v_address address = {.family = wanted == 1 ? AF_INET : AF_INET6};
        memcpy(address.bytes, packet + data, length);
        int allocation = v_add_address(addresses, &address);
        if (allocation) { addresses->count = before; return allocation; }
    }
    return addresses->count > before ? 0 : aliases ? V_DNS_ALIAS : EAI_NODATA;
}
static int v_dns_question(unsigned char query[272], const char *name, unsigned int type) {
    unsigned short id;
    long random = V_RAW3(SYS_getrandom, &id, sizeof id, 0);
    if (random != sizeof id) {
        int fd = open("/dev/urandom", O_RDONLY | O_CLOEXEC);
        if (fd < 0) return -1;
        ssize_t read_size;
        do { read_size = read(fd, &id, sizeof id); } while (read_size < 0 && errno == EINTR);
        int error = errno; close(fd); errno = error;
        if (read_size != sizeof id) { errno = EIO; return -1; }
    }
    memset(query, 0, 12);
    query[0] = (unsigned char)(id >> 8); query[1] = (unsigned char)id;
    query[2] = 1; query[5] = 1;
    size_t used = 12;
    const char *part = name;
    while (*part) {
        const char *end = part;
        while (*end && *end != '.') ++end;
        size_t length = (size_t)(end - part);
        if (!length || length > 63 || used + length + 6 > 272) { errno = EINVAL; return -1; }
        query[used++] = (unsigned char)length;
        memcpy(query + used, part, length); used += length;
        part = *end ? end + 1 : end;
    }
    query[used++] = 0;
    query[used++] = (unsigned char)(type >> 8); query[used++] = (unsigned char)type;
    query[used++] = 0; query[used++] = 1;
    return (int)used;
}
static int v_dns_exchange(struct v_resolver *resolver, const struct v_address *server,
                          const unsigned char *query, size_t query_size, unsigned char *reply,
                          int tcp, int64_t deadline) {
    int fd = socket(server->family, (tcp ? SOCK_STREAM : SOCK_DGRAM) | SOCK_NONBLOCK | SOCK_CLOEXEC, 0);
    if (fd < 0) return -1;
    struct sockaddr_storage address;
    socklen_t address_size = v_sockaddr(server, resolver->port, &address);
    int size = -1;
    if (connect(fd, (struct sockaddr *)&address, address_size) < 0) {
        if (errno != EINPROGRESS || v_dns_wait(fd, POLLOUT, deadline) < 0) goto done;
        int error = 0; socklen_t length = sizeof error;
        if (getsockopt(fd, SOL_SOCKET, SO_ERROR, &error, &length) < 0) goto done;
        if (error) { errno = error; goto done; }
    }
    if (tcp) {
        unsigned char prefix[2] = {(unsigned char)(query_size >> 8), (unsigned char)query_size};
        if (v_dns_transfer(fd, prefix, 2, 1, deadline) < 0 ||
            v_dns_transfer(fd, (unsigned char *)query, query_size, 1, deadline) < 0 ||
            v_dns_transfer(fd, prefix, 2, 0, deadline) < 0) goto done;
        size = (int)v_dns_u16(prefix);
        if (size < 12) { errno = EPROTO; size = -1; }
        else if (v_dns_transfer(fd, reply, (size_t)size, 0, deadline) < 0) size = -1;
    } else {
        if (v_dns_transfer(fd, (unsigned char *)query, query_size, 1, deadline) < 0) goto done;
        for (;;) {
            long received = recv(fd, reply, 65535, 0);
            if (received >= 12 && v_dns_u16(reply) == v_dns_u16(query)) {
                char expected[256], actual[256];
                size_t a = 12, b = 12;
                int query_ok = !v_dns_name(query, query_size, &a, expected);
                int reply_ok = !v_dns_name(reply, (size_t)received, &b, actual);
                if (query_ok && reply_ok && query_size - a >= 4 && (size_t)received - b >= 4 &&
                    (!v_equal(expected, actual) || memcmp(query + a, reply + b, 4))) continue;
                size = (int)received; break;
            }
            if (received < 0 && errno != EAGAIN && errno != EWOULDBLOCK && errno != EINTR) break;
            if (v_dns_wait(fd, POLLIN, deadline) < 0) break;
        }
    }
done:;
    int error = errno;
    close(fd);
    errno = error;
    return size;
}
static int v_dns_io_error(void) {
    switch (errno) {
    case ETIMEDOUT: case EAGAIN: case EINTR: case ECONNREFUSED: case ECONNRESET:
    case ENETDOWN: case ENETUNREACH: case EHOSTUNREACH: case EPIPE:
        return EAI_AGAIN;
    case ENOMEM: return EAI_MEMORY;
    case EPROTO: return EAI_FAIL;
    default: return EAI_SYSTEM;
    }
}
static int v_dns_lookup(struct v_resolver *resolver, const char *name, unsigned int type,
                        struct v_address_array *addresses, char canonical[256]) {
    unsigned char query[272];
    unsigned char *reply = malloc(65535);
    if (!reply) return EAI_MEMORY;
    char current[256], seen[16][256];
    memcpy(current, name, strlen(name) + 1);
    int result = EAI_AGAIN;
    for (int aliases = 0; aliases < 16; ++aliases) {
        memcpy(seen[aliases], current, strlen(current) + 1);
        int query_size = v_dns_question(query, current, type);
        if (query_size < 0) { result = errno == EINVAL ? EAI_NONAME : EAI_SYSTEM; break; }
        int found_alias = 0;
        for (int attempt = 0; attempt < resolver->attempts && !found_alias; ++attempt) {
            for (int server = 0; server < resolver->server_count; ++server) {
                int64_t now = v_dns_now();
                if (now < 0) { result = EAI_SYSTEM; goto done; }
                int64_t deadline = now + resolver->timeout * 1000;
                int size = v_dns_exchange(resolver, &resolver->servers[server], query, (size_t)query_size, reply, 0, deadline);
                if (size < 0) {
                    result = v_dns_io_error();
                    if (result == EAI_MEMORY || result == EAI_SYSTEM) goto done;
                    continue;
                }
                result = v_dns_parse(reply, (size_t)size, v_dns_u16(query), current, type, addresses, canonical);
                if (result == -V_DNS_ALIAS) {
                    size = v_dns_exchange(resolver, &resolver->servers[server], query, (size_t)query_size, reply, 1, deadline);
                    result = size < 0 ? v_dns_io_error() : v_dns_parse(reply, (size_t)size, v_dns_u16(query), current, type, addresses, canonical);
                    if (result == -V_DNS_ALIAS) result = EAI_FAIL;
                }
                if (result == EAI_MEMORY || result == EAI_SYSTEM) goto done;
                if (result == 0 || result == EAI_NONAME || result == EAI_NODATA) goto done;
                if (result == V_DNS_ALIAS) {
                    for (int j = 0; j <= aliases; ++j) if (v_equal(seen[j], canonical)) { result = EAI_FAIL; goto done; }
                    memcpy(current, canonical, strlen(canonical) + 1);
                    found_alias = 1; break;
                }
            }
        }
        if (!found_alias) break;
        result = EAI_FAIL;
    }
done:
    free(reply);
    return result;
}

static int v_hostname(const char *name) {
    size_t size = strlen(name);
    if (!size || size > 253) return 0;
    unsigned int label = 0;
    for (const unsigned char *p = (const unsigned char *)name; *p; ++p) {
        if (*p == '.') { if (!label || label > 63) return 0; label = 0; continue; }
        if (!((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z') ||
              (*p >= '0' && *p <= '9') || *p == '-' || *p == '_')) return 0;
        ++label;
    }
    return label && label <= 63;
}

static int v_hosts(const char *path, const char *name, struct v_address_array *addresses, char canonical[256]) {
    FILE *file = fopen(path, "r");
    if (!file) return errno == ENOENT || errno == ENOTDIR || errno == EACCES ? 0 : EAI_SYSTEM;
    char *line = NULL;
    size_t capacity = 0;
    while (getline(&line, &capacity, file) >= 0) {
        char *cursor = line, *ip = v_token(&cursor), *first = v_token(&cursor), *alias = first;
        if (!ip || !first) continue;
        int matches = 0;
        do { if (v_equal(alias, name)) matches = 1; } while ((alias = v_token(&cursor)) != NULL);
        if (!matches) continue;
        struct v_address address;
        if (!v_ip_literal(ip, &address)) continue;
        int allocation = v_add_address(addresses, &address);
        if (allocation) { free(line); fclose(file); return allocation; }
        if (!*canonical && v_hostname(first)) memcpy(canonical, first, strlen(first) + 1);
    }
    int failed = ferror(file), error = errno;
    free(line); fclose(file);
    if (failed) { errno = error; return EAI_SYSTEM; }
    return 0;
}

struct v_service { unsigned short port; int type, protocol; };
static int v_services(const char *path, const char *name, int type, int protocol, int flags,
                      struct v_service ports[2], int *count) {
    *count = 0;
    if (type && type != SOCK_STREAM && type != SOCK_DGRAM && type != SOCK_RAW) return EAI_SOCKTYPE;
    if ((type == SOCK_STREAM && protocol && protocol != IPPROTO_TCP) ||
        (type == SOCK_DGRAM && protocol && protocol != IPPROTO_UDP)) return EAI_SERVICE;
    unsigned int number = 0;
    if (!name || v_decimal(name, 65535, &number)) {
        if (type == SOCK_RAW || (!type && protocol && protocol != IPPROTO_TCP && protocol != IPPROTO_UDP)) {
            if (name) return EAI_SERVICE;
            ports[(*count)++] = (struct v_service){0, SOCK_RAW, protocol};
        } else {
            if ((!type || type == SOCK_STREAM) && (!protocol || protocol == IPPROTO_TCP))
                ports[(*count)++] = (struct v_service){(unsigned short)number, SOCK_STREAM, IPPROTO_TCP};
            if ((!type || type == SOCK_DGRAM) && (!protocol || protocol == IPPROTO_UDP))
                ports[(*count)++] = (struct v_service){(unsigned short)number, SOCK_DGRAM, IPPROTO_UDP};
        }
        return 0;
    }
    if (flags & AI_NUMERICSERV) return EAI_NONAME;
    if (!*name || type == SOCK_RAW) return EAI_SERVICE;
    for (const char *p = name; *p; ++p) if (*p < '0' || *p > '9') goto named;
    return EAI_SERVICE;
named:;
    FILE *file = fopen(path, "r");
    if (!file) return errno == ENOENT || errno == ENOTDIR || errno == EACCES ? EAI_SERVICE : EAI_SYSTEM;
    char *line = NULL;
    size_t capacity = 0;
    while (*count < 2 && getline(&line, &capacity, file) >= 0) {
        char *cursor = line, *first = v_token(&cursor), *specification = v_token(&cursor), *alias;
        if (!first || !specification) continue;
        int matches = !strcmp(first, name);
        while ((alias = v_token(&cursor)) != NULL) if (!strcmp(alias, name)) matches = 1;
        if (!matches) continue;
        char *slash = strchr(specification, '/');
        if (!slash) continue;
        *slash++ = 0;
        if (!v_decimal(specification, 65535, &number)) continue;
        int candidate = !strcmp(slash, "tcp") ? IPPROTO_TCP : !strcmp(slash, "udp") ? IPPROTO_UDP : 0;
        int kind = candidate == IPPROTO_TCP ? SOCK_STREAM : SOCK_DGRAM;
        if (!candidate || (type && kind != type) || (protocol && protocol != candidate)) continue;
        int duplicate = 0;
        for (int i = 0; i < *count; ++i) if (ports[i].type == kind) duplicate = 1;
        if (!duplicate) ports[(*count)++] = (struct v_service){(unsigned short)number, kind, candidate};
    }
    int failed = ferror(file), error = errno;
    free(line); fclose(file);
    if (failed) { errno = error; return EAI_SYSTEM; }
    return *count ? 0 : EAI_SERVICE;
}

extern int32_t v_native_configured_families(void);

void freeaddrinfo(struct addrinfo *info) {
    while (info) { struct addrinfo *next = info->ai_next; free(info); info = next; }
}

static int v_resolve_getaddrinfo(const char *host, const char *service, const struct addrinfo *hints,
                              struct addrinfo **result, const struct v_resolver_config *paths, struct v_address_array *addresses) {
    struct addrinfo defaults = {0};
    if (!hints) hints = &defaults;
    *result = NULL;
    if (!host && !service) return EAI_NONAME;
    int flags = hints->ai_flags, family = hints->ai_family;
    if (flags & ~(AI_PASSIVE | AI_CANONNAME | AI_NUMERICHOST | AI_NUMERICSERV | AI_ADDRCONFIG | AI_V4MAPPED | AI_ALL)) return EAI_BADFLAGS;
    if (!host && (flags & AI_CANONNAME)) return EAI_BADFLAGS;
    if (family != AF_UNSPEC && family != AF_INET && family != AF_INET6) return EAI_FAMILY;
    struct v_service ports[2];
    int port_count, error = v_services(paths->services_path, service, hints->ai_socktype, hints->ai_protocol, flags, ports, &port_count);
    if (error) return error;
    int configured = 3;
    if (flags & AI_ADDRCONFIG) {
        configured = v_native_configured_families();
        if (configured < 0) return EAI_SYSTEM;
    }
    char name[256], canonical[256] = {0};
    if (!host) {
        addresses->data[addresses->count++] = (struct v_address){.family = AF_INET};
        addresses->data[addresses->count++] = (struct v_address){.family = AF_INET6};
        if (!(flags & AI_PASSIVE)) {
            addresses->data[0].bytes[0] = 127; addresses->data[0].bytes[3] = 1;
            addresses->data[1].bytes[15] = 1;
        }
    } else {
        size_t size = strlen(host);
        if (!size || size > 254) return EAI_NONAME;
        memcpy(name, host, size + 1);
        int absolute = name[size - 1] == '.';
        if (absolute) name[--size] = 0;
        if (v_ip_literal(host, &addresses->data[0])) {
            addresses->count = 1;
            memcpy(canonical, host, strlen(host) + 1);
        } else {
            if ((flags & AI_NUMERICHOST) || !v_hostname(name)) return EAI_NONAME;
            error = v_hosts(paths->hosts_path, name, addresses, canonical);
            if (error) return error;
            if (!addresses->count && v_equal(name, "localhost")) {
                addresses->data[addresses->count++] = (struct v_address){.family = AF_INET, .bytes = {127, 0, 0, 1}};
                addresses->data[addresses->count++] = (struct v_address){.family = AF_INET6, .bytes = {[15] = 1}};
            }
            if (!addresses->count) {
                struct v_resolver resolver;
                error = v_configure(&resolver, paths);
                if (error) return error;
                int dots = 0, attempted_absolute = 0, last_error = EAI_NONAME;
                for (const char *p = name; *p; ++p) if (*p == '.') ++dots;
                for (int attempt = -1; attempt <= resolver.search_count; ++attempt) {
                    char query[256];
                    if (attempt == -1) {
                        if (!absolute && dots < resolver.ndots) continue;
                        attempted_absolute = 1;
                        memcpy(query, name, size + 1);
                    } else if (attempt == resolver.search_count) {
                        if (attempted_absolute) break;
                        memcpy(query, name, size + 1);
                    } else {
                        if (absolute) break;
                        size_t suffix = strlen(resolver.search[attempt]);
                        if (size + suffix + 1 > 253) continue;
                        memcpy(query, name, size); query[size] = '.';
                        memcpy(query + size + 1, resolver.search[attempt], suffix + 1);
                    }
                    for (int version = 4; version <= 6; version += 2) {
                        if ((version == 4 && (!(configured & 1) || (family == AF_INET6 && !(flags & AI_V4MAPPED)))) ||
                            (version == 6 && (!(configured & 2) || family == AF_INET))) continue;
                        char resolved[256];
                        error = v_dns_lookup(&resolver, query, version == 4 ? 1 : 28, addresses, resolved);
                        if (!error && !*canonical) memcpy(canonical, resolved, strlen(resolved) + 1);
                        else if (error == EAI_SYSTEM || error == EAI_MEMORY) return error;
                        else if (error == EAI_AGAIN || error == EAI_FAIL || last_error == EAI_NONAME) last_error = error;
                    }
                    if (addresses->count) break;
                }
                if (!addresses->count) return last_error;
            }
        }
        if (!*canonical) memcpy(canonical, name, strlen(name) + 1);
    }
    int has6 = 0;
    for (int i = 0; i < addresses->count; ++i) if (addresses->data[i].family == AF_INET6) has6 = 1;
    struct addrinfo **tail = result;
    for (int i = 0; i < addresses->count; ++i) {
        struct v_address address = addresses->data[i];
        if (!(configured & (address.family == AF_INET ? 1 : 2))) continue;
        if (address.family == AF_INET && family == AF_INET6 && (flags & AI_V4MAPPED) &&
            (!has6 || (flags & AI_ALL))) {
            memmove(address.bytes + 12, address.bytes, 4);
            memset(address.bytes, 0, 10); address.bytes[10] = address.bytes[11] = 255;
            address.family = AF_INET6;
        }
        if (family && family != address.family) continue;
        for (int port = 0; port < port_count; ++port) {
            struct v_addrinfo *node = calloc(1, sizeof *node);
            if (!node) { freeaddrinfo(*result); *result = NULL; return EAI_MEMORY; }
            node->info.ai_family = address.family;
            node->info.ai_socktype = ports[port].type;
            node->info.ai_protocol = ports[port].protocol;
            node->info.ai_addr = (struct sockaddr *)&node->address;
            node->info.ai_addrlen = v_sockaddr(&address, ports[port].port, &node->address);
            if (host && tail == result && (flags & AI_CANONNAME)) {
                memcpy(node->canonical, canonical, strlen(canonical) + 1);
                node->info.ai_canonname = node->canonical;
            }
            *tail = &node->info; tail = &node->info.ai_next;
        }
    }
    return *result ? 0 : EAI_NONAME;
}

int v_freestanding_getaddrinfo(const char *host, const char *service, const struct addrinfo *hints,
                              struct addrinfo **result, const struct v_resolver_config *paths) {
    struct v_address_array addresses = {.capacity = 8};
    addresses.data = malloc(addresses.capacity * sizeof *addresses.data);
    if (!addresses.data) { *result = NULL; return EAI_MEMORY; }
    int error = v_resolve_getaddrinfo(host, service, hints, result, paths, &addresses);
    free(addresses.data);
    if (error == EAI_SYSTEM && errno == ENOMEM) error = EAI_MEMORY;
    return error;
}

static const struct v_resolver_config v_resolver_paths = {"/etc/hosts", "/etc/services", "/etc/resolv.conf", 53};
int getaddrinfo(const char *host, const char *service, const struct addrinfo *hints, struct addrinfo **result) {
    uint32_t shield = v_cancel_shield_enter();
    int code = v_freestanding_getaddrinfo(host, service, hints, result, &v_resolver_paths);
    v_cancel_shield_leave(shield);
    return code;
}
/* Opaque access to the existing DNS transport for V's reverse resolver.
 * These adapters introduce no additional resolver policy or packet parsing. */
uintptr_t v_resolver_context_size(void) { return sizeof(struct v_resolver); }
int32_t v_resolver_context_init(uintptr_t context, uintptr_t paths) {
    return v_configure((void *)context, paths ? (const void *)paths : &v_resolver_paths);
}
void v_resolver_properties(uintptr_t context, uintptr_t output) {
    const struct v_resolver *r = (const void *)context;
    int32_t *values = (void *)output;
    values[0] = r->server_count; values[1] = r->attempts; values[2] = r->timeout;
}
int32_t v_resolver_question(uintptr_t output, uintptr_t name, uint32_t type) {
    return v_dns_question((void *)output, (const void *)name, type);
}
int32_t v_resolver_exchange(uintptr_t context, int32_t server, uintptr_t query,
    uintptr_t query_size, uintptr_t reply, int32_t tcp, int64_t deadline) {
    struct v_resolver *r = (void *)context;
    return v_dns_exchange(r, &r->servers[server], (const void *)query, query_size,
                          (void *)reply, tcp, deadline);
}
int64_t v_resolver_now(void) { return v_dns_now(); }
int32_t v_resolver_io_error(void) { return v_dns_io_error(); }
const char *gai_strerror(int code) {
    switch (code) {
    case 0: return "Success";
    case EAI_BADFLAGS: return "Invalid address flags";
    case EAI_FAMILY: return "Unsupported address family";
    case EAI_SOCKTYPE: return "Unsupported socket type";
    case EAI_SERVICE: return "Service not available for socket type";
    case EAI_MEMORY: return "Out of memory";
    case EAI_NONAME: return "Name or service not known";
    case EAI_NODATA: return "No address associated with name";
    case EAI_AGAIN: return "Temporary name resolution failure";
    case EAI_FAIL: return "Non-recoverable name resolution failure";
    case EAI_SYSTEM: return "System error during name resolution";
    default: return "Unknown address resolution error";
    }
}
