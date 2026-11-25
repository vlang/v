/* Exercise the exact parser implementation, under separate public names so
 * the ordinary runtime and this white-box guard-page fixture can coexist. */
#include "probe.h"
#include <netdb.h>
#include "../../linux_bare/libc_resolver.h"
#undef getaddrinfo
#undef freeaddrinfo
#undef gai_strerror
#define getaddrinfo v_fuzz_getaddrinfo
#define freeaddrinfo v_fuzz_freeaddrinfo
#define gai_strerror v_fuzz_gai_strerror
#define v_freestanding_getaddrinfo v_fuzz_resolve
#include "../../linux_bare/libc_resolver.c"
#include <sys/mman.h>

int v_freestanding_dns_parser_fuzz(void) {
    long page = sysconf(_SC_PAGESIZE);
    if (page < 512) return __LINE__;
    unsigned char *mapping = (void *)syscall(SYS_mmap, NULL, (size_t)page * 2,
        PROT_READ | PROT_WRITE, MAP_PRIVATE | MAP_ANONYMOUS, -1, (off_t)0);
    if (mapping == MAP_FAILED) return __LINE__;
    if (syscall(SYS_mprotect, mapping + page, (size_t)page, PROT_NONE) < 0) return __LINE__;
    static const unsigned char original[] = {
        0x12,0x34,0x81,0x80,0,1,0,1,0,0,0,0,
        1,'x',4,'t','e','s','t',0,0,1,0,1,
        0xc0,0x0c,0,1,0,1,0,0,0,30,0,4,192,0,2,1
    };
    uint32_t random = UINT32_C(0x9e3779b9);
    for (unsigned int iteration = 0; iteration < 50000; ++iteration) {
        random ^= random << 13; random ^= random >> 17; random ^= random << 5;
        size_t size = iteration < sizeof original ? iteration : random % 512;
        unsigned char *packet = mapping + page - size;
        for (size_t i = 0; i < size; ++i) packet[i] = i < sizeof original ? original[i] : (unsigned char)(random >> (i % 4 * 8));
        if (size && iteration >= sizeof original) {
            unsigned int changes = 1 + random % 4;
            while (changes--) {
                random ^= random << 13; random ^= random >> 17; random ^= random << 5;
                packet[random % size] ^= (unsigned char)(random >> 16);
            }
            if (size >= 2) { packet[0] = 0x12; packet[1] = 0x34; }
        }
        struct { unsigned char before[16]; char value[256]; unsigned char after[16]; } canonical;
        memset(&canonical, 0xa5, sizeof canonical);
        struct v_address_array addresses = {0};
        int result = v_dns_parse(packet, size, 0x1234, "x.test", 1, &addresses, canonical.value);
        for (unsigned int i = 0; i < 16; ++i)
            if (canonical.before[i] != 0xa5 || canonical.after[i] != 0xa5) return __LINE__;
        if (result == 0 && !addresses.count) return __LINE__;
        if (result != 0 && addresses.count) return __LINE__;
        free(addresses.data);
    }
    if (syscall(SYS_munmap, mapping, (size_t)page * 2) < 0) return __LINE__;
    return 0;
}
