#include "libc_compat.h"
#include "libc_interfaces_abi.h"
#include <ifaddrs.h>
#include <net/if.h>
#include <netpacket/packet.h>
#include <netinet/in.h>
#include <string.h>
/* Native field layouts, not netlink parsing or interface policy. */
uintptr_t v_ifaddr_size(void) { return sizeof(struct ifaddrs); }
uintptr_t v_ifaddr_sockaddr_size(int32_t family,uintptr_t length) {
    switch(family) {
    case AF_INET: return sizeof(struct sockaddr_in);
    case AF_INET6: return sizeof(struct sockaddr_in6);
    default: return sizeof(struct sockaddr_ll)+(length>8?length-8:0);
    }
}
void v_ifaddr_assign(uintptr_t output,uintptr_t next,uintptr_t name,uint32_t flags,uintptr_t address,uintptr_t mask,uintptr_t broadcast,uintptr_t data) {
    struct ifaddrs *a=(struct ifaddrs *)output;
    *a=(struct ifaddrs){.ifa_next=(struct ifaddrs *)next,.ifa_name=(char *)name,.ifa_flags=flags,.ifa_addr=(struct sockaddr *)address,.ifa_netmask=(struct sockaddr *)mask,.ifa_broadaddr=(struct sockaddr *)broadcast,.ifa_data=(void *)data};
}
void v_ifaddr_sockaddr(uintptr_t output,int32_t family,uint32_t index,uint16_t kind,uintptr_t address,uintptr_t size) {
    if(family==AF_INET) {
        struct sockaddr_in *a=(struct sockaddr_in *)output;
        a->sin_family=family; memcpy(&a->sin_addr,(const void *)address,4);
    } else if(family==AF_INET6) {
        struct sockaddr_in6 *a=(struct sockaddr_in6 *)output;
        a->sin6_family=family; memcpy(&a->sin6_addr,(const void *)address,16);
        a->sin6_scope_id=index;
    } else {
        struct sockaddr_ll *a=(struct sockaddr_ll *)output;
        a->sll_family=family; a->sll_ifindex=index; a->sll_hatype=kind; a->sll_halen=size;
        memcpy(a->sll_addr,(const void *)address,size);
    }
}
uintptr_t v_ifaddr_next(uintptr_t a) { return (uintptr_t)((const struct ifaddrs *)a)->ifa_next; }
extern int32_t v_native_getifaddrs(uintptr_t);
extern void v_native_freeifaddrs(uintptr_t);
extern uint32_t v_native_if_index(uintptr_t);
int getifaddrs(struct ifaddrs **out) { return v_native_getifaddrs((uintptr_t)out); }
void freeifaddrs(struct ifaddrs *out) { v_native_freeifaddrs((uintptr_t)out); }
unsigned int if_nametoindex(const char *name) { return v_native_if_index((uintptr_t)name); }
extern uintptr_t v_native_if_name(uint32_t, uintptr_t);
char *if_indextoname(unsigned int index, char *output) { return (char *)v_native_if_name(index, (uintptr_t)output); }
