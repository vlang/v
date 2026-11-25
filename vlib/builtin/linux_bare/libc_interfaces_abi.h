#ifndef V_BARE_INTERFACES_ABI_H
#define V_BARE_INTERFACES_ABI_H
#include <stdint.h>
int v_resolver_wait_readable(int fd, int64_t deadline);
uintptr_t v_ifaddr_size(void);
uintptr_t v_ifaddr_sockaddr_size(int32_t,uintptr_t);
void v_ifaddr_assign(uintptr_t,uintptr_t,uintptr_t,uint32_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t);
void v_ifaddr_sockaddr(uintptr_t,int32_t,uint32_t,uint16_t,uintptr_t,uintptr_t);
uintptr_t v_ifaddr_next(uintptr_t);
#endif
