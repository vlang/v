#ifndef V_BARE_NAMEINFO_ABI_H
#define V_BARE_NAMEINFO_ABI_H
#include <stdint.h>
#include <netdb.h>
int32_t v_name_sockaddr(uintptr_t, uint32_t, uintptr_t);
void v_name_paths(uintptr_t, uintptr_t);
uintptr_t v_resolver_context_size(void);
int32_t v_resolver_context_init(uintptr_t, uintptr_t);
void v_resolver_properties(uintptr_t, uintptr_t);
int32_t v_resolver_question(uintptr_t, uintptr_t, uint32_t);
int32_t v_resolver_exchange(uintptr_t, int32_t, uintptr_t, uintptr_t, uintptr_t, int32_t, int64_t);
int64_t v_resolver_now(void);
int32_t v_resolver_io_error(void);
#endif
