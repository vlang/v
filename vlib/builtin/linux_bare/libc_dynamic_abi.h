#ifndef V_BARE_DYNAMIC_ABI_H
#define V_BARE_DYNAMIC_ABI_H
#include <stdint.h>
#include <dlfcn.h>
#include <link.h>
#ifndef DLFO_STRUCT_HAS_EH_DBASE
/* Public GNU ELF64 API prefix, also usable with older target headers. */
struct dl_find_object {
    unsigned long long dlfo_flags;
    void *dlfo_map_start, *dlfo_map_end;
    struct link_map *dlfo_link_map;
    void *dlfo_eh_frame;
    unsigned long long __dlfo_reserved[7];
};
int _dl_find_object(void *,struct dl_find_object *);
#endif
uintptr_t v_dynamic_head(void);
uintptr_t v_dynamic_field(uintptr_t, int);
uintptr_t v_dynamic_error_buffer(void);
int v_dynamic_error_pending(int);
uintptr_t v_dynamic_ifunc(uintptr_t);
void v_dynamic_address_set(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t);
int v_dynamic_visit(uintptr_t, uintptr_t, uintptr_t, uintptr_t, uintptr_t, uint16_t);
void v_dynamic_find_set(uintptr_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t);
#endif
