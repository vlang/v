#include "libc_compat.h"
#include "libc_dynamic_abi.h"
#include <dlfcn.h>
#include <link.h>
/* Public loader metadata, native callback ABI and per-thread storage only. */
extern struct r_debug _r_debug __attribute__((weak));
uintptr_t v_dynamic_head(void) { return &_r_debug ? (uintptr_t)_r_debug.r_map : 0; }
uintptr_t v_dynamic_field(uintptr_t object, int field) {
    const struct link_map *map = (const struct link_map *)object;
    switch (field) {
    case 0: return map->l_addr;
    case 1: return (uintptr_t)map->l_name;
    case 2: return (uintptr_t)map->l_ld;
    case 3: return (uintptr_t)map->l_next;
    default: return (uintptr_t)map->l_prev;
    }
}
static __thread char v_dynamic_error[512];
static __thread int v_dynamic_pending;
uintptr_t v_dynamic_error_buffer(void) { return (uintptr_t)v_dynamic_error; }
int v_dynamic_error_pending(int value) {
    int previous = v_dynamic_pending;
    if (value >= 0) v_dynamic_pending = value;
    return previous;
}
uintptr_t v_dynamic_ifunc(uintptr_t function) {
    #if defined(__aarch64__)
    extern uintptr_t v_native_getauxval(uintptr_t);
    return ((uintptr_t (*)(uintptr_t))function)(v_native_getauxval(16));
#else
    return ((uintptr_t (*)(void))function)();
#endif
}
extern uintptr_t v_dynamic_open(uintptr_t, int32_t);
extern uintptr_t v_dynamic_symbol(uintptr_t, uintptr_t, uintptr_t);
extern int32_t v_dynamic_close(uintptr_t);
extern uintptr_t v_dynamic_error_get(void);
void *dlopen(const char *name, int flags) { return (void *)v_dynamic_open((uintptr_t)name, flags); }
void *dlsym(void *handle, const char *name) {
    return (void *)v_dynamic_symbol((uintptr_t)handle, (uintptr_t)name,
                                  (uintptr_t)__builtin_return_address(0));
}
int dlclose(void *handle) { return v_dynamic_close((uintptr_t)handle); }
char *dlerror(void) { return (char *)v_dynamic_error_get(); }
extern int32_t v_dynamic_address(uintptr_t, uintptr_t);
extern int32_t v_dynamic_address_extra(uintptr_t, uintptr_t, uintptr_t, int32_t);
extern int32_t v_dynamic_info(uintptr_t, int32_t, uintptr_t);
extern int32_t v_dynamic_iterate(uintptr_t, uintptr_t);
int dladdr(const void *address, Dl_info *info) { return v_dynamic_address((uintptr_t)address,(uintptr_t)info); }
int dladdr1(const void *address, Dl_info *info, void **extra, int flags) {
    return v_dynamic_address_extra((uintptr_t)address,(uintptr_t)info,(uintptr_t)extra,flags);
}
int dlinfo(void *handle,int request,void *output) { return v_dynamic_info((uintptr_t)handle,request,(uintptr_t)output); }
void v_dynamic_address_set(uintptr_t output, uintptr_t file, uintptr_t base, uintptr_t name, uintptr_t symbol) {
    Dl_info *info=(Dl_info *)output;
    info->dli_fname=(const char *)file; info->dli_fbase=(void *)base;
    info->dli_sname=(const char *)name; info->dli_saddr=(void *)symbol;
}
int v_dynamic_visit(uintptr_t callback, uintptr_t data, uintptr_t base, uintptr_t name, uintptr_t phdr, uint16_t count) {
    struct dl_phdr_info info={0};
    info.dlpi_addr=base; info.dlpi_name=(const char *)name;
    info.dlpi_phdr=(const ElfW(Phdr) *)phdr; info.dlpi_phnum=count;
    /* Only advertise the public prefix whose contents are available here. */
    return ((int (*)(struct dl_phdr_info *,size_t,void *))callback)(
        &info,offsetof(struct dl_phdr_info,dlpi_adds),(void *)data);
}
int dl_iterate_phdr(int (*callback)(struct dl_phdr_info *,size_t,void *),void *data) {
    return v_dynamic_iterate((uintptr_t)callback,(uintptr_t)data);
}
extern int32_t v_dynamic_find_object(uintptr_t,uintptr_t);
int _dl_find_object(void *address,struct dl_find_object *output) {
    return v_dynamic_find_object((uintptr_t)address,(uintptr_t)output);
}
void v_dynamic_find_set(uintptr_t output,uintptr_t start,uintptr_t end,uintptr_t map,uintptr_t eh,uintptr_t sframe) {
    struct dl_find_object *info=(struct dl_find_object *)output;
    *info=(struct dl_find_object){0};
    info->dlfo_map_start=(void *)start; info->dlfo_map_end=(void *)end;
    info->dlfo_link_map=(struct link_map *)map; info->dlfo_eh_frame=(void *)eh;
#ifdef DLFO_FLAG_SFRAME
    info->dlfo_sframe=(void *)sframe;
    if(sframe) info->dlfo_flags=DLFO_FLAG_SFRAME;
#else
    (void)sframe;
#endif
}
