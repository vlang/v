#ifndef V_BARE_GLOB_ABI_H
#define V_BARE_GLOB_ABI_H
#include <stdint.h>
#include <glob.h>
void v_glob_view(uintptr_t, uintptr_t, int32_t, int32_t);
void v_glob_assign(uintptr_t, uintptr_t, int32_t);
uintptr_t v_glob_open(uintptr_t, uintptr_t);
uintptr_t v_glob_read(uintptr_t, uintptr_t, int32_t);
void v_glob_close(uintptr_t, uintptr_t);
int32_t v_glob_stat(uintptr_t, uintptr_t, int32_t, int32_t, uintptr_t);
int32_t v_glob_error(uintptr_t, uintptr_t, int32_t);
uintptr_t v_glob_home(uintptr_t);
#endif
