#ifndef V_BARE_DIRECTORY_ABI_H
#define V_BARE_DIRECTORY_ABI_H
#include <stdint.h>
uintptr_t v_directory_open(uintptr_t);
uintptr_t v_directory_read(uintptr_t, int32_t);
int32_t v_directory_close(uintptr_t);
uintptr_t v_directory_entry_size(uintptr_t, int32_t);
int32_t v_directory_select(uintptr_t, uintptr_t, int32_t);
int32_t v_directory_compare(uintptr_t, uintptr_t, uintptr_t, int32_t);
#endif
