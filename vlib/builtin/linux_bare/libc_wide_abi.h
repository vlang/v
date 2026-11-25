#ifndef V_BARE_WIDE_ABI_H
#define V_BARE_WIDE_ABI_H
#include <stdint.h>
uintptr_t v_wide_codeset(uintptr_t);
uintptr_t v_wide_state(int32_t);
uintptr_t v_wide_decode(uintptr_t,uintptr_t,uintptr_t,uintptr_t);
uintptr_t v_wide_encode(uintptr_t,uint32_t,uintptr_t);
#endif
