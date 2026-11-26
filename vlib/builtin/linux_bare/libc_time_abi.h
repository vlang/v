#ifndef V_BARE_TIME_ABI_H
#define V_BARE_TIME_ABI_H
#include <stdint.h>
#include <time.h>
void v_time_tm_set(uintptr_t, uintptr_t, int64_t, uintptr_t);
void v_time_tm_get(uintptr_t, uintptr_t);
uintptr_t v_time_tm_static(int32_t);
uintptr_t v_time_text_static(int32_t);
void v_time_names_set(int64_t, int32_t, uintptr_t, uintptr_t);
int32_t v_time_errno_get(void);
void v_time_errno_set(int32_t);
uintptr_t v_time_clocks_per_second(void);
int64_t v_time_local_offset(int64_t);
#endif
