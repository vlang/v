#ifndef V_BARE_PATTERN_ABI_H
#define V_BARE_PATTERN_ABI_H
#include <stdint.h>
#include <regex.h>
#ifndef REG_STARTEND
#define REG_STARTEND 0
#endif
void v_pattern_match_get(uintptr_t,uintptr_t,uintptr_t);
void v_pattern_match_set(uintptr_t,uintptr_t,int64_t,int64_t);
int64_t v_pattern_offset_max(void);
#endif
