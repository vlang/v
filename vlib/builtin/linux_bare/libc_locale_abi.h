#ifndef V_BARE_LOCALE_ABI_H
#define V_BARE_LOCALE_ABI_H
#include <stdint.h>
#include <langinfo.h>
uintptr_t v_locale_select(uintptr_t);
uintptr_t v_locale_time_format(uintptr_t,uintptr_t,uintptr_t,uintptr_t,uintptr_t);
uintptr_t v_locale_gnu_data(void);
uint32_t v_locale_gnu_count(void);
uintptr_t v_locale_langinfo_base(int32_t,uintptr_t);
uint32_t v_locale_bits(uintptr_t);
#endif
