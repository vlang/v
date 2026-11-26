#ifndef V_LINUX_BARE_RUNTIME_H
#define V_LINUX_BARE_RUNTIME_H
#include <stddef.h>
#include <stdint.h>
#include "initializers.h"
#include "libc_environment.h"
#include "libc_sort.h"
int clearenv(void);
void v_freestanding_stdio_flush(void);
void v_freestanding_program_names_set(uintptr_t, uintptr_t, uintptr_t);
uintptr_t v_freestanding_program_short_name(void);
uintptr_t v_freestanding_program_auxiliary(void);
int v_freestanding_log_format(uintptr_t, uintptr_t, uintptr_t);
int32_t v_freestanding_log_connect(int32_t, uintptr_t, uint32_t);
#endif
