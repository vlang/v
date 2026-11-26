#ifndef V_BARE_SHELL_ABI_H
#define V_BARE_SHELL_ABI_H
#include <stdint.h>
uintptr_t v_shell_stream_open(int32_t, uintptr_t);
int32_t v_shell_stream_close(uintptr_t);
#endif
