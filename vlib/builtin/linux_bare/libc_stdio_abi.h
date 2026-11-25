#ifndef V_BARE_STDIO_ABI_H
#define V_BARE_STDIO_ABI_H
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <wchar.h>
uintptr_t v_stdio_standard(int32_t);
size_t v_stdio_standard_size(void);
int64_t v_stdio_cookie_read(uintptr_t, uintptr_t, uintptr_t, uintptr_t);
int64_t v_stdio_cookie_write(uintptr_t, uintptr_t, uintptr_t, uintptr_t);
int32_t v_stdio_cookie_seek(uintptr_t, uintptr_t, int64_t *, int32_t);
int32_t v_stdio_cookie_close(uintptr_t, uintptr_t);
uintptr_t v_stdio_decode(uintptr_t, uintptr_t, uintptr_t);
uintptr_t v_stdio_encode(uintptr_t, uint32_t, uintptr_t);
void v_stdio_position_set(uintptr_t, int64_t, uintptr_t);
int64_t v_stdio_position_get(uintptr_t, uintptr_t);
#endif
