#ifndef V_BARE_STDIO_ABI_H
#define V_BARE_STDIO_ABI_H
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <wchar.h>
/* GNU stdio inlines inspect these flags and the read/write pointer pairs.
 * Leave the pointer pairs empty so every character goes through our buffer. */
#if defined(__GLIBC__)
_Static_assert(offsetof(FILE, _flags) == 0
    && offsetof(FILE, _IO_read_ptr) + sizeof(char *) <= 56
    && offsetof(FILE, _IO_read_end) + sizeof(char *) <= 56
    && offsetof(FILE, _IO_write_ptr) + sizeof(char *) <= 56
    && offsetof(FILE, _IO_write_end) + sizeof(char *) <= 56, "GNU FILE inline prefix layout");
_Static_assert(_IO_EOF_SEEN == 0x10 && _IO_ERR_SEEN == 0x20, "GNU FILE state flags");
#endif
enum { V_STDIO_EOF = 0x10, V_STDIO_ERROR = 0x20 };
uintptr_t v_stdio_standard(int32_t);
uintptr_t v_stdio_stdout(void);
size_t v_stdio_standard_size(void);
int64_t v_stdio_cookie_read(uintptr_t, uintptr_t, uintptr_t, uintptr_t);
int64_t v_stdio_cookie_write(uintptr_t, uintptr_t, uintptr_t, uintptr_t);
int32_t v_stdio_cookie_seek(uintptr_t, uintptr_t, int64_t *, int32_t);
int32_t v_stdio_cookie_close(uintptr_t, uintptr_t);
uintptr_t v_stdio_decode(uintptr_t, uintptr_t, uintptr_t, int32_t);
uintptr_t v_stdio_encode(uintptr_t, uint32_t, uintptr_t, int32_t);
void v_stdio_position_set(uintptr_t, int64_t, uintptr_t);
int64_t v_stdio_position_get(uintptr_t, uintptr_t);
#endif
