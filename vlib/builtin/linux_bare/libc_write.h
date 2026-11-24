#ifndef V_LINUX_BARE_LIBC_WRITE_H
#define V_LINUX_BARE_LIBC_WRITE_H

#include <stdint.h>
#include <stddef.h>
#include <unistd.h>

/* An explicitly scalar interface keeps V's type representation behind the
 * platform POSIX ABI. uintptr_t preserves the borrowed buffer address. */
int64_t v_freestanding_write(int32_t fd, uintptr_t address, size_t count);

#endif
