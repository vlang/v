#ifndef V_BARE_NATIVE_ABI_H
#define V_BARE_NATIVE_ABI_H
#include <stdint.h>
#include <stddef.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/auxv.h>
#include <sys/syscall.h>
#include <termios.h>
#include <stdlib.h>
uintptr_t v_native_environment(void);
size_t v_native_termios_size(void);
/* statvfs has a toolchain-specific public layout. */
void v_native_statvfs_assign(uintptr_t, uintptr_t);
#endif
