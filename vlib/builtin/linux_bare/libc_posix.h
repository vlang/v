#ifndef V_LINUX_BARE_LIBC_POSIX_H
#define V_LINUX_BARE_LIBC_POSIX_H

#include <stdint.h>
#include <stddef.h>
#include <unistd.h>
#include <time.h>

/* Keep the platform's pointer qualifiers and struct layout in C. The V
 * bridges forward borrowed addresses without inspecting or copying them. */
int64_t v_freestanding_read(int32_t fd, uintptr_t address, size_t count);
int64_t v_freestanding_clock_gettime(int32_t clock, uintptr_t stamp);
int64_t v_freestanding_nanosleep(uintptr_t request, uintptr_t remaining);
int64_t v_freestanding_tty_probe(int32_t fd, uintptr_t request, uintptr_t termios);

#endif
