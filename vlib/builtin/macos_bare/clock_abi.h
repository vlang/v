#ifndef V_MACOS_CLOCK_ABI_H
#define V_MACOS_CLOCK_ABI_H
#include <stddef.h>
#include <time.h>
#include <errno.h>
#include <sys/time.h>

/* V's readonly view preserves the SDK's const pointee type in export wrappers. */
typedef const struct timespec VMacClockConstTimespec;
typedef clockid_t VMacClockId;
typedef time_t VMacClockTime;
_Static_assert(sizeof(time_t) == 8, "64-bit clock time_t ABI");
_Static_assert(sizeof(clockid_t) == 4, "clock selector ABI");
_Static_assert(sizeof(struct timespec) == 16, "64-bit clock timespec ABI");
_Static_assert(offsetof(struct timespec, tv_nsec) == 8, "clock nanosecond field ABI");
#endif
