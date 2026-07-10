module time

// Declarations for the Linux timerfd API (timerfd_create(2)): kernel timers
// delivered as FD READABILITY. Import `time` and call them directly — create,
// arm with C.timerfd_settime + C.itimerspec, then read 8 bytes (a u64 count
// of expirations since the last read) from the fd, or register the fd with
// epoll / os.notify and treat expiry as just another readiness event.
//
// timerfd is a Linux-only kernel API, so guard every call site with `$if linux {`.
//
// Example (a 1 s periodic tick inside an event loop):
// ```v ignore
// tfd := C.timerfd_create(C.CLOCK_MONOTONIC, C.TFD_CLOEXEC | C.TFD_NONBLOCK)
// spec := C.itimerspec{
// 	it_value:    C.timespec{1, 0}
// 	it_interval: C.timespec{1, 0}
// }
// C.timerfd_settime(tfd, 0, &spec, unsafe { nil })
// // ... register tfd for readability; on the event:
// mut expirations := u64(0)
// C.read(tfd, &expirations, 8) // re-levels the fd; > 1 reports missed ticks
// ```

// The `#include linux` form emits the include wrapped in `#if defined(__linux__)`,
// so it stays inert on non-Linux targets. This is why the file is NOT named
// `timerfd_linux.c.v`: the OS-guard on `#include` is only applied for
// non-platform-suffixed files, and the cross-platform bootstrap `vc/v.c` is
// generated on Linux with `-os cross` (which compiles the host's `_linux.c.v`
// files) but is then compiled on macOS/Windows/BSD, where <sys/timerfd.h> is
// absent. An unguarded include there breaks the bootstrap on every non-Linux
// runner; the C declarations below are only referenced under `$if linux`.
#include linux <sys/timerfd.h>

pub struct C.itimerspec {
pub mut:
	it_interval C.timespec
	it_value    C.timespec
}

fn C.timerfd_create(clockid int, flags int) int
fn C.timerfd_settime(fd int, flags int, new_value &C.itimerspec, old_value &C.itimerspec) int
fn C.timerfd_gettime(fd int, curr_value &C.itimerspec) int
