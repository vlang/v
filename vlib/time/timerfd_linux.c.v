module time

// Declarations for the Linux timerfd API (timerfd_create(2)): kernel timers
// delivered as FD READABILITY. Import `time` and call them directly — create,
// arm with C.timerfd_settime + C.itimerspec, then read 8 bytes (a u64 count
// of expirations since the last read) from the fd, or register the fd with
// epoll / os.notify and treat expiry as just another readiness event.
//
// This file is only compiled on Linux; guard call sites with `$if linux {`.
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

#include <sys/timerfd.h>

pub struct C.itimerspec {
pub mut:
	it_interval C.timespec
	it_value    C.timespec
}

fn C.timerfd_create(clockid int, flags int) int
fn C.timerfd_settime(fd int, flags int, new_value &C.itimerspec, old_value &C.itimerspec) int
fn C.timerfd_gettime(fd int, curr_value &C.itimerspec) int
