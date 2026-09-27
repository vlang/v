module sync

#include <time.h>
#include <errno.h>

pub struct C.timespec {
pub mut:
	tv_sec  i64
	tv_nsec i64
}

fn C.clock_gettime(i32, &C.timespec) i32
fn C.nanosleep(&C.timespec, &C.timespec) i32

fn sync_mono_now() i64 {
	mut ts := C.timespec{}
	C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
	return ts.tv_sec * 1_000_000_000 + ts.tv_nsec
}

fn sync_realtime_deadline(timeout i64) C.timespec {
	mut ts := C.timespec{}
	C.clock_gettime(C.CLOCK_REALTIME, &ts)
	ts.tv_sec += timeout / 1_000_000_000
	ts.tv_nsec += timeout % 1_000_000_000
	if ts.tv_nsec >= 1_000_000_000 {
		ts.tv_nsec -= 1_000_000_000
		ts.tv_sec++
	}
	return ts
}

fn sync_sleep_nanoseconds(duration i64) {
	mut request := C.timespec{
		tv_sec:  duration / 1_000_000_000
		tv_nsec: duration % 1_000_000_000
	}
	mut remaining := C.timespec{}
	for C.nanosleep(&request, &remaining) < 0 {
		if C.errno != C.EINTR {
			break
		}
		request = remaining
	}
}
