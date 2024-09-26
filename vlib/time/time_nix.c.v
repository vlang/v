// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

#flag darwin -I@VEXEROOT/thirdparty/legacy/include/LegacySupport
#include <time.h>
#include <errno.h>

pub struct C.tm {
pub mut:
	tm_sec    int
	tm_min    int
	tm_hour   int
	tm_mday   int
	tm_mon    int
	tm_year   int
	tm_wday   int
	tm_yday   int
	tm_isdst  int
	tm_gmtoff int
}

fn C.timegm(&C.tm) C.time_t

// preferring localtime_r over the localtime because
// from docs localtime_r is thread safe,
fn C.localtime_r(t &C.time_t, tm &C.tm)

fn make_unix_time(t C.tm) i64 {
	return unsafe { i64(C.timegm(&t)) }
}

// local returns t with the location set to local time.
pub fn (t Time) local() Time {
	if t.is_local {
		return t
	}
	loc_tm := C.tm{}
	t_ := t.unix()
	C.localtime_r(voidptr(&t_), &loc_tm)
	return convert_ctime(loc_tm, t.nanosecond)
}

// in most systems, these are __quad_t, which is an i64
pub struct C.timespec {
pub mut:
	tv_sec  i64
	tv_nsec i64
}

// the first arg is defined in include/bits/types.h as `__S32_TYPE`, which is `int`
fn C.clock_gettime(int, &C.timespec)

fn C.nanosleep(req &C.timespec, rem &C.timespec) int

// sys_mono_now returns a *monotonically increasing time*, NOT a time adjusted for daylight savings, location etc.
pub fn sys_mono_now() u64 {
	$if macos {
		return sys_mono_now_darwin()
	} $else {
		ts := C.timespec{}
		C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
		return u64(ts.tv_sec) * 1_000_000_000 + u64(ts.tv_nsec)
	}
}

// Note: vpc_now is used by `v -profile` .
// It should NOT call *any other v function*, just C functions and casts.
@[inline]
fn vpc_now() u64 {
	ts := C.timespec{}
	C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
	return u64(ts.tv_sec) * 1_000_000_000 + u64(ts.tv_nsec)
}

// The linux_* functions are placed here, since they're used on Android as well
// TODO: should `$if linux {}` be parsed on Android as well? (Android runs under the Linux kernel)
// linux_now returns the local time with high precision for most os:es
// this should be implemented properly with support for leap seconds.
// It uses the realtime clock to get and converts it to local time
fn linux_now() Time {
	// get the high precision time as UTC realtime clock
	// and use the nanoseconds part
	mut ts := C.timespec{}
	C.clock_gettime(C.CLOCK_REALTIME, &ts)
	loc_tm := C.tm{}
	C.localtime_r(voidptr(&ts.tv_sec), &loc_tm)
	return convert_ctime(loc_tm, int(ts.tv_nsec))
}

fn linux_utc() Time {
	// get the high precision time as UTC realtime clock
	// and use the nanoseconds part
	mut ts := C.timespec{}
	C.clock_gettime(C.CLOCK_REALTIME, &ts)
	return unix_nanosecond(i64(ts.tv_sec), int(ts.tv_nsec))
}

// dummy to compile with all compilers
fn win_now() Time {
	return Time{}
}

// dummy to compile with all compilers
fn win_utc() Time {
	return Time{}
}

// return absolute timespec for now()+d
pub fn (d Duration) timespec() C.timespec {
	mut ts := C.timespec{}
	C.clock_gettime(C.CLOCK_REALTIME, &ts)
	d_sec := d / second
	d_nsec := d % second
	ts.tv_sec += d_sec
	ts.tv_nsec += d_nsec
	if ts.tv_nsec > i64(second) {
		ts.tv_nsec -= i64(second)
		ts.tv_sec++
	}
	return ts
}

// sleep suspends the execution of the calling thread for a given duration (in nanoseconds).
pub fn sleep(duration Duration) {
	mut req := C.timespec{duration / second, duration % second}
	rem := C.timespec{}
	for C.nanosleep(&req, &rem) < 0 {
		if C.errno == C.EINTR {
			// Interrupted by a signal handler
			req = rem
		} else {
			break
		}
	}
}
