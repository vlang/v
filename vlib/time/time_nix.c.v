// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

#include <time.h>

struct C.tm {
	tm_sec  int
	tm_min  int
	tm_hour int
	tm_mday int
	tm_mon  int
	tm_year int
	tm_wday int
	tm_yday int
	tm_isdst int
}

fn C.timegm(&tm) time_t
fn C.localtime_r(t &C.time_t, tm &C.tm )

fn make_unix_time(t C.tm) int {
	return int(C.timegm(&t))
}

fn to_local_time(t Time) Time {
	loc_tm := C.tm{}
	C.localtime_r(time_t(&t.unix), &loc_tm)

	return convert_ctime(loc_tm, t.microsecond)
}

type time_t voidptr

// in most systems, these are __quad_t, which is an i64
struct C.timespec {
mut:
	tv_sec  i64
	tv_nsec i64
}

// the first arg is defined in include/bits/types.h as `__S32_TYPE`, which is `int`
fn C.clock_gettime(int, &C.timespec)

pub fn sys_mono_now() u64 {
	$if macos {
		return sys_mono_now_darwin()
	} $else {
		ts := C.timespec{}
		C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
		return u64(ts.tv_sec) * 1_000_000_000 + u64(ts.tv_nsec)
	}
}

// NB: vpc_now is used by `v -profile` .
// It should NOT call *any other v function*, just C functions and casts.
[inline]
fn vpc_now() u64 {
	ts := C.timespec{}
	C.clock_gettime(C.CLOCK_MONOTONIC, &ts)
	return u64(ts.tv_sec) * 1_000_000_000 + u64(ts.tv_nsec)
}



// dummy to compile with all compilers
pub fn win_now() Time {
	return Time{}
}

// dummy to compile with all compilers
pub fn win_utc() Time {
	return Time{}
}

// dummy to compile with all compilers
pub struct C.timeval {
	tv_sec  u64
	tv_usec u64
}

// return absolute timespec for now()+d
pub fn (d Duration) timespec() C.timespec {
	mut ts := C.timespec{}
	C.clock_gettime(C.CLOCK_REALTIME, &ts)
	d_sec := d / second
	d_nsec := d % second
	ts.tv_sec += d_sec
	ts.tv_nsec += d_nsec
	if ts.tv_nsec > second {
		ts.tv_nsec -= second
		ts.tv_sec++
	}
	return ts
}

// return timespec of 1970/1/1
pub fn zero_timespec() C.timespec {
	ts := C.timespec{
		tv_sec:  0
		tv_nsec: 0
	}
	return ts
}
