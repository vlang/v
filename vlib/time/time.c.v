// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

#include <time.h>

// C.timeval represents a C time value.
pub struct C.timeval {
	tv_sec  u64
	tv_usec u64
}

fn C.localtime(t &C.time_t) &C.tm

fn C.time(t &C.time_t) C.time_t

// now returns current local time.
pub fn now() Time {
	$if macos {
		return darwin_now()
	}
	$if windows {
		return win_now()
	}
	$if solaris {
		return solaris_now()
	}
	$if linux || android {
		return linux_now()
	}
	// defaults to most common feature, the microsecond precision is not available
	// in this API call
	t := C.time(0)
	now := C.localtime(&t)
	return convert_ctime(*now, 0)
}

// utc returns the current UTC time.
pub fn utc() Time {
	$if macos {
		return darwin_utc()
	}
	$if windows {
		return win_utc()
	}
	$if solaris {
		return solaris_utc()
	}
	$if linux || android {
		return linux_utc()
	}
	// defaults to most common feature, the microsecond precision is not available
	// in this API call
	t := C.time(0)
	_ = C.time(&t)
	return unix2(i64(t), 0)
}

// new_time returns a time struct with calculated Unix time.
pub fn new_time(t Time) Time {
	if t.unix != 0 {
		return t
	}
	tt := C.tm{
		tm_sec: t.second
		tm_min: t.minute
		tm_hour: t.hour
		tm_mday: t.day
		tm_mon: t.month - 1
		tm_year: t.year - 1900
	}
	utime := make_unix_time(tt)
	return Time{
		...t
		unix: utime
	}
}

// ticks returns a number of milliseconds elapsed since system start.
pub fn ticks() i64 {
	$if windows {
		return C.GetTickCount()
	} $else {
		ts := C.timeval{}
		C.gettimeofday(&ts, 0)
		return i64(ts.tv_sec * u64(1000) + (ts.tv_usec / u64(1000)))
	}
	// t := i64(C.mach_absolute_time())
	// # Nanoseconds elapsedNano = AbsoluteToNanoseconds( *(AbsoluteTime *) &t );
	// # return (double)(* (uint64_t *) &elapsedNano) / 1000000;
}

/*
// sleep makes the calling thread sleep for a given number of seconds.
[deprecated: 'call time.sleep(n * time.second)']
pub fn sleep(seconds int) {
	wait(seconds * time.second)
}
*/

// str returns time in the same format as `parse` expects ("YYYY-MM-DD HH:MM:SS").
pub fn (t Time) str() string {
	// TODO Define common default format for
	// `str` and `parse` and use it in both ways
	return t.format_ss()
}

// convert_ctime converts a C time to V time.
fn convert_ctime(t C.tm, microsecond int) Time {
	return Time{
		year: t.tm_year + 1900
		month: t.tm_mon + 1
		day: t.tm_mday
		hour: t.tm_hour
		minute: t.tm_min
		second: t.tm_sec
		microsecond: microsecond
		unix: make_unix_time(t)
	}
}

pub const (
	infinite = Duration(C.INT64_MAX)
)
