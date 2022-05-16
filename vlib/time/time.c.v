// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
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
fn C.localtime_r(t &C.time_t, tm &C.tm)

fn C.time(t &C.time_t) C.time_t

fn C.gmtime(t &C.time_t) &C.tm
fn C.gmtime_r(t &C.time_t, res &C.tm) &C.tm
fn C.strftime(buf &C.char, maxsize C.size_t, fmt &C.char, tm &C.tm) C.size_t

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

// str returns time in the same format as `parse` expects ("YYYY-MM-DD HH:mm:ss").
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
		// for the actual code base when we
		// call convert_ctime, it is always
		// when we manage the local time.
		is_local: true
	}
}

// strftime returns the formatted time using `strftime(3)`
pub fn (t Time) strftime(fmt string) string {
	mut tm := &C.tm{}
	$if windows {
		tm = C.gmtime(voidptr(&t.unix))
	} $else {
		C.gmtime_r(voidptr(&t.unix), tm)
	}
	mut buf := [1024]C.char{}
	fmt_c := unsafe { &C.char(fmt.str) }
	C.strftime(&buf[0], C.size_t(sizeof(buf)), fmt_c, tm)
	return unsafe { cstring_to_vstring(&char(&buf[0])) }
}
