// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

#flag darwin -I@VEXEROOT/thirdparty/legacy/include/LegacySupport

pub struct C.tm {
pub mut:
	tm_sec   i32
	tm_min   i32
	tm_hour  i32
	tm_mday  i32
	tm_mon   i32
	tm_year  i32
	tm_wday  i32
	tm_yday  i32
	tm_isdst i32
	// C `long tm_gmtoff`: pointer-width, which V's `int` already matches (i64 on
	// 64-bit, i32 on 32-bit), unlike the 32-bit C `int` fields above.
	tm_gmtoff int
}

fn C.timegm(&C.tm) C.time_t

// preferring localtime_r over the localtime because
// from docs localtime_r is thread safe,
fn C.localtime_r(t &C.time_t, tm &C.tm)

// c_local_epoch_offset returns how many seconds east of UTC the local zone is at
// the instant `sec`; `loc_tm` is the caller's localtime_r result for `sec`. glibc,
// macOS and the BSDs carry it in struct tm's tm_gmtoff. Solaris's struct tm has no
// such member, so there it is the local calendar fields read as UTC, minus `sec`.
@[inline]
fn c_local_epoch_offset(sec i64, loc_tm C.tm) i64 {
	$if solaris {
		return local_offset_from_fields(sec, &loc_tm)
	} $else {
		return i64(loc_tm.tm_gmtoff)
	}
}

// local_offset_from_fields returns the UTC offset of the local zone at the instant
// `sec`, from `loc_tm`, the localtime_r result for `sec`. It uses only the calendar
// fields, so it works where struct tm has no tm_gmtoff.
@[inline]
fn local_offset_from_fields(sec i64, loc_tm &C.tm) i64 {
	return portable_timegm(loc_tm) - sec
}

fn make_unix_time(t C.tm) i64 {
	return unsafe { i64(C.timegm(&t)) }
}

// localtime_r already resolves the local UTC offset, so callers can reuse it.
@[inline]
fn convert_ctime_with_unix(t C.tm, nanosecond int, unix i64) Time {
	return Time{
		year:       t.tm_year + 1900
		month:      t.tm_mon + 1
		day:        t.tm_mday
		hour:       t.tm_hour
		minute:     t.tm_min
		second:     t.tm_sec
		nanosecond: nanosecond
		unix:       unix
		is_local:   true
	}
}

// local returns t with the location set to local time.
pub fn (t Time) local() Time {
	if t.is_local {
		return t
	}
	loc_tm := C.tm{}
	t_ := t.unix()
	C.localtime_r(voidptr(&t_), &loc_tm)
	return convert_ctime_with_unix(loc_tm, t.nanosecond, t_ + c_local_epoch_offset(t_, loc_tm))
}

// in most systems, these are __quad_t, which is an i64
pub struct C.timespec {
pub mut:
	tv_sec  i64
	tv_nsec i64
}

// the first arg is defined in include/bits/types.h as `__S32_TYPE`, which is `int`
fn C.clock_gettime(i32, &C.timespec) i32

fn C.nanosleep(req &C.timespec, rem &C.timespec) i32

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
	return convert_ctime_with_unix(loc_tm, int(ts.tv_nsec), i64(ts.tv_sec) + c_local_epoch_offset(i64(ts.tv_sec),
		loc_tm))
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
