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

// unix_time returns Unix time.
[inline]
pub fn (t Time) unix_time() i64 {
	return t.unix
}

// unix_time_milli returns Unix time with millisecond resolution.
[inline]
pub fn (t Time) unix_time_milli() i64 {
	return t.unix * 1000 + (t.microsecond / 1000)
}

// add returns a new time that duration is added
pub fn (t Time) add(d Duration) Time {
	microseconds := i64(t.unix) * 1_000_000 + t.microsecond + d.microseconds()
	unix := microseconds / 1_000_000
	micro := microseconds % 1_000_000
	return unix2(unix, int(micro))
}

// add_seconds returns a new time struct with an added number of seconds.
pub fn (t Time) add_seconds(seconds int) Time {
	return t.add(seconds * second)
}

// add_days returns a new time struct with an added number of days.
pub fn (t Time) add_days(days int) Time {
	return t.add(days * 24 * hour)
}

// since returns a number of seconds elapsed since a given time.
fn since(t Time) int {
	// TODO Use time.Duration instead of seconds
	return 0
}

// relative returns a string representation of the difference between t
// and the current time.
pub fn (t Time) relative() string {
	znow := now()
	secs := znow.unix - t.unix
	if secs <= 30 {
		// right now or in the future
		// TODO handle time in the future
		return 'now'
	}
	if secs < 60 {
		return '1m'
	}
	if secs < 3600 {
		m := secs / 60
		if m == 1 {
			return '1 minute ago'
		}
		return '$m minutes ago'
	}
	if secs < 3600 * 24 {
		h := secs / 3600
		if h == 1 {
			return '1 hour ago'
		}
		return '$h hours ago'
	}
	if secs < 3600 * 24 * 5 {
		d := secs / 3600 / 24
		if d == 1 {
			return '1 day ago'
		}
		return '$d days ago'
	}
	if secs > 3600 * 24 * 10000 {
		return ''
	}
	return t.md()
}

// relative_short returns a string saying how long ago a time occured as follows:
// 0-30 seconds: `"now"`; 30-60 seconds: `"1m"`; anything else is rounded to the
// nearest minute, hour or day; anything higher than 10000 days (about 27 years)
// years returns an empty string.
// Some Examples:
// `0s -> 'now'`;
// `20s -> 'now'`;
// `47s -> '1m'`;
// `456s -> '7m'`;
// `1234s -> '20m'`;
// `16834s -> '4h'`;
// `1687440s -> '33d'`;
// `15842354871s -> ''`
pub fn (t Time) relative_short() string {
	znow := now()
	secs := znow.unix - t.unix
	if secs <= 30 {
		// right now or in the future
		// TODO handle time in the future
		return 'now'
	}
	if secs < 60 {
		return '1m'
	}
	if secs < 3600 {
		return '${secs / 60}m'
	}
	if secs < 3600 * 24 {
		return '${secs / 3600}h'
	}
	if secs < 3600 * 24 * 5 {
		return '${secs / 3600 / 24}d'
	}
	if secs > 3600 * 24 * 10000 {
		return ''
	}
	return t.md()
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

// is_leap_year checks if a given a year is a leap year.
pub fn is_leap_year(year int) bool {
	return (year % 4 == 0) && (year % 100 != 0 || year % 400 == 0)
}

// days_in_month returns a number of days in a given month.
pub fn days_in_month(month int, year int) ?int {
	if month > 12 || month < 1 {
		return error('Invalid month: $month')
	}
	extra := if month == 2 && is_leap_year(year) { 1 } else { 0 }
	res := month_days[month - 1] + extra
	return res
}

// str returns time in the same format as `parse` expects ("YYYY-MM-DD HH:MM:SS").
pub fn (t Time) str() string {
	// TODO Define common default format for
	// `str` and `parse` and use it in both ways
	return t.format_ss()
}

// str returns time in the same format as `parse` expects ("YYYY-MM-DD HH:MM:SS").
pub fn (t Time) debug() string {
	return 'Time{ year: ${t.year:04} month: ${t.month:02} day: ${t.day:02} hour: ${t.hour:02} minute: ${t.minute:02} second: ${t.second:02} microsecond: ${t.microsecond:06} unix: ${t.unix:07} }'
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

// offset returns time zone UTC offset in seconds.
pub fn offset() int {
	t := now()
	local := t.local()
	return int(local.unix - t.unix)
}
