// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

#include <time.h>

pub const (
	days_string        = 'MonTueWedThuFriSatSun'
	month_days         = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	months_string      = 'JanFebMarAprMayJunJulAugSepOctNovDec'
	// The unsigned zero year for internal calculations.
	// Must be 1 mod 400, and times before it will not compute correctly,
	// but otherwise can be changed at will.
	absolute_zero_year = i64(-292277022399) // as i64
	seconds_per_minute = 60
	seconds_per_hour   = 60 * seconds_per_minute
	seconds_per_day    = 24 * seconds_per_hour
	seconds_per_week   = 7 * seconds_per_day
	days_per_400_years = 365 * 400 + 97
	days_per_100_years = 365 * 100 + 24
	days_per_4_years   = 365 * 4 + 1
	days_before        = [
		0,
		31,
		31 + 28,
		31 + 28 + 31,
		31 + 28 + 31 + 30,
		31 + 28 + 31 + 30 + 31,
		31 + 28 + 31 + 30 + 31 + 30,
		31 + 28 + 31 + 30 + 31 + 30 + 31,
		31 + 28 + 31 + 30 + 31 + 30 + 31 + 31,
		31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30,
		31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31,
		31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30,
		31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31,
	]
	long_days          = ['Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday']
)

// Time contains various time units for a point in time.
pub struct Time {
pub:
	year        int
	month       int
	day         int
	hour        int
	minute      int
	second      int
	microsecond int
	unix        u64
}

// FormatDelimiter contains different time formats.
pub enum FormatTime {
	hhmm12
	hhmm24
	hhmmss12
	hhmmss24
	hhmmss24_milli
	hhmmss24_micro
	no_time
}

// FormatDelimiter contains different date formats.
pub enum FormatDate {
	ddmmyy
	ddmmyyyy
	mmddyy
	mmddyyyy
	mmmd
	mmmdd
	mmmddyyyy
	no_date
	yyyymmdd
}

// FormatDelimiter contains different time/date delimiters.
pub enum FormatDelimiter {
	dot
	hyphen
	slash
	space
	no_delimiter
}

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
	return convert_ctime(now, 0)
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
	return unix2(int(t), 0)
}

// smonth returns month name.
pub fn (t Time) smonth() string {
	if t.month <= 0 || t.month > 12 {
		return '---'
	}
	i := t.month - 1
	return time.months_string[i * 3..(i + 1) * 3]
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
	utime := u64(make_unix_time(tt))
	return Time{
		...t
		unix: utime
	}
}

// unix_time returns Unix time.
[inline]
pub fn (t Time) unix_time() int {
	return int(t.unix)
}

// unix_time_milli returns Unix time with millisecond resolution.
[inline]
pub fn (t Time) unix_time_milli() u64 {
	return t.unix * 1000 + u64(t.microsecond / 1000)
}

// add returns a new time that duration is added
pub fn (t Time) add(d Duration) Time {
	microseconds := i64(t.unix) * 1000 * 1000 + t.microsecond + d.microseconds()
	unix := microseconds / (1000 * 1000)
	micro := microseconds % (1000 * 1000)
	return unix2(int(unix), int(micro))
}

// add_seconds returns a new time struct with an added number of seconds.
pub fn (t Time) add_seconds(seconds int) Time {
	return t.add(seconds * time.second)
}

// add_days returns a new time struct with an added number of days.
pub fn (t Time) add_days(days int) Time {
	return t.add(days * 24 * time.hour)
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

// day_of_week returns the current day of a given year, month, and day,
// as an integer.
pub fn day_of_week(y int, m int, d int) int {
	// Sakomotho's algorithm is explained here:
	// https://stackoverflow.com/a/6385934
	t := [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
	mut sy := y
	if m < 3 {
		sy = sy - 1
	}
	return (sy + sy / 4 - sy / 100 + sy / 400 + t[m - 1] + d - 1) % 7 + 1
}

// day_of_week returns the current day as an integer.
pub fn (t Time) day_of_week() int {
	return day_of_week(t.year, t.month, t.day)
}

// weekday_str returns the current day as a string.
pub fn (t Time) weekday_str() string {
	i := t.day_of_week() - 1
	return time.days_string[i * 3..(i + 1) * 3]
}

// weekday_str returns the current day as a string.
pub fn (t Time) long_weekday_str() string {
	i := t.day_of_week() - 1
	return time.long_days[i]
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

// sleep_ms makes the calling thread sleep for a given number of milliseconds.
[deprecated: 'call time.sleep(n * time.millisecond)']
pub fn sleep_ms(milliseconds int) {
	wait(milliseconds * time.millisecond)
}

// usleep makes the calling thread sleep for a given number of microseconds.
[deprecated: 'call time.sleep(n * time.microsecond)']
pub fn usleep(microseconds int) {
	wait(microseconds * time.microsecond)
}

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
	res := time.month_days[month - 1] + extra
	return res
}

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
		microsecond: time.microsecond
		unix: u64(make_unix_time(t))
	}
}

// A lot of these are taken from the Go library.
pub type Duration = i64

pub const (
	nanosecond  = Duration(1)
	microsecond = Duration(1000 * nanosecond)
	millisecond = Duration(1000 * microsecond)
	second      = Duration(1000 * millisecond)
	minute      = Duration(60 * second)
	hour        = Duration(60 * minute)
	infinite    = Duration(-1)
)

// nanoseconds returns the duration as an integer number of nanoseconds.
pub fn (d Duration) nanoseconds() i64 {
	return i64(d)
}

// microseconds returns the duration as an integer number of microseconds.
pub fn (d Duration) microseconds() i64 {
	return i64(d) / 1000
}

// milliseconds returns the duration as an integer number of milliseconds.
pub fn (d Duration) milliseconds() i64 {
	return i64(d) / 1000000
}

// The following functions return floating point numbers because it's common to
// consider all of them in sub-one intervals
// seconds returns the duration as a floating point number of seconds.
pub fn (d Duration) seconds() f64 {
	sec := d / time.second
	nsec := d % time.second
	return f64(sec) + f64(nsec) / 1e9
}

// minutes returns the duration as a floating point number of minutes.
pub fn (d Duration) minutes() f64 {
	min := d / time.minute
	nsec := d % time.minute
	return f64(min) + f64(nsec) / (60 * 1e9)
}

// hours returns the duration as a floating point number of hours.
pub fn (d Duration) hours() f64 {
	hr := d / time.hour
	nsec := d % time.hour
	return f64(hr) + f64(nsec) / (60 * 60 * 1e9)
}

// offset returns time zone UTC offset in seconds.
pub fn offset() int {
	t := now()
	local := t.local()
	return int(local.unix - t.unix)
}
