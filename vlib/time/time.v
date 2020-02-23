// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

#include <time.h>

const (
	days_string = 'MonTueWedThuFriSatSun'
	month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	months_string = 'JanFebMarAprMayJunJulAugSepOctNovDec'
	// The unsigned zero year for internal calculations.
	// Must be 1 mod 400, and times before it will not compute correctly,
	// but otherwise can be changed at will.
	absolute_zero_year = i64(-292277022399 )//as i64
	seconds_per_minute = 60
	seconds_per_hour = 60 * seconds_per_minute
	seconds_per_day = 24 * seconds_per_hour
	seconds_per_week = 7 * seconds_per_day
	days_per_400_years = 365 * 400 + 97
	days_per_100_years = 365 * 100 + 24
	days_per_4_years = 365 * 4 + 1
	days_before = [0, 31, 31 + 28, 31 + 28 + 31, 31 + 28 + 31 + 30, 31 + 28 + 31 + 30 + 31, 31 + 28 + 31 + 30 + 31 + 30, 31 + 28 + 31 + 30 + 31 + 30 + 31, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31, ]
)

pub struct Time {
pub:
	year   int
	month  int
	day    int
	hour   int
	minute int
	second int
	unix int
}

pub enum FormatTime {
	hhmm12
	hhmm24
	hhmmss12
	hhmmss24
	no_time
}

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

pub enum FormatDelimiter {
	dot
	hyphen
	slash
	space
}

pub struct C.time_t {}

pub struct C.timeval {
	tv_sec  u64
	tv_usec u64
}

fn C.localtime(int) &C.tm

fn C.time(int) C.time_t

// now returns current local time.
pub fn now() Time {
	t := C.time(0)
	mut now := &C.tm(0)
	now = C.localtime(&t)
	return convert_ctime(now)
}

// smonth returns month name.
pub fn (t Time) smonth() string {
	i := t.month - 1
	return months_string[i * 3..(i + 1) * 3]
}

// new_time returns a time struct with calculated Unix time.
pub fn new_time(t Time) Time {
	return Time{
		year: t.year
		month: t.month
		day: t.day
		hour: t.hour
		minute: t.minute
		second: t.second
		unix: t.calc_unix()
	}
	// TODO Use the syntax below when it works with reserved keywords like `unix`
	// return {
	// 	t |
	// 	unix:t.calc_unix()
	// }
}

// calc_unix returns Unix time.
pub fn (t &Time) calc_unix() int {
	if t.unix != 0 {
		return t.unix
	}
	tt := C.tm{
		tm_sec: t.second
		tm_min: t.minute
		tm_hour: t.hour
		tm_mday: t.day
		tm_mon: t.month - 1
		tm_year: t.year - 1900
	}
	return make_unix_time(tt)
}

// add_days returns a new time struct with an added number of seconds.
pub fn (t Time) add_seconds(seconds int) Time {
	// TODO Add(d time.Duration)
	return unix(t.unix + seconds)
}

// add_days returns a new time struct with an added number of days.
pub fn (t Time) add_days(days int) Time {
	return unix(t.unix + days * 3600 * 24)
}

// since returns a number of seconds elapsed since a given time.
fn since(t Time) int {
	// TODO Use time.Duration instead of seconds
	return 0
}

// relative returns a string representation of difference between time
// and current time.
pub fn (t Time) relative() string {
	now := time.now()
	secs := now.unix - t.unix
	if secs <= 30 {
		// right now or in the future
		// TODO handle time in the future
		return 'now'
	}
	if secs < 60 {
		return '1m'
	}
	if secs < 3600 {
		return '${secs/60}m'
	}
	if secs < 3600 * 24 {
		return '${secs/3600}h'
	}
	if secs < 3600 * 24 * 5 {
		return '${secs/3600/24}d'
	}
	if secs > 3600 * 24 * 10000 {
		return ''
	}
	return t.md()
}

// day_of_week returns the current day of a given year, month, and day,
// as an integer.
pub fn day_of_week(y, m, d int) int {
	// Sakomotho's algorithm is explained here:
	// https://stackoverflow.com/a/6385934
	t := [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
	mut sy := y
	if (m < 3) {
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
	return days_string[i * 3..(i + 1) * 3]
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

// sleep makes the calling thread sleep for a given number of seconds.
pub fn sleep(seconds int) {
	$if windows {
		C.Sleep(seconds * 1000)
	} $else {
		C.sleep(seconds)
	}
}

// sleep_ms makes the calling thread sleep for a given number of milliseconds.
pub fn sleep_ms(milliseconds int) {
	$if windows {
		C.Sleep(milliseconds)
	} $else {
		C.usleep(milliseconds * 1000)
	}
}

// usleep makes the calling thread sleep for a given number of microseconds.
pub fn usleep(microseconds int) {
	$if windows {
		milliseconds := microseconds / 1000
		C.Sleep(milliseconds)
	} $else {
		C.usleep(microseconds)
	}
}

// is_leap_year checks if a given a year is a leap year.
pub fn is_leap_year(year int) bool {
	return (year % 4 == 0) && (year % 100 != 0 || year % 400 == 0)
}

// days_in_month returns a number of days in a given month.
pub fn days_in_month(month, year int) ?int {
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

fn convert_ctime(t C.tm) Time {
	return Time{
		year: t.tm_year + 1900
		month: t.tm_mon + 1
		day: t.tm_mday
		hour: t.tm_hour
		minute: t.tm_min
		second: t.tm_sec
		unix: make_unix_time(t)
	}
}
