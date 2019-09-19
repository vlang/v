// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

import rand

const (
	MonthDays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
)

#include <time.h>

struct Time {
pub:
	year   int
	month  int
	day    int
	hour   int
	minute int
	second int
	uni    int // TODO it's safe to use "unix" now
}


fn C.localtime(int) &C.tm

fn remove_me_when_c_bug_is_fixed() { // TODO
}

struct C.tm {
	tm_year int
	tm_mon  int
	tm_mday int
	tm_hour int
	tm_min  int
	tm_sec  int
}

fn C.time(int) i64

pub fn now() Time {
	t := C.time(0)
	mut now := &C.tm{!}
	now = C.localtime(&t)
	return convert_ctime(now)
}

pub fn random() Time {
	now_unix := now().uni
	rand_unix := rand.next(now_unix)

	return time.unix(rand_unix)
}

const (
// The unsigned zero year for internal calculations.
	// Must be 1 mod 400, and times before it will not compute correctly,
	// but otherwise can be changed at will.
	absoluteZeroYear = i64(-292277022399)

	secondsPerMinute = 60
	secondsPerHour   = 60 * secondsPerMinute
	secondsPerDay    = 24 * secondsPerHour
	secondsPerWeek   = 7 * secondsPerDay
	daysPer400Years  = 365*400 + 97
	daysPer100Years  = 365*100 + 24
	daysPer4Years    = 365*4 + 1

 daysBefore = [
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

)


// Based on Go's time package.
// Copyright 2009 The Go Authors.
pub fn unix(abs int) Time {
	// Split into time and day.
	mut d := abs / secondsPerDay

	// Account for 400 year cycles.
	mut n := d / daysPer400Years
	mut y := 400 * n
	d -= daysPer400Years * n

	// Cut off 100-year cycles.
	// The last cycle has one extra leap year, so on the last day
	// of that year, day / daysPer100Years will be 4 instead of 3.
	// Cut it back down to 3 by subtracting n>>2.
	n = d / daysPer100Years
	n -= n >> 2
	y += 100 * n
	d -= daysPer100Years * n

	// Cut off 4-year cycles.
	// The last cycle has a missing leap year, which does not
	// affect the computation.
	n = d / daysPer4Years
	y += 4 * n
	d -= daysPer4Years * n

	// Cut off years within a 4-year cycle.
	// The last year is a leap year, so on the last day of that year,
	// day / 365 will be 4 instead of 3. Cut it back down to 3
	// by subtracting n>>2.
	n = d / 365
	n -= n >> 2
	y += n
	d -= 365 * n

	yday := int(d)
	mut day := yday

	year := abs / int(3.154e+7) + 1970 //int(i64(y) + absoluteZeroYear)
	hour := int(abs%secondsPerDay) / secondsPerHour
	minute := int(abs % secondsPerHour) / secondsPerMinute
	second := int(abs % secondsPerMinute)
	
	if is_leap_year(year) {
		// Leap year
		if day > 31+29-1 {
			// After leap day; pretend it wasn't there.
			day--
		} 		else if day == 31+29-1 {
			// Leap day.
			day = 29
			return Time{year:year, month:2, day:day, hour:hour, minute: minute, second: second}
		}
	}

	// Estimate month on assumption that every month has 31 days.
	// The estimate may be too low by at most one month, so adjust.
	mut month := day / 31
	mut begin := 0
	end := int(daysBefore[month+1])
	if day >= end {
		month++
		begin = end
	} else {
		begin = int(daysBefore[month])
	}

	month++ // because January is 1
	day = day - begin + 1
	return Time{year:year, month: month, day:day, hour:hour, minute: minute, second: second}
}

pub fn convert_ctime(t tm) Time {
	return Time {
		year: t.tm_year + 1900
		month: t.tm_mon + 1
		day: t.tm_mday
		hour: t.tm_hour
		minute: t.tm_min
		second: t.tm_sec
		uni: C.mktime(&t)
	}
}

pub fn (t Time) format_ss() string {
	return '${t.year}-${t.month:02d}-${t.day:02d} ${t.hour:02d}:${t.minute:02d}:${t.second:02d}'
}

pub fn (t Time) format() string {
	return '${t.year}-${t.month:02d}-${t.day:02d} ${t.hour:02d}:${t.minute:02d}'
}

const (
	Months = 'JanFebMarAprMayJunJulAugSepOctNovDec'
	Days = 'MonTueWedThuFriSatSun'
)

pub fn (t Time) smonth() string {
	i := t.month - 1
	return Months.substr(i * 3, (i + 1) * 3)
}

// 21:04
pub fn (t Time) hhmm() string {
	return '${t.hour:02d}:${t.minute:02d}'
}

/*
fn (t Time) hhmm_tmp() string {
	return '${t.hour:02d}:${t.minute:02d}'
}
*/

// 9:04pm
pub fn (t Time) hhmm12() string {
	mut am := 'am'
	mut hour := t.hour
	if t.hour > 11 {
		am = 'pm'
	}
	if t.hour > 12 {
		hour = hour - 12
	}
	if t.hour == 0 {
		hour = 12
	}
	return '$hour:${t.minute:02d} $am'
}

// 21:04:03
pub fn (t Time) hhmmss() string {
	return '${t.hour:02d}:${t.minute:02d}:${t.second:02d}'
}

// 2012-01-05
pub fn (t Time) ymmdd() string {
	return '${t.year}-${t.month:02d}-${t.day:02d}'
}

// Jul 3
pub fn (t Time) md() string {
	// jl := t.smonth()
	s := '${t.smonth()} $t.day'
	return s
}

pub fn (t Time) clean() string {
	nowe := time.now()
	// if amtime {
	// hm = t.Format("3:04 pm")
	// }
	// Today
	if t.month == nowe.month && t.year == nowe.year && t.day == nowe.day {
		return t.hhmm()
	}
	// This week
	// if time.Since(t) < 24*7*time.Hour {
	// return t.Weekday().String()[:3] + " " + hm
	// }
	// This year
	if t.year == nowe.year {
		return '${t.smonth()} ${t.day} ${t.hhmm()}'
	}
	return t.format()
	// return fmt.Sprintf("%4d/%02d/%02d", t.Year(), t.Month(), t.Day()) + " " + hm
}

pub fn (t Time) clean12() string {
	nowe := time.now()
	// if amtime {
	// hm = t.Format("3:04 pm")
	// }
	// Today
	if t.month == nowe.month && t.year == nowe.year && t.day == nowe.day {
		return t.hhmm12()
	}
	// This week
	// if time.Since(t) < 24*7*time.Hour {
	// return t.Weekday().String()[:3] + " " + hm
	// }
	// This year
	if t.year == nowe.year {
		return '${t.smonth()} ${t.day} ${t.hhmm12()}'
	}
	return t.format()
	// return fmt.Sprintf("%4d/%02d/%02d", t.Year(), t.Month(), t.Day()) + " " + hm
}

// `parse` parses time in the following format: "2018-01-27 12:48:34"
pub fn parse(s string) Time {
	// println('parse="$s"')
	pos := s.index(' ')
	if pos <= 0 {
		println('bad time format')
		return now()
	}
	symd := s.left(pos)
	ymd := symd.split('-')
	if ymd.len != 3 {
		println('bad time format')
		return now()
	}
	shms := s.right(pos)
	hms := shms.split(':')
	hour := hms[0]
	minute := hms[1]
	second := hms[2]
	// //////////
	return new_time(Time {
		year: ymd[0].int()
		month: ymd[1].int()
		day: ymd[2].int()
		hour: hour.int()
		minute: minute.int()
		second: second.int()
	})
}

pub fn new_time(t Time) Time {
	return{t | uni: t.calc_unix()}
}

pub fn (t &Time) calc_unix() int {
	if t.uni != 0  {
		return t.uni
	}
	tt := C.tm{
	tm_sec : t.second
	tm_min : t.minute
	tm_hour : t.hour
	tm_mday : t.day
	tm_mon : t.month-1
	tm_year : t.year - 1900
	}
	return C.mktime(&tt)
}

// TODO add(d time.Duration)
pub fn (t Time) add_seconds(seconds int) Time {
	return unix(t.uni + seconds)
}

// TODO use time.Duration instead of seconds
fn since(t Time) int {
	return 0
}

pub fn (t Time) relative() string {
	now := time.now()
	secs := now.uni - t.uni
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

pub fn day_of_week(y, m, d int) int {
	// Sakomotho's algorithm is explained here:
	// https://stackoverflow.com/a/6385934
	t := [0, 3, 2, 5, 0, 3, 5, 1, 4, 6, 2, 4]
	mut sy := y
	if (m < 3) {
		sy = sy - 1
	}
    return ( sy + sy/4 - sy/100 + sy/400 + t[m-1] + d - 1) % 7 + 1
}

pub fn (t Time) day_of_week() int {
	return day_of_week(t.year, t.month, t.day)
}

// weekday_str() returns the current day in string (upto 3 characters)
pub fn (t Time) weekday_str() string {
	i := t.day_of_week() - 1
	return Days.substr(i * 3, (i + 1) * 3)
}

struct C.timeval  {
	tv_sec int
	tv_usec int
}

// in ms
pub fn ticks() i64 {
	$if windows {
		return C.GetTickCount()
	}
	$else {
		ts := C.timeval{}
		C.gettimeofday(&ts,0)
		return ts.tv_sec * 1000 + (ts.tv_usec / 1000)
	}

/*
	t := i64(C.mach_absolute_time())
	# Nanoseconds elapsedNano = AbsoluteToNanoseconds( *(AbsoluteTime *) &t );
	# return (double)(* (uint64_t *) &elapsedNano) / 1000000;
*/
}

pub fn sleep(seconds int) {
	$if windows {
		C._sleep(seconds * 1000)
	}
	$else {
		C.sleep(seconds)
	}
}

pub fn usleep(n int) {
$if windows {
	//C._usleep(n)
}
$else {
	C.usleep(n)
}
}

pub fn sleep_ms(n int) {
	$if windows {
		C.Sleep(n)
	}
	$else {
		C.usleep(n * 1000)
	}
}

// Determine whether a year is a leap year.
pub fn is_leap_year(year int) bool {
	return (year%4 == 0) && (year%100 != 0 || year%400 == 0)
}

// Returns number of days in month
pub fn days_in_month(month, year int) ?int {
	if month > 12 || month < 1 {
		return error('Invalid month: $month')
	}
	extra :=	if month == 2 && is_leap_year(year) {1} else {0}
	res := MonthDays[month-1] + extra
	return res
}
