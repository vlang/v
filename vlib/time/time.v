// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

import rand

// Common time spans in seconds.
// For usage in expressions like 
// `time.now().uni + 12 * days`
const (
	second  =   1
	seconds =   1
	minute  =  60 * second
	minutes =  60 * second
	hour    =  60 * minute
	hours   =  60 * minute
	day     =  24 * hour
	days    =  24 * hour
	week    =   7 * day
	weeks   =   7 * day
	month   =  30 * day
	months  =  30 * day
	year    = 365 * day
	years   = 365 * day
)

const (
	month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
)

const (
	// The unsigned zero year for internal calculations.
	// Must be 1 mod 400, and times before it will not compute correctly,
	// but otherwise can be changed at will.
	absolute_zero_year = i64(-292277022399)

	seconds_per_minute = 60
	seconds_per_hour   = 60 * seconds_per_minute
	seconds_per_day    = 24 * seconds_per_hour
	seconds_per_week   = 7 * seconds_per_day
	days_per_400_years  = 365*400 + 97
	days_per_100_years  = 365*100 + 24
	days_per_4_years    = 365*4 + 1

 	days_before = [
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

const (
	months_string = 'JanFebMarAprMayJunJulAugSepOctNovDec'
	days_string = 'MonTueWedThuFriSatSun'
)

// `struct Time` represents local time.
// `uni` field is Unit time and is always UTC.
struct Time {
pub:
	year int
	month int
	day int
	hour int
	minute int
	second int
	weekday int
	day_of_year int
	utc_diff int
	is_dst bool
	uni int // TODO it's safe to use "unix" now
}

// fn remove_me_when_c_bug_is_fixed() { // TODO
// }

// #include <time.h>

struct C.time_t {}

struct C.tm {
	tm_sec int
	tm_min int
	tm_hour int
	tm_mday int
	tm_mon int
	tm_year int
	tm_wday int
	tm_yday int
	tm_isdst int
}

fn C.time(int) C.time_t
fn C.localtime(int) &C.tm

// now() returns local time with time zone time difference
// and Daylight Saving Time status if available.
pub fn now() Time {
	t := C.time(0)
	mut now := &C.tm{!}
	now = C.localtime(&t)
	return from_tm(now)
}

// from_unix_to_local(uni int) converts Unix time to `struct Time`.
// Time in `struct Time` is represented as local time.
// `utc_diff` field is set to the time difference from UTC in seconds.
// `is_dst` field is `true` if Daylight Saving Time is used.
// Works on time range 1901-12-13 20:45:52 UTC to 2038-01-19 03:14:07 UTC
pub fn from_unix_to_local(uni int) Time {
	// must convert to `time_t`, because `time_t` can be bigger than `int`
	// C.localtime expects &time_t and would refer to wrong memory if given &int
	t := time_t(uni)
	mut tm := &C.tm{!}
	tm = C.localtime(&t)
	if tm == C.NULL { panic('Converting Unix time to local time failed') }
	return from_tm(tm)	
}

// from_unix(uni i64) converts Unix time to `struct Time`.
// Time in `struct Time` is represented as UTC time.
// Works on time range of Y1970 +/- 2^63 seconds (~ +/- 209 billion years).
// Based on the algorithm of Howard Hinnant.
// http://howardhinnant.github.io/date_algorithms.html#civil_from_days
pub fn from_unix_to_utc(uni int) Time {
    z := int(uni / (24 * 60 * 60)) + 719468
    era := (if z >= 0 { z } else { z - 146096}) / 146097
    doe := z - era * 146097
    yoe := (doe - doe / 1460 + doe / 36524 - doe / 146096) / 365
    doy := doe - (365 * yoe + yoe / 4 - yoe / 100)
    mp := (5 * doy + 2) / 153
    mut d := doy - (153 * mp + 2) / 5 + 1
    m := mp + (if mp < 10 { 3 } else { -9 })
    y := yoe + era * 400 + (if m > 2 { 0 } else { 1 })

    mut t := int(uni % (24 * 60 * 60))
	if t < 0 {
		d = d - 1
		t = 24 * hour + t
	}
    h := t / (60 * 60)
    k := t % (60 * 60) / 60
    s := t % 60

    return Time {
        year   : y
        month  : m
        day    : d
        hour   : h
        minute : k
        second : s
		weekday : 0 // TODO
		day_of_year : 0 // TODO
		utc_diff : 0
		is_dst : false
        uni    : uni
    }
}

// from_tm(t tm) converts `struct C.tm` data to `struct Time` format.
// Works on time range 1901-12-13 20:45:52 UTC to 2038-01-19 03:14:07 UTC
pub fn from_tm(t tm) Time {
	res := Time {
		year : t.tm_year + 1900
		month : t.tm_mon + 1
		day : t.tm_mday
		hour : t.tm_hour
		minute : t.tm_min
		second : t.tm_sec
		weekday : if t.tm_wday == 0 { 7 } else { t.tm_wday }
		day_of_year : t.tm_yday + 1
		is_dst : t.tm_isdst == 1
		uni : C.mktime(&t)
	}
	return { res | utc_diff : res.to_unix() - res.uni }
}

// to_unix(t Time) calculates the Unix timestamp (the number of seconds that
// have elapsed since the Unix epoch, that is the time 00:00:00 UTC on 1 January 1970,
// minus leap seconds) from `struct Time`. Time in `t` is interpreted as UTC time.
// Based on the algorithm of Howard Hinnant.
// http://howardhinnant.github.io/date_algorithms.html#days_from_civil
pub fn (t Time) to_unix() int {
    y := if t.month > 2 { t.year } else { t.year - 1 }
    era := (if y >= 0 { y } else { (y - 399) }) / 400
    yoe := y - era * 400
    doy := (153 * (t.month + (if t.month > 2 { -3 } else { 9 })) + 2) / 5 + t.day - 1
    doe := yoe * 365 + yoe / 4 - yoe / 100 + doy
    sod := t.hour * 60 * 60 + t.minute * 60 + t.second
    return (era * 146097 + doe - 719468) * 24 * 60 * 60 + sod
}

// random() returns random local time in the range
// from 1970-01-01 00:00:00:00 UTC to 2038-01-19 03:14:07 UTC.
// Import module `random` and use `rand.seed(time.now().uni)`
// to get different random times on every run of your program.
pub fn random() Time {
	rand_unix := rand.next(0x7fffffff)
	return from_unix_to_local(rand_unix)
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
	return days_string.substr(i * 3, (i + 1) * 3)
}

// is_leap_year(year int) determines whether a year is a leap year.
pub fn is_leap_year(year int) bool {
	return (year % 4 == 0) && (year % 100 != 0 || year % 400 == 0)
}

// Returns number of days in month
pub fn days_in_month(month, year int) ?int {
	if month > 12 || month < 1 {
		return error('Invalid month: $month')
	}
	extra :=	if month == 2 && is_leap_year(year) {1} else {0}
	res := month_days[month-1] + extra
	return res
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
	return from_unix_to_local(t.uni + seconds)
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

pub fn (t Time) format_ss() string {
	return '${t.year}-${t.month:02d}-${t.day:02d} ${t.hour:02d}:${t.minute:02d}:${t.second:02d}'
}

pub fn (t Time) format() string {
	return '${t.year}-${t.month:02d}-${t.day:02d} ${t.hour:02d}:${t.minute:02d}'
}

pub fn (t Time) smonth() string {
	i := t.month - 1
	return months_string.substr(i * 3, (i + 1) * 3)
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

// 05.02.2012
pub fn (t Time) ddmmy() string {
	return '${t.day:02d}.${t.month:02d}.${t.year}'
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