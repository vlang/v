// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

import rand

const (
	days_string = 'MonTueWedThuFriSatSun'
	month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
	months_string = 'JanFebMarAprMayJunJulAugSepOctNovDec'
	// The unsigned zero year for internal calculations.
	// Must be 1 mod 400, and times before it will not compute correctly,
	// but otherwise can be changed at will.
	absolute_zero_year = i64(-292277022399)
	seconds_per_minute = 60
	seconds_per_hour = 60 * seconds_per_minute
	seconds_per_day = 24 * seconds_per_hour
	seconds_per_week = 7 * seconds_per_day
	days_per_400_years = 365 * 400 + 97
	days_per_100_years = 365 * 100 + 24
	days_per_4_years = 365 * 4 + 1
	days_before = [0, 31, 31 + 28, 31 + 28 + 31, 31 + 28 + 31 + 30, 31 + 28 + 31 + 30 + 31, 31 + 28 + 31 + 30 + 31 + 30, 31 + 28 + 31 + 30 + 31 + 30 + 31, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30, 31 + 28 + 31 + 30 + 31 + 30 + 31 + 31 + 30 + 31 + 30 + 31, ]
)

#include <time.h>
pub struct Time {
pub:
	year   int
	month  int
	day    int
	hour   int
	minute int
	second int
	uni    int // TODO it's safe to use "unix" now
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

fn C.localtime(int) &C.tm


fn remove_me_when_c_bug_is_fixed() {
	// TODO
}

pub struct C.time_t {
}

struct C.tm {
	tm_year int
	tm_mon  int
	tm_mday int
	tm_hour int
	tm_min  int
	tm_sec  int
}

fn C.time(int) C.time_t


pub fn now() Time {
	t := C.time(0)
	mut now := &C.tm(0)
	now = C.localtime(&t)
	return convert_ctime(now)
}

pub fn random() Time {
	now_unix := now().uni
	rand_unix := rand.next(now_unix)
	return time.unix(rand_unix)
}

// Based on Go's time package.
// Copyright 2009 The Go Authors.
pub fn unix(abs int) Time {
	// Split into time and day.
	mut d := abs / seconds_per_day
	// Account for 400 year cycles.
	mut n := d / days_per_400_years
	mut y := 400 * n
	d -= days_per_400_years * n
	// Cut off 100-year cycles.
	// The last cycle has one extra leap year, so on the last day
	// of that year, day / days_per_100_years will be 4 instead of 3.
	// Cut it back down to 3 by subtracting n>>2.
	n = d / days_per_100_years
	n -= n>>2
	y += 100 * n
	d -= days_per_100_years * n
	// Cut off 4-year cycles.
	// The last cycle has a missing leap year, which does not
	// affect the computation.
	n = d / days_per_4_years
	y += 4 * n
	d -= days_per_4_years * n
	// Cut off years within a 4-year cycle.
	// The last year is a leap year, so on the last day of that year,
	// day / 365 will be 4 instead of 3. Cut it back down to 3
	// by subtracting n>>2.
	n = d / 365
	n -= n>>2
	y += n
	d -= 365 * n
	yday := d
	mut day := yday
	year := abs / int(3.154e+7) + 1970 // int(i64(y) + absolute_zero_year)
	hour := (abs % seconds_per_day) / seconds_per_hour
	minute := (abs % seconds_per_hour) / seconds_per_minute
	second := (abs % seconds_per_minute)
	if is_leap_year(year) {
		// Leap year
		if day > 31 + 29 - 1 {
			// After leap day; pretend it wasn't there.
			day--
		}
		else if day == 31 + 29 - 1 {
			// Leap day.
			day = 29
			return Time{
				year: year
				month: 2
				day: day
				hour: hour
				minute: minute
				second: second
			}
		}
	}
	// Estimate month on assumption that every month has 31 days.
	// The estimate may be too low by at most one month, so adjust.
	mut month := day / 31
	mut begin := 0
	end := (days_before[month + 1])
	if day >= end {
		month++
		begin = end
	}
	else {
		begin = (days_before[month])
	}
	month++ // because January is 1
	day = day - begin + 1
	return Time{
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		uni: abs
	}
}

pub fn convert_ctime(t tm) Time {
	return Time{
		year: t.tm_year + 1900
		month: t.tm_mon + 1
		day: t.tm_mday
		hour: t.tm_hour
		minute: t.tm_min
		second: t.tm_sec
		uni: C.mktime(&t)
	}
}

// format_ss  returns a string for t in a given format YYYY-MM-DD HH:MM:SS in
// 24h notation
// @param
// @return    string
// @example   1980-07-11 21:23:42
pub fn (t Time) format_ss() string {
	return t.get_fmt_str(.hyphen, .hhmmss24, .yyyymmdd)
}

// format_ss  returns a string for t in a given format YYYY-MM-DD HH:MM in 24h
// notation
// @param
// @return    string
// @example   1980-07-11 21:23
pub fn (t Time) format() string {
	return t.get_fmt_str(.hyphen, .hhmm24, .yyyymmdd)
}

pub fn (t Time) smonth() string {
	i := t.month - 1
	return months_string[i * 3..(i + 1) * 3]
}

// hhmm     returns a string for t in the given format HH:MM in 24h notation
// @example 21:04
pub fn (t Time) hhmm() string {
	return t.get_fmt_time_str(.hhmm24)
}

/*
fn (t Time) hhmm_tmp() string {
	return '${t.hour:02d}:${t.minute:02d}'
}
*/

// hhmm12   returns a string for t in the given format HH:MM in 12h notation
pub fn (t Time) hhmm12() string {
	return t.get_fmt_time_str(.hhmm12)
}

// hhmmss   returns a string for t in the given format HH:MM:SS in 24h notation
pub fn (t Time) hhmmss() string {
	return t.get_fmt_time_str(.hhmmss24)
}

// ymmdd    returns a string for t in the given format YYYY-MM-DD
pub fn (t Time) ymmdd() string {
	return t.get_fmt_date_str(.hyphen, .yyyymmdd)
}

// ddmmy    returns a string for t in the given format DD.MM.YYYY
pub fn (t Time) ddmmy() string {
	return t.get_fmt_date_str(.dot, .ddmmyyyy)
}

// md       returns a string for t in the given format MMM D
pub fn (t Time) md() string {
	return t.get_fmt_date_str(.space, .mmmd)
}

pub fn (t Time) clean() string {
	nowe := time.now()
	// if amtime {
	// hm = t.Format("3:04 pm")
	// }
	// Today
	if t.month == nowe.month && t.year == nowe.year && t.day == nowe.day {
		return t.get_fmt_time_str(.hhmm24)
	}
	// This week
	// if time.Since(t) < 24*7*time.Hour {
	// return t.Weekday().String()[:3] + " " + hm
	// }
	// This year
	if t.year == nowe.year {
		return t.get_fmt_str(.space, .hhmm24, .mmmd)
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
		return t.get_fmt_time_str(.hhmm12)
	}
	// This week
	// if time.Since(t) < 24*7*time.Hour {
	// return t.Weekday().String()[:3] + " " + hm
	// }
	// This year
	if t.year == nowe.year {
		return t.get_fmt_str(.space, .hhmm12, .mmmd)
	}
	return t.format()
	// return fmt.Sprintf("%4d/%02d/%02d", t.Year(), t.Month(), t.Day()) + " " + hm
}
// `parse` parses time in the following format: "2018-01-27 12:48:34"
pub fn parse(s string) Time {
	// println('parse="$s"')
	pos := s.index(' ') or {
		println('bad time format')
		return now()
	}
	symd := s[..pos]
	ymd := symd.split('-')
	if ymd.len != 3 {
		println('bad time format')
		return now()
	}
	shms := s[pos..]
	hms := shms.split(':')
	hour := hms[0][1..]
	minute := hms[1]
	second := hms[2]
	// //////////
	return new_time(Time{
		year: ymd[0].int()
		month: ymd[1].int()
		day: ymd[2].int()
		hour: hour.int()
		minute: minute.int()
		second: second.int()
	})
}

// `parse_iso` parses time in the following format: "Thu, 12 Dec 2019 06:07:45 GMT"
pub fn parse_iso(s string) Time {
	fields := s.split(' ')
	if fields.len < 5 {
		return Time{}
	}

	pos := months_string.index(fields[2]) or { return Time{} }
	mm := pos/3 + 1

	tmstr := malloc(s.len*2)
	count := int(C.sprintf(charptr(tmstr), '%s-%02d-%s %s'.str,
		fields[3].str, mm, fields[1].str, fields[4].str))
	return parse(tos(tmstr, count))
}

pub fn new_time(t Time) Time {
	return {
		t |
		uni:t.calc_unix()
	}
}

pub fn (t &Time) calc_unix() int {
	if t.uni != 0 {
		return t.uni
	}
	tt := C.tm{
		tm_sec: t.second
		tm_min: t.minute
		tm_hour: t.hour
		tm_mday: t.day
		tm_mon: t.month - 1
		tm_year: t.year - 1900
	}
	return C.mktime(&tt)
}

// TODO add(d time.Duration)
pub fn (t Time) add_seconds(seconds int) Time {
	return unix(t.uni + seconds)
}

pub fn (t Time) add_days(days int) Time {
	return unix(t.uni + days * 3600 * 24)
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
	return (sy + sy / 4 - sy / 100 + sy / 400 + t[m - 1] + d - 1) % 7 + 1
}

pub fn (t Time) day_of_week() int {
	return day_of_week(t.year, t.month, t.day)
}

// weekday_str() returns the current day in string (upto 3 characters)
pub fn (t Time) weekday_str() string {
	i := t.day_of_week() - 1
	return days_string[i * 3..(i + 1) * 3]
}

pub struct C.timeval {
	tv_sec  u64
	tv_usec u64
}

// in ms
pub fn ticks() i64 {
	$if windows {
		return C.GetTickCount()
	} $else {
		ts := C.timeval{
		}
		C.gettimeofday(&ts, 0)
		return i64(ts.tv_sec * u64(1000) + (ts.tv_usec / u64(1000)))
	}
	/*
	t := i64(C.mach_absolute_time())
	# Nanoseconds elapsedNano = AbsoluteToNanoseconds( *(AbsoluteTime *) &t );
	# return (double)(* (uint64_t *) &elapsedNano) / 1000000;
*/

}

pub fn sleep(seconds int) {
	$if windows {
		C.Sleep(seconds * 1000)
	} $else {
		C.sleep(seconds)
	}
}

pub fn usleep(n int) {
	$if windows {
		// C._usleep(n)
	} $else {
		C.usleep(n)
	}
}

pub fn sleep_ms(n int) {
	$if windows {
		C.Sleep(n)
	} $else {
		C.usleep(n * 1000)
	}
}

// Determine whether a year is a leap year.
pub fn is_leap_year(year int) bool {
	return (year % 4 == 0) && (year % 100 != 0 || year % 400 == 0)
}

// Returns number of days in month
pub fn days_in_month(month, year int) ?int {
	if month > 12 || month < 1 {
		return error('Invalid month: $month')
	}
	extra := if month == 2 && is_leap_year(year) { 1 } else { 0 }
	res := month_days[month - 1] + extra
	return res
}

// get_fmt_time_str   returns a string for time t in a given format
// @param             FormatTime
// @return            string
// @example           21:23:42
pub fn (t Time) get_fmt_time_str(fmt_time FormatTime) string {
	if fmt_time == .no_time {
		return ''
	}
	tp := if t.hour > 11 { 'p.m.' } else { 'a.m.' }
	hour := if t.hour > 12 { t.hour - 12 } else if t.hour == 0 { 12 } else { t.hour }
	return match fmt_time {
		.hhmm12{
			'$hour:${t.minute:02d} $tp'
		}
		.hhmm24{
			'${t.hour:02d}:${t.minute:02d}'
		}
		.hhmmss12{
			'$hour:${t.minute:02d}:${t.second:02d} $tp'
		}
		.hhmmss24{
			'${t.hour:02d}:${t.minute:02d}:${t.second:02d}'
		}
		else {
			'unknown enumeration $fmt_time'}}
}

// get_fmt_date_str   returns a string for t in a given date format
// @param             FormatDelimiter, FormatDate
// @return            string
// @example           11.07.1980
pub fn (t Time) get_fmt_date_str(fmt_dlmtr FormatDelimiter, fmt_date FormatDate) string {
	if fmt_date == .no_date {
		return ''
	}
	month := '${t.smonth()}'
	year := t.year.str()[2..]
	return match fmt_date {
		.ddmmyy{
			'${t.day:02d}|${t.month:02d}|$year'
		}
		.ddmmyyyy{
			'${t.day:02d}|${t.month:02d}|${t.year}'
		}
		.mmddyy{
			'${t.month:02d}|${t.day:02d}|$year'
		}
		.mmddyyyy{
			'${t.month:02d}|${t.day:02d}|${t.year}'
		}
		.mmmd{
			'$month|${t.day}'
		}
		.mmmdd{
			'$month|${t.day:02d}'
		}
		.mmmddyyyy{
			'$month|${t.day:02d}|${t.year}'
		}
		.yyyymmdd{
			'${t.year}|${t.month:02d}|${t.day:02d}'
		}
		else {
			'unknown enumeration $fmt_date'}}.replace('|', match fmt_dlmtr {
		.dot{
			'.'
		}
		.hyphen{
			'-'
		}
		.slash{
			'/'
		}
		.space{
			' '
		}
		else {
			'unknown enumeration $fmt_dlmtr'}})
}

// get_fmt_str  returns a string for t in a given format for time and date
// @param       FormatDelimiter, FormatTime, FormatDate
// @return      string
// @example     11.07.1980 21:23:42
pub fn (t Time) get_fmt_str(fmt_dlmtr FormatDelimiter, fmt_time FormatTime, fmt_date FormatDate) string {
	if fmt_date == .no_date {
		if fmt_time == .no_time {
			// saving one function call although it's checked in
			// t.get_fmt_time_str(fmt_time) in the beginning
			return ''
		}
		else {
			return t.get_fmt_time_str(fmt_time)
		}
	}
	else {
		if fmt_time != .no_time {
			return t.get_fmt_date_str(fmt_dlmtr, fmt_date) + ' ' + t.get_fmt_time_str(fmt_time)
		}
		else {
			return t.get_fmt_date_str(fmt_dlmtr, fmt_date)
		}
	}
}

