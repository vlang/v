// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

import rand

#include <time.h>
struct Time {
pub:
	year   int
	day    int
	month  int
	hour   int
	minute int
	second int
	uni    int // TODO it's safe to use "unix" now
}

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

pub fn now() Time {
	# time_t t = time(0);
	// t := C.time(0)
	# struct tm * now = localtime(&t);
	res := Time{}
	# res.year = now->tm_year + 1900;
	# res.month = now->tm_mon + 1;
	# res.day = now->tm_mday;
	# res.hour = now->tm_hour;
	# res.minute = now->tm_min;
	# res.second = now->tm_sec;
	# res.uni = (int)t;
	// # res.ms = now->tm_msec;
	return res
}

// fn now() Time {
// t := C.time(0)
// now := localtime(&t)
// return Time{
// year: now.tm_year + 1900
// month : now.tm_mon + 1
// day : now.tm_mday
// hour : now.tm_hour
// minute : now.tm_min
// second : now.tm_sec
// uni : int(t)
// }
// }
pub fn random() Time {
	return Time {
		year: rand.next(2) + 201
		month: rand.next(12) + 1
		day: rand.next(30) + 1
		hour: rand.next(24)
		minute: rand.next(60)
		second: rand.next(60)
	}
}

pub fn unix(u string) Time {
	// println('unix time($u)')
	// # int aa = atoi(u.str);
	// #printf("!!!! %d\n", aa);
	# int uni = atoi(u.str);
	# time_t t = (time_t)uni;
	# struct tm * now = localtime(&t);
	// println('got tm')
	// TODO COPY PASTA
	res := Time{}
	# res.year = now->tm_year + 1900;
	# res.month = now->tm_mon + 1;
	# res.day = now->tm_mday;
	# res.hour = now->tm_hour;
	# res.minute = now->tm_min;
	# res.second = now->tm_sec;
	# res.uni = uni;
	// println('end unix')
	return res
}

pub fn convert_ctime(t tm) Time {
	return Time {
		year: t.tm_year + 1900
		month: t.tm_mon + 1
		day: t.tm_mday
		hour: t.tm_hour
		minute: t.tm_min
		second: t.tm_sec
	}
	// uni = uni;
}

pub fn unixn(uni int) Time {
	// println('unix time($u)')
	// # int aa = atoi(u.str);
	// #printf("!!!! %d\n", aa);
	# time_t t = (time_t)uni;
	# struct tm * now = localtime(&t);
	// println('got tm')
	// TODO COPY PASTA
	res := Time{}
	# res.year = now->tm_year + 1900;
	# res.month = now->tm_mon + 1;
	# res.day = now->tm_mday;
	# res.hour = now->tm_hour;
	# res.minute = now->tm_min;
	# res.second = now->tm_sec;
	# res.uni = uni;
	// println('end unix')
	return res
}

fn (t Time) format_ss() string {
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

fn (t Time) hhmm_tmp() string {
	return '${t.hour:02d}:${t.minute:02d}'
}

// 21:04
pub fn (t Time) hhmm12() string {
	mut am := 'am'
	mut hour = t.hour
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
fn (t Time) hhmmss() string {
	return '${t.hour:02d}:${t.minute:02d}:${t.second:02d}'
}

// 2012-01-05
fn (t Time) ymmdd() string {
	return '${t.year}-${t.month:02d}-${t.day:02d}'
}

// Jul 3
fn (t Time) md() string {
	// jl := t.smonth()
	s := '${t.smonth()} $t.day'
	return s
}

fn (t Time) clean() string {
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

fn (t Time) clean12() string {
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

/* 
// in ms
fn ticks() double {
	# struct timeval  tv;
	# gettimeofday(&tv, NULL);
	# double time_in_mill =	  (tv.tv_sec) * 1000 + (tv.tv_usec) / 1000 ; // convert tv_sec & tv_usec to millisecond
	// # printf("!!!%f\n", time_in_mill);
	// # return (int)time_in_mill;
	// # return (int)(time_in_mill - 1521561736529);
	# return (long)(time_in_mill - 1523777913000);
	return double(0)
	// return int64(0)
}
*/
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
		year: ymd[0].to_i()
		month: ymd[1].to_i()
		day: ymd[2].to_i()
		hour: hour.to_i()
		minute: minute.to_i()
		second: second.to_i()
	})
}

fn new_time(t Time) Time {
	return{t | uni: t.calc_unix()}
}

fn (t &Time) calc_unix() int {
	# struct tm lDate;
	# lDate.tm_sec = t->second;
	# lDate.tm_min = t->minute;
	# lDate.tm_hour = t->hour;
	# lDate.tm_mday = t->day;
	# lDate.tm_mon = t->month-1;
	# lDate.tm_year = t->year - 1900;
	# time_t kek = mktime(&lDate);
	// # t->uni = (int)kek;
	# return (int)kek;
	return 0
}

// TODO add(d time.Duration)
pub fn (t Time) add_seconds(seconds int) Time {
	return unixn(t.uni + seconds)
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

fn day_of_week(y, m, d int) int {
	// TODO please no
	# return  (d += m < 3 ? y-- : y - 2, 23*m/9 + d + 4 + y/4- y/100 + y/400)%7;
	return 0
}

pub fn (t Time) day_of_week() int {
	return day_of_week(t.year, t.month, t.day)
}

// weekday_str() returns the current day in string (upto 3 characters)
pub fn (t Time) weekday_str() string {
	i := t.day_of_week() - 1
	return Days.substr(i * 3, (i + 1) * 3)
}
