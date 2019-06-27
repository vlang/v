// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module time

#include <time.h>

struct Time {
	sec i64
	nsec i32
	mono i64
}

fn (a Time) + (b Time) Time {
	sec := a.sec + b.sec
	nsec := a.nsec + b.nsec
	return Time{
		sec: sec + i64(nsec)/i64(1000000000)
		nsec: nsec % i32(1000000000)
	}
}

fn (a Time) - (b Time) Time {
	sec := a.sec - b.sec - i64(1)
	nsec := a.nsec - b.nsec + i32(1000000000)
	t := Time{
		sec: sec + i64(nsec)/i64(1000000000)
		nsec: nsec % i32(1000000000)
	}
	if a.mono <= i64(0) || b.mono <= i64(0) {
		return t
	}

	mono := a.mono - b.mono
	if mono > i64(0) && t.sec >= i64(0) && t.nsec >= i32(0) {
		return t
	}
	if mono < i64(0) && t.sec <= i64(0) && t.nsec <= i32(0) {
		return t
	}
	if mono == i64(0) {
		return Time{}
	}
	return Time{
		sec: mono / i64(1000000000)
		nsec: i32(mono%i64(1000000000))
	}
}

fn (t Time) local() Info {
	info := Info{}
	# struct tm *x = localtime(&t.sec);
	# info.year = x->tm_year + 1900;
	# info.month = x->tm_mon + 1;
	# info.day = x->tm_mday;
	# info.hour = x->tm_hour;
	# info.minute = x->tm_min;
	# info.second = x->tm_sec;
	# info.yday = x->tm_yday;
	# info.wday = x->tm_wday;
	return info
}

fn (t Time) utc() Info {
	info := Info{}
	# struct tm *x = gmtime(&t.sec);
	# info.year = x->tm_year + 1900;
	# info.month = x->tm_mon + 1;
	# info.day = x->tm_mday;
	# info.hour = x->tm_hour;
	# info.minute = x->tm_min;
	# info.second = x->tm_sec;
	# info.yday = x->tm_yday;
	# info.wday = x->tm_wday;
	return info
}

fn (t Time) days() f64 {
	return f64(t.sec)/86400.0
}

fn (t Time) hours() f64 {
	return f64(t.sec)/3600.0
}

fn (t Time) minutes() f64 {
	return f64(t.sec)/60.0
}

fn (t Time) seconds() f64 {
	return f64(t.sec) + f64(t.nsec)/1000000000.0
}

fn (t Time) milliseconds() f64 {
	return 1000.0*f64(t.sec) + f64(t.nsec)/1000000.0
}

fn (t Time) microseconds() f64 {
	return 1000000.0*f64(t.sec) + f64(t.nsec)/1000.0
}

fn (t Time) nanoseconds() f64 {
	return 1000000000.0*f64(t.sec) + f64(t.nsec)
}


fn (t Time) format(fmt string) string {
	res := ''
	cfmt := fmt.cstr()
	# char buf[1024];
	# struct tm *x = localtime(&t.sec);
	# strftime(buf, 1024, cfmt, x);
	# res = tos2(buf);
	return res
}

fn (t Time) str() string {
	if t.sec == i64(0) {
		if t.nsec == i32(0) {
			return '0s'
		}
		if t.nsec < i32(1000) && t.nsec > i32(-1000) {
			return '${t.nsec}ns'
		}
		if t.nsec < i32(1000000) && t.nsec > i32(-1000000) {
			return '${f64(t.nsec)/1000.0:.1f}µs'
		}
		if t.nsec < i32(1000000000) && t.nsec > i32(-1000000000) {
			return '${f64(t.nsec)/1000000.0:.1f}ms'
		}
	}
	if t.sec < i64(60) && t.sec > i64(-60) {
		return '${f64(t.sec)+f64(t.nsec)/1000000000.0:.1f}s'
	}
	if t.sec < i64(3600) && t.sec > i64(-3600) {
		return '${f64(t.sec)/60.0:.1f}m'
	}
	if t.sec < i64(86400) && t.sec > i64(-86400) {
		return '${f64(t.sec)/3600.0:.1f}h'
	}
	if t.sec < i64(864000) && t.sec > i64(-864000) {
		return '${f64(t.sec)/86400.0:.1f}d'
	}
	return '${f64(t.sec)/86400.0:.0f}d'
}
