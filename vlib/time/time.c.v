// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

import strings

#include <time.h>

// C.timeval represents a C time value.
pub struct C.timeval {
pub:
	tv_sec  u64
	tv_usec u64
}

type C.time_t = i64

fn C.time(t &C.time_t) C.time_t
fn C.localtime(t &C.time_t) &C.tm
fn C.localtime_r(t &C.time_t, tm &C.tm)
fn C.gmtime(t &C.time_t) &C.tm
fn C.gmtime_r(t &C.time_t, res &C.tm) &C.tm
fn C.strftime(buf &char, maxsize usize, const_format &char, const_tm &C.tm) usize

// now returns the current local time.
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
	return linux_now()
	/*
	// defaults to most common feature, the microsecond precision is not available
	// in this API call
	t := C.time(0)
	now := C.localtime(&t)
	return convert_ctime(*now, 0)
	*/
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
	return linux_utc()
}

fn time_with_unix(t Time) Time {
	if t.has_location() {
		return t
	}
	if t.unix != 0 {
		return t
	}
	normalized := normalize_zero_date_parts(t)
	return Time{
		...normalized
		unix: time_fields_to_unix(normalized)
	}
}

@[inline]
fn normalize_zero_date_parts(t Time) Time {
	if t.month != 0 && t.day != 0 {
		return t
	}
	return Time{
		...t
		month: if t.month == 0 { 1 } else { t.month }
		day:   if t.day == 0 { 1 } else { t.day }
	}
}

@[inline]
fn time_fields_to_unix(t Time) i64 {
	return i64(t.days_from_unix_epoch()) * i64(seconds_per_day) +
		i64(t.hour) * i64(seconds_per_hour) + i64(t.minute) * i64(seconds_per_minute) +
		i64(t.second)
}

// unix_now returns the current UNIX time in seconds (UTC), i.e. the value
// that `utc().unix()` would produce, without constructing a `Time` (no
// calendar conversion, no allocation). It is the cheapest wall-clock read
// available — a plain `time()` call, served from the vDSO on Linux — and is
// the natural choice for second-resolution caches, timeouts and TTL checks.
pub fn unix_now() i64 {
	return i64(C.time(0))
}

// ticks returns the number of milliseconds since the UNIX epoch.
// On Windows ticks returns the number of milliseconds elapsed since system start.
pub fn ticks() i64 {
	$if windows {
		return C.GetTickCount()
	} $else {
		ts := C.timeval{}
		C.gettimeofday(&ts, 0)
		return i64(ts.tv_sec * u64(1000) + (ts.tv_usec / u64(1_000)))
	}
	// t := i64(C.mach_absolute_time())
	// # Nanoseconds elapsedNano = AbsoluteToNanoseconds( *(AbsoluteTime *) &t );
	// # return (double)(* (uint64_t *) &elapsedNano) / 1000000;
}

// str returns the time in the same format as `parse` expects ("YYYY-MM-DD HH:mm:ss").
pub fn (t Time) str() string {
	// TODO: Define common default format for
	// `str` and `parse` and use it in both ways
	return t.format_ss()
}

// convert_ctime converts a C time to V time.
fn convert_ctime(t C.tm, nanosecond int) Time {
	return Time{
		year:       t.tm_year + 1900
		month:      t.tm_mon + 1
		day:        t.tm_mday
		hour:       t.tm_hour
		minute:     t.tm_min
		second:     t.tm_sec
		nanosecond: nanosecond
		unix:       make_unix_time(t)
		// for the actual code base when we
		// call convert_ctime, it is always
		// when we manage the local time.
		is_local: true
	}
}

// strftime returns the formatted time using `strftime(3)`.
pub fn (t Time) strftime(fmt string) string {
	mut strftime_fmt := fmt
	mut strftime_unix := t.unix
	if loc := t.location() {
		zone := loc.zone_at(t.unix) or { Zone{} }
		strftime_fmt = strftime_location_format(fmt, zone.name, strftime_zone_offset(zone.offset),
			t.unix.str())
		strftime_unix = t.local_unix()
	}
	mut tm := &C.tm{}
	$if windows {
		tm = C.gmtime(voidptr(&strftime_unix))
	} $else {
		C.gmtime_r(voidptr(&strftime_unix), tm)
	}
	mut buf := [1024]char{}
	fmt_c := unsafe { &char(strftime_fmt.str) }
	C.strftime(&buf[0], usize(sizeof(buf)), fmt_c, tm)
	return unsafe { cstring_to_vstring(&buf[0]) }
}

fn strftime_location_format(fmt string, zone_name string, zone_offset string, unix_time string) string {
	mut out := strings.new_builder(fmt.len)
	for i := 0; i < fmt.len; i++ {
		if fmt[i] != `%` || i + 1 >= fmt.len {
			out.write_u8(fmt[i])
			continue
		}
		directive_start := i
		i++
		mut no_padding := false
		mut padding := ` `
		mut uppercase := false
		mut alternate_case := false
		for i < fmt.len {
			match fmt[i] {
				`-` {
					no_padding = true
				}
				`_` {
					no_padding = false
					padding = ` `
				}
				`0` {
					no_padding = false
					padding = `0`
				}
				`^` {
					uppercase = true
				}
				`#` {
					alternate_case = true
				}
				else {
					break
				}
			}
			i++
		}
		mut width := 0
		for i < fmt.len && fmt[i] >= `0` && fmt[i] <= `9` {
			width = width * 10 + int(fmt[i] - `0`)
			i++
		}
		if i < fmt.len && (fmt[i] == `E` || fmt[i] == `O`) {
			i++
		}
		if i >= fmt.len {
			out.write_string(fmt[directive_start..])
			break
		}
		value := match fmt[i] {
			`Z` {
				zone_name
			}
			`z` {
				zone_offset
			}
			`s` {
				unix_time
			}
			else {
				out.write_string(fmt[directive_start..i + 1])
				continue
			}
		}
		formatted := strftime_location_value(value, width, no_padding, padding, uppercase,
			alternate_case)
		// The value is inserted into the libc format as a literal.
		out.write_string(formatted.replace('%', '%%'))
	}
	return out.str()
}

fn strftime_location_value(value string, width int, no_padding bool, padding u8, uppercase bool, alternate_case bool) string {
	mut formatted := value
	if uppercase {
		formatted = formatted.to_upper()
	}
	if alternate_case {
		formatted = formatted.to_lower()
	}
	if no_padding || formatted.len >= width {
		return formatted
	}
	pad := if padding == `0` { '0' } else { ' ' }
	return pad.repeat(width - formatted.len) + formatted
}

fn strftime_zone_offset(offset int) string {
	sign := if offset < 0 { '-' } else { '+' }
	abs_offset := if offset < 0 { -offset } else { offset }
	hours := abs_offset / seconds_per_hour
	minutes := (abs_offset % seconds_per_hour) / seconds_per_minute
	return '${sign}${hours:02}${minutes:02}'
}

// some *nix system functions (e.g. `C.poll()`, C.epoll_wait()) accept an `int`
// value as *timeout in milliseconds* with the special value `-1` meaning "infinite"
pub fn (d Duration) sys_milliseconds() int {
	if d > 2147483647 * millisecond { // treat 2147483647000001 .. C.INT64_MAX as "infinite"
		return -1
	} else if d <= 0 {
		return 0 // treat negative timeouts as 0 - consistent with Unix behaviour
	} else {
		return int(d / millisecond)
	}
}
