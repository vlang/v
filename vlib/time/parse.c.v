// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

import strconv

// parse_rfc3339 returns the time from a date string in RFC 3339 datetime format.
// See also https://ijmacd.github.io/rfc3339-iso8601/ for a visual reference of
// the differences between ISO-8601 and RFC 3339.
pub fn parse_rfc3339(s string) !Time {
	if s == '' {
		return error_invalid_time(0, 'datetime string is empty')
	}
	// Normalize the input before parsing. Good since iso8601 doesn't permit lower case `t` and `z`.
	sn := s.replace_each(['t', 'T', 'z', 'Z'])
	mut t := parse_iso8601(sn) or { Time{} }
	// If parse_iso8601 DID NOT result in default values (i.e. date was parsed correctly)
	if t != Time{} {
		return t
	}

	t_i := sn.index('T') or { -1 }
	parts := if t_i != -1 { [sn[..t_i], sn[t_i + 1..]] } else { sn.split(' ') }

	// Check if sn is date only
	if !parts[0].contains_any(' Z') && parts[0].contains('-') {
		year, month, day := parse_iso8601_date(sn)!
		t = new_time(Time{
			year: year
			month: month
			day: day
		})
		return t
	}
	// Check if sn is time only
	if !parts[0].contains('-') && parts[0].contains(':') {
		mut hour_, mut minute_, mut second_, mut microsecond_, mut nanosecond_, mut unix_offset, mut is_local_time := 0, 0, 0, 0, 0, i64(0), true
		hour_, minute_, second_, microsecond_, nanosecond_, unix_offset, is_local_time = parse_iso8601_time(parts[0])!
		t = new_time(Time{
			hour: hour_
			minute: minute_
			second: second_
			nanosecond: nanosecond_
		})
		if is_local_time {
			return t // Time is already local time
		}
		mut unix_time := t.unix
		if unix_offset < 0 {
			unix_time -= (-unix_offset)
		} else if unix_offset > 0 {
			unix_time += unix_offset
		}
		t = unix_nanosecond(i64(unix_time), t.nanosecond)
		return t
	}

	return error_invalid_time(9, 'malformed date')
}

// parse returns the time from a date string in "YYYY-MM-DD HH:mm:ss" format.
pub fn parse(s string) !Time {
	if s == '' {
		return error_invalid_time(0, 'datetime string is empty')
	}
	pos := s.index(' ') or {
		return error_invalid_time(1, 'string has no space between date and time')
	}
	symd := s[..pos]
	ymd := symd.split('-')
	if ymd.len != 3 {
		return error_invalid_time(2, 'date must be in the form of y-m-d')
	}
	shms := s[pos..]
	hms := shms.split(':')
	if hms.len != 3 {
		return error_invalid_time(9, 'time must be in the form of H:i:s')
	}
	hour_ := hms[0][1..]
	minute_ := hms[1]
	second_ := hms[2]
	//
	iyear := strconv.atoi(ymd[0]) or {
		return error_invalid_time(0, 'invalid year format: ${ymd[0]}')
	}
	imonth := strconv.atoi(ymd[1]) or {
		return error_invalid_time(0, 'invalid month format: ${ymd[1]}')
	}
	iday := strconv.atoi(ymd[2]) or {
		return error_invalid_time(0, 'invalid day format: ${ymd[2]}')
	}
	ihour := strconv.atoi(hour_) or {
		return error_invalid_time(0, 'invalid hour format: ${hour_}')
	}
	iminute := strconv.atoi(minute_) or {
		return error_invalid_time(0, 'invalid minute format: ${minute_}')
	}
	isecond := strconv.atoi(second_) or {
		return error_invalid_time(0, 'invalid second format: ${second_}')
	}

	// eprintln('>> iyear: $iyear | imonth: $imonth | iday: $iday | ihour: $ihour | iminute: $iminute | isecond: $isecond')
	if iyear > 9999 || iyear < -9999 {
		return error_invalid_time(3, 'year must be between -10000 and 10000')
	}
	if imonth > 12 || imonth < 1 {
		return error_invalid_time(4, 'month must be between 1 and 12')
	}
	if iday > 31 || iday < 1 {
		return error_invalid_time(5, 'day must be between 1 and 31')
	}
	if ihour > 23 || ihour < 0 {
		return error_invalid_time(6, 'hours must be between 0 and 24')
	}
	if iminute > 59 || iminute < 0 {
		return error_invalid_time(7, 'minutes must be between 0 and 60')
	}
	if isecond > 59 || isecond < 0 {
		return error_invalid_time(8, 'seconds must be between 0 and 60')
	}
	res := new_time(Time{
		year: iyear
		month: imonth
		day: iday
		hour: ihour
		minute: iminute
		second: isecond
	})
	return res
}

// parse_format parses the string `s`, as a custom `format`, containing the following specifiers:
// YYYY - 4 digit year, 0000..9999
// YY - 2 digit year, 00..99
// M - month, 1..12
// MM - month, 2 digits, 01..12
// MMM - month, three letters, Jan..Dec
// MMMM - name of month
// D - day of the month, 1..31
// DD - day of the month, 01..31
// H - hour, 0..23
// HH - hour, 00..23
// h - hour, 0..23
// hh - hour, 0..23
// k - hour, 0..23
// kk - hour, 0..23
// m - minute, 0..59
// mm - minute, 0..59
// s - second, 0..59
// ss - second, 0..59
pub fn parse_format(s string, format string) !Time {
	if s == '' {
		return error_invalid_time(0, 'datetime string is empty')
	}
	mut p := new_date_time_parser(s, format)
	return p.parse()
}

// parse_iso8601 parses the ISO 8601 time format yyyy-MM-ddTHH:mm:ss.dddddd+dd:dd as local time.
// The fraction part is difference in milli seconds, and the last part is offset from UTC time.
// Both can be +/- HH:mm .
// See https://en.wikipedia.org/wiki/ISO_8601 .
// Remarks: not all of ISO 8601 is supported; checks and support for leapseconds should be added.
pub fn parse_iso8601(s string) !Time {
	if s == '' {
		return error_invalid_time(0, 'datetime string is empty')
	}
	t_i := s.index('T') or { -1 }
	parts := if t_i != -1 { [s[..t_i], s[t_i + 1..]] } else { s.split(' ') }
	if !(parts.len == 1 || parts.len == 2) {
		return error_invalid_time(12, 'malformed date')
	}
	year, month, day := parse_iso8601_date(parts[0])!
	mut hour_, mut minute_, mut second_, mut microsecond_, mut nanosecond_, mut unix_offset, mut is_local_time := 0, 0, 0, 0, 0, i64(0), true
	if parts.len == 2 {
		hour_, minute_, second_, microsecond_, nanosecond_, unix_offset, is_local_time = parse_iso8601_time(parts[1])!
	}
	mut t := new_time(
		year: year
		month: month
		day: day
		hour: hour_
		minute: minute_
		second: second_
		nanosecond: nanosecond_
	)
	if is_local_time {
		return t // Time already local time
	}
	mut unix_time := t.unix
	if unix_offset < 0 {
		unix_time -= (-unix_offset)
	} else if unix_offset > 0 {
		unix_time += unix_offset
	}
	t = unix_nanosecond(i64(unix_time), t.nanosecond)
	return t
}

// parse_rfc2822 returns the time from a date string in RFC 2822 datetime format.
pub fn parse_rfc2822(s string) !Time {
	if s == '' {
		return error_invalid_time(0, 'datetime string is empty')
	}
	fields := s.split(' ')
	if fields.len < 5 {
		return error_invalid_time(1, 'datetime string must have 5 components, has: ${fields.len}')
	}
	pos := months_string.index(fields[2]) or {
		return error_invalid_time(2, 'invalid month format')
	}
	mm := pos / 3 + 1
	unsafe {
		tmstr := malloc_noscan(s.len * 2)
		count := C.snprintf(&char(tmstr), (s.len * 2), c'%s-%02d-%s %s', fields[3].str,
			mm, fields[1].str, fields[4].str)
		return parse(tos(tmstr, count))
	}
}

// ----- iso8601 -----
fn parse_iso8601_date(s string) !(int, int, int) {
	year, month, day, dummy := 0, 0, 0, u8(0)
	count := unsafe { C.sscanf(&char(s.str), c'%4d-%2d-%2d%c', &year, &month, &day, &dummy) }
	if count != 3 {
		return error_invalid_time(10, 'datetime string must have 3 components, but has ${count}')
	}
	if year > 9999 {
		return error_invalid_time(13, 'year must be smaller than 10000')
	}
	if month > 12 {
		return error_invalid_time(14, 'month must be smaller than 12')
	}
	if day > 31 {
		return error_invalid_time(15, 'day must be smaller than 31')
	}
	return year, month, day
}

fn parse_iso8601_time(s string) !(int, int, int, int, int, i64, bool) {
	hour_ := 0
	minute_ := 0
	second_ := 0
	mut microsecond_ := 0
	mut nanosecond_ := 0
	plus_min_z := `a`
	offset_hour := 0
	offset_minute := 0
	mut count := 0
	count = unsafe {
		C.sscanf(&char(s.str), c'%2d:%2d:%2d.%9d%c', &hour_, &minute_, &second_, &nanosecond_,
			&char(&plus_min_z))
	}
	if count == 5 && plus_min_z == `Z` {
		// normalise the nanoseconds:
		mut ndigits := 0
		if mut pos := s.index('.') {
			pos++
			for ; pos < s.len && s[pos].is_digit(); pos++ {
				ndigits++
			}
		}
		for ndigits < 9 {
			nanosecond_ *= 10
			ndigits++
		}
		microsecond_ = nanosecond_ / 1000
	} else {
		count = unsafe {
			C.sscanf(&char(s.str), c'%2d:%2d:%2d.%9d%c%2d:%2d', &hour_, &minute_, &second_,
				&microsecond_, &char(&plus_min_z), &offset_hour, &offset_minute)
		}
		// Missread microsecond ([Sec Hour Minute].len == 3 < 4)
		if count < 4 {
			count = unsafe {
				C.sscanf(&char(s.str), c'%2d:%2d:%2d%c%2d:%2d', &hour_, &minute_, &second_,
					&char(&plus_min_z), &offset_hour, &offset_minute)
			}
			count++ // Increment count because skipped microsecond
		}
		if count < 4 {
			return error_invalid_time(10, 'malformed date')
		}
		nanosecond_ = microsecond_ * 1000
	}
	is_local_time := plus_min_z == `a` && count == 4
	is_utc := plus_min_z == `Z` && count == 5
	if !(count == 7 || is_local_time || is_utc) {
		return error_invalid_time(11, 'malformed date')
	}
	if plus_min_z != `+` && plus_min_z != `-` && !is_utc && !is_local_time {
		return error_invalid_time(12, 'missing timezone')
	}
	mut unix_offset := 0
	if offset_hour > 0 {
		unix_offset += 3600 * offset_hour
	}
	if offset_minute > 0 {
		unix_offset += 60 * offset_minute
	}
	if plus_min_z == `+` {
		unix_offset *= -1
	}
	// eprintln('parse_iso8601_time s: $s | hour_: $hour_ | minute_: $minute_ | second_: $second_ | microsecond_: $microsecond_ | nanosecond_: $nanosecond_ | unix_offset: $unix_offset | is_local_time: $is_local_time')
	return hour_, minute_, second_, microsecond_, nanosecond_, unix_offset, is_local_time
}
