// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

// parse_rfc2822 returns time from a date string in RFC 2822 datetime format.
pub fn parse_rfc2822(s string) ?Time {
	if s == '' {
		return error_invalid_time(0)
	}
	fields := s.split(' ')
	if fields.len < 5 {
		return error_invalid_time(1)
	}
	pos := months_string.index(fields[2]) or { return error_invalid_time(2) }
	mm := pos / 3 + 1
	unsafe {
		tmstr := malloc_noscan(s.len * 2)
		count := C.snprintf(&char(tmstr), (s.len * 2), c'%s-%02d-%s %s', fields[3].str,
			mm, fields[1].str, fields[4].str)
		return parse(tos(tmstr, count))
	}
}

// ----- iso8601 -----
fn parse_iso8601_date(s string) ?(int, int, int) {
	year, month, day, dummy := 0, 0, 0, byte(0)
	count := unsafe { C.sscanf(&char(s.str), c'%4d-%2d-%2d%c', &year, &month, &day, &dummy) }
	if count != 3 {
		return error_invalid_time(10)
	}
	return year, month, day
}

fn parse_iso8601_time(s string) ?(int, int, int, int, i64, bool) {
	hour_ := 0
	minute_ := 0
	second_ := 0
	microsecond_ := 0
	plus_min_z := `a`
	offset_hour := 0
	offset_minute := 0
	mut count := unsafe {
		C.sscanf(&char(s.str), c'%2d:%2d:%2d.%6d%c%2d:%2d', &hour_, &minute_, &second_,
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
		return error_invalid_time(10)
	}
	is_local_time := plus_min_z == `a` && count == 4
	is_utc := plus_min_z == `Z` && count == 5
	if !(count == 7 || is_local_time || is_utc) {
		return error_invalid_time(11)
	}
	if plus_min_z != `+` && plus_min_z != `-` && !is_utc && !is_local_time {
		return error_invalid_time(12)
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
	return hour_, minute_, second_, microsecond_, unix_offset, is_local_time
}
