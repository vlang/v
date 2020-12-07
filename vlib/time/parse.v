// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

// parse returns time from a date string in "YYYY-MM-DD HH:MM:SS" format.
pub fn parse(s string) ?Time {
	pos := s.index(' ') or {
		return error('Invalid time format: $s')
	}
	symd := s[..pos]
	ymd := symd.split('-')
	if ymd.len != 3 {
		return error('Invalid time format: $s')
	}
	shms := s[pos..]
	hms := shms.split(':')
	hour := hms[0][1..]
	minute := hms[1]
	second := hms[2]
	res := new_time(Time{
		year: ymd[0].int()
		month: ymd[1].int()
		day: ymd[2].int()
		hour: hour.int()
		minute: minute.int()
		second: second.int()
	})
	return res
}

// parse_rfc2822 returns time from a date string in RFC 2822 datetime format.
pub fn parse_rfc2822(s string) ?Time {
	fields := s.split(' ')
	if fields.len < 5 {
		return error('Invalid time format: $s')
	}
	pos := months_string.index(fields[2]) or {
		return error('Invalid time format: $s')
	}
	mm := pos / 3 + 1
	mut tmstr := byteptr(0)
	unsafe {
		tmstr = malloc(s.len * 2)
	}
	count := unsafe {C.snprintf(charptr(tmstr), (s.len * 2), '%s-%02d-%s %s', fields[3].str,
		mm, fields[1].str, fields[4].str)}
	return parse(tos(tmstr, count))
}

// parse_iso8601 parses rfc8601 time format yyyy-MM-ddTHH:mm:ss.dddddd+dd:dd as local time
// the fraction part is difference in milli seconds and the last part is offset
// from UTC time and can be both +/- HH:mm
// remarks: not all iso8601 is supported
// also checks and support for leapseconds should be added in future PR
pub fn parse_iso8601(s string) ?Time {
	year := 0
	month := 0
	day := 0
	hour := 0
	minute := 0
	second := 0
	mic_second := 0
	time_char := `a`
	plus_min_z := `a`
	offset_hour := 0
	offset_min := 0
	mut count := unsafe {C.sscanf(charptr(s.str), '%4d-%2d-%2d%c%2d:%2d:%2d.%6d%c%2d:%2d',
		&year, &month, &day, charptr(&time_char), &hour, &minute, &second, &mic_second, charptr(&plus_min_z),
		&offset_hour, &offset_min)}
	// Missread microsec ([Year Month Day T Sec Hour Minute].len == 7 < 8)
	if count < 8 {
		count = unsafe {C.sscanf(charptr(s.str), '%4d-%2d-%2d%c%2d:%2d:%2d%c%2d:%2d',
			&year, &month, &day, charptr(&time_char), &hour, &minute, &second, charptr(&plus_min_z),
			&offset_hour, &offset_min)}
		count++ // Increment count because skipped microsec
	}
	is_local_time := plus_min_z == `a` && count == 8
	is_utc := plus_min_z == `Z` && count == 9
	if count != 11 && !is_local_time && !is_utc {
		return error('Invalid 8601 format')
	}
	if time_char != `T` && time_char != ` ` {
		return error('Invalid 8601 format, expected space or `T` as time separator')
	}
	if plus_min_z != `+` && plus_min_z != `-` && plus_min_z != `Z` && !is_local_time {
		return error('Invalid 8601 format, expected `Z` or `+` or `-` as time separator')
	}
	mut t := new_time(Time{
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		microsecond: mic_second
	})
	if is_local_time {
		return to_local_time(t)
	}
	mut unix_time := t.unix
	mut unix_offset := int(0)
	if offset_hour > 0 {
		unix_offset += 3600 * offset_hour
	}
	if offset_min > 0 {
		unix_offset += 60 * offset_min
	}
	if unix_offset != 0 {
		if plus_min_z == `+` {
			unix_time -= u64(unix_offset)
		} else {
			unix_time += u64(unix_offset)
		}
		t = unix2(int(unix_time), t.microsecond)
	}
	// Convert the time to local time
	return to_local_time(t)
}
