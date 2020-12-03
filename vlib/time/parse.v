// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

import regex

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

fn re_get_str_group(re regex.RE, input string, name string) ?string {
	i, j := re.get_group(name)
	if i >= 0 && j > i {
		return input[i..j]
	}
	return none
}

fn re_get_int_group(re regex.RE, input string, name string) ?int {
	s := re_get_str_group(re, input, name) or {
		return none
	}
	return s.int()
}

fn new_iso8601_query() string {
	date_query := r'(?P<year>\d{4})-(?P<month>\d{2})-(?P<day>\d{2})'
	time_query := r'(?P<hour>\d{2}):(?P<minute>\d{2}):(?P<second>\d{2})\.(?P<microsecond>\d{6})'
	offset_query := r'(?P<plusminus>\+|-)(?P<offsethour>\d{2}):(?P<offsetmin>\d{2})'
	z_query := r'(?P<z>Z)'
	return '^${date_query}T${time_query}($z_query|$offset_query)?'
}

const (
	err_invalid_8601 = error('Invalid 8601 format')
	iso8601_query    = new_iso8601_query()
)

// parse_iso8601 parses rfc8601 time format yyyy-MM-ddTHH:mm:ss.dddddd+dd:dd
// the fraction part is difference in milli seconds and the last part is offset
// from UTC time and can be both +/- HH:mm
// remarks: not all iso8601 is supported
// also checks and support for leapseconds should be added in future PR
pub fn parse_iso8601(s string) ?Time {
	mut re := regex.new()
	re.group_max = 10
	re.compile_opt(iso8601_query) or {
		println(err)
	}
	re.match_string(s)
	year := re_get_int_group(re, s, 'year') or {
		return err_invalid_8601
	}
	month := re_get_int_group(re, s, 'month') or {
		return err_invalid_8601
	}
	day := re_get_int_group(re, s, 'day') or {
		return err_invalid_8601
	}
	hour := re_get_int_group(re, s, 'hour') or {
		return err_invalid_8601
	}
	minute := re_get_int_group(re, s, 'minute') or {
		return err_invalid_8601
	}
	second := re_get_int_group(re, s, 'second') or {
		return err_invalid_8601
	}
	microsecond := re_get_int_group(re, s, 'microsecond') or {
		return err_invalid_8601
	}
	z_i, z_j := re.get_group('z')
	is_utc := z_i >= 0 && z_j > z_i
	plus_minus := re_get_str_group(re, s, 'plusminus') or {
		''
	}
	is_local_time := !is_utc && plus_minus.len == 0
	offset_hour := re_get_int_group(re, s, 'offsethour') or {
		0
	}
	offset_min := re_get_int_group(re, s, 'offsetmin') or {
		0
	}
	mut t := new_time(Time{
		year: year
		month: month
		day: day
		hour: hour
		minute: minute
		second: second
		microsecond: microsecond
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
		if plus_minus == '+' {
			unix_time -= u64(unix_offset)
		} else {
			unix_time += u64(unix_offset)
		}
		t = unix2(int(unix_time), t.microsecond)
	}
	return t
}
