// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

import strconv

const date_format_buffer = [u8(`0`), `0`, `0`, `0`, `-`, `0`, `0`, `-`, `0`, `0`]!
const time_format_buffer = [u8(`0`), `0`, `:`, `0`, `0`, `:`, `0`, `0`]!

fn validate_time_bounds(hour int, minute int, second int, nanosecond int) ! {
	if hour < 0 || hour > 23 {
		return error('invalid hour: ${hour}')
	}
	if minute < 0 || minute > 59 {
		return error('invalid minute: ${minute}')
	}
	if second < 0 || second > 59 {
		return error('invalid second: ${second}')
	}
	if nanosecond < 0 || nanosecond > 1_000_000_000 {
		return error('invalid nanosecond: ${nanosecond}')
	}
}

fn check_and_extract_time(s string) !(int, int, int, int) {
	mut hour_ := 0
	mut minute_ := 0
	mut second_ := 0
	mut nanosecond_ := 0

	// Check if the string start in the format "HH:MM:SS"
	for i := 0; i < time_format_buffer.len; i++ {
		if time_format_buffer[i] == u8(`0`) {
			if s[i] < u8(`0`) && s[i] > u8(`9`) {
				return error('`HH:MM:SS` match error: expected digit, not `${s[i]}` in position ${i}')
			} else {
				if i < 2 {
					hour_ = hour_ * 10 + (s[i] - u8(`0`))
				} else if i < 5 {
					minute_ = minute_ * 10 + (s[i] - u8(`0`))
				} else {
					second_ = second_ * 10 + (s[i] - u8(`0`))
				}
			}
		} else if time_format_buffer[i] != s[i] {
			return error('time separator error: expected `:`, not `${[s[i]].bytestr()}` in position ${i}')
		}
	}

	if s.len == time_format_buffer.len + 1 {
		if s[time_format_buffer.len] !in [u8(`Z`), `z`] {
			return error('timezone error: expected "Z" or "z" at the end of the string')
		}
		validate_time_bounds(hour_, minute_, second_, nanosecond_)!
		return hour_, minute_, second_, nanosecond_
	}

	if s.len < time_format_buffer.len + 1 {
		return error('datetime string is too short')
	}

	if s[time_format_buffer.len] == u8(`.`) {
		// Check if the string contains the nanoseconds part after the time part
		if s.len < time_format_buffer.len + 1 {
			return error('datetime string is too short')
		}
		// Check if the string start in the format ".NNNNNNNNN"
		mut nanosecond_digits := 0
		for i := time_format_buffer.len + 1; i < s.len; i++ {
			if s[i] < u8(`0`) || s[i] > u8(`9`) {
				if s[i] in [u8(`Z`), `z`] {
					if i != s.len - 1 {
						return error('timezone error: "Z" or "z" can only be at the end of the string')
					}
					break
				} else if s[i] in [u8(`+`), `-`] {
					break
				}
				return error('nanoseconds error: expected digit, not `${s[i]}` in position ${i}')
			}
			if !(i >= time_format_buffer.len + 1 + 9) {
				// nanoseconds limit is 9 digits
				nanosecond_ = nanosecond_ * 10 + (s[i] - u8(`0`))
				nanosecond_digits++
			}
		}
		if nanosecond_digits < 9 {
			for i := 0; i < 9 - nanosecond_digits; i++ {
				nanosecond_ *= 10
			}
		}
	}
	validate_time_bounds(hour_, minute_, second_, nanosecond_)!
	return hour_, minute_, second_, nanosecond_
}

fn check_and_extract_date(s string) !(int, int, int) {
	mut year := 0
	mut month := 0
	mut day := 0
	// Check if the string start in the format "YYYY-MM-DD"
	for i := 0; i < date_format_buffer.len; i++ {
		if date_format_buffer[i] == u8(`0`) {
			if s[i] < u8(`0`) && s[i] > u8(`9`) {
				return error('`YYYY-MM-DD` match error: expected digit, not `${s[i]}` in position ${i}')
			} else {
				if i < 4 {
					year = year * 10 + (s[i] - u8(`0`))
				} else if i < 7 {
					month = month * 10 + (s[i] - u8(`0`))
				} else {
					day = day * 10 + (s[i] - u8(`0`))
				}
			}
		} else if date_format_buffer[i] != s[i] {
			return error('date separator error:expected "${date_format_buffer[i]}", not `${s[i]}` in position ${i}')
		}
	}
	if month < 1 || month > 12 {
		return error('date error: invalid month ${month}')
	}
	if day < 1 || day > 31 {
		return error('date error: invalid day ${day}')
	}
	return year, month, day
}

// parse_rfc3339 returns the time from a date string in RFC 3339 datetime format.
// See also https://ijmacd.github.io/rfc3339-iso8601/ for a visual reference of
// the differences between ISO-8601 and RFC 3339.
pub fn parse_rfc3339(s string) !Time {
	if s == '' {
		return error_invalid_time(0, 'datetime string is empty')
	}

	if s.len < time_format_buffer.len {
		return error('string is too short to parse')
	}

	mut year, mut month, mut day := 0, 0, 0
	mut hour_, mut minute_, mut second_, mut nanosecond_ := 0, 0, 0, 0

	is_time := if s.len >= time_format_buffer.len {
		s[2] == u8(`:`) && s[5] == u8(`:`)
	} else {
		false
	}
	if is_time {
		return error('missing date part of RFC 3339')
	}

	is_date := if s.len >= date_format_buffer.len {
		s[4] == u8(`-`) && s[7] == u8(`-`)
	} else {
		false
	}

	if is_date {
		year, month, day = check_and_extract_date(s)!
		if s.len == date_format_buffer.len {
			return new(Time{
				year:     year
				month:    month
				day:      day
				is_local: false
			})
		}
	}

	is_datetime := if s.len >= date_format_buffer.len + 1 + time_format_buffer.len + 1 {
		is_date && s[10] == u8(`T`)
	} else {
		false
	}
	if is_datetime {
		// year, month, day := check_and_extract_date(s)!
		hour_, minute_, second_, nanosecond_ = check_and_extract_time(s[date_format_buffer.len + 1..])!
	}

	mut timezone_start_position := 0

	if is_datetime || is_time {
		timezone_start_position = date_format_buffer.len + 1 + time_format_buffer.len
		if s[timezone_start_position] == u8(`.`) {
			timezone_start_position++

			for s[timezone_start_position] !in [u8(`Z`), `z`, `+`, `-`] {
				timezone_start_position++
				if timezone_start_position == s.len {
					return error('timezone error: expected "Z" or "z" or "+" or "-" in position ${timezone_start_position}, not "${[
						s[timezone_start_position],
					].bytestr()}"')
				}
			}
		}
	}

	pos := date_format_buffer.len + time_format_buffer.len + 1
	if pos >= s.len {
		return error('timezone error: datetime string is too short')
	}
	if s[date_format_buffer.len + time_format_buffer.len + 1] !in [u8(`Z`), `z`, `+`, `-`, `.`] {
		// RFC 3339 needs a timezone
		return error('timezone error: expected "Z" or "z" or "+" or "-" in position ${
			date_format_buffer.len + time_format_buffer.len + 1}, not "${[
			s[date_format_buffer.len + time_format_buffer.len + 1],
		].bytestr()}"')
	} else {
		if s[s.len - 1] in [u8(`Z`), `z`] {
			return new(Time{
				year:       year
				month:      month
				day:        day
				hour:       hour_
				minute:     minute_
				second:     second_
				nanosecond: nanosecond_
				is_local:   false
			})
		} else {
			// Check if the string contains the timezone part after the time part +00:00
			if s.len < date_format_buffer.len + 1 + time_format_buffer.len + 6 {
				return error('datetime string is too short')
			}
			if s[s.len - 3] != u8(`:`) {
				return error('timezone separator error: expected ":", not `${[
					s[date_format_buffer.len + time_format_buffer.len + 3],
				].bytestr()}` in position ${date_format_buffer.len + time_format_buffer.len + 3}')
			}

			// Check if it is UTC time
			if unsafe { vmemcmp(s.str + s.len - 5, c'00:00', 5) == 0 } {
				return new(Time{
					year:       year
					month:      month
					day:        day
					hour:       hour_
					minute:     minute_
					second:     second_
					nanosecond: nanosecond_
					is_local:   false
				})
			}

			is_negative := s[s.len - 6] == u8(`-`)

			// To local time using the offset to add_seconds
			mut offset_in_minutes := 0
			mut offset_in_hours := 0
			// offset hours
			for i := 0; i < 2; i++ {
				offset_in_hours = offset_in_minutes * 10 + (s[s.len - 5 + i] - u8(`0`))
			}

			// offset minutes
			for i := 0; i < 2; i++ {
				offset_in_minutes = offset_in_minutes * 10 + (s[s.len - 2 + i] - u8(`0`))
			}

			offset_in_minutes += offset_in_hours * 60

			if !is_negative {
				offset_in_minutes *= -1
			}

			mut time_to_be_returned := new(Time{
				year:       year
				month:      month
				day:        day
				hour:       hour_
				minute:     minute_
				second:     second_
				nanosecond: nanosecond_
				is_local:   false
			})

			time_to_be_returned = time_to_be_returned.add_seconds(offset_in_minutes * 60)

			return time_to_be_returned
		}
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
	res := new(Time{
		year:   iyear
		month:  imonth
		day:    iday
		hour:   ihour
		minute: iminute
		second: isecond
	})
	return res
}

// parse_format parses the string `s`, as a custom `format`, containing the following specifiers:
//
// |Category| Format | Description |
// |:-----  | :----- | :---------- |
// |Year    | YYYY   | 4 digit year, 0000..9999 |
// |        | YY     | 2 digit year, 00..99 |
// |Month   | M      | month, 1..12 |
// |        | MM     | month, 2 digits, 01..12 |
// |        | MMM    | month, three letters, Jan..Dec |
// |        | MMMM   | name of month |
// |Day     | D      | day of the month, 1..31 |
// |        | DD     | day of the month, 01..31 |
// |        | d      | day of week, 0..6 |
// |        | c      | day of week, 1..7 |
// |        | dd     | day of week, Su..Sa |
// |        | ddd    | day of week, Sun..Sat |
// |        | dddd   | day of week, Sunday..Saturday |
// |Hour    | H      | hour, 0..23 |
// |        | HH     | hour, 00..23 |
// |        | h      | hour, 0..23 |
// |        | hh     | hour, 0..23 |
// |        | k      | hour, 0..23 |
// |        | kk     | hour, 0..23 |
// |Minute  | m      | minute, 0..59 |
// |        | mm     | minute, 0..59 |
// |Second  | s      | second, 0..59 |
// |        | ss     | second, 0..59 |
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
	mut t := new(
		year:       year
		month:      month
		day:        day
		hour:       hour_
		minute:     minute_
		second:     second_
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
	// eprintln('parse_iso8601_time s: $s | hour_: $hour_ | minute_: $minute_ | second_: $second_ | microsecond_: $microsecond_ | nanosecond_: $nanosecond_ | unix_offset: $unix_offset | is_local: $is_local_time')
	return hour_, minute_, second_, microsecond_, nanosecond_, unix_offset, is_local_time
}
