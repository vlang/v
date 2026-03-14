module time

struct DateTimeParser {
	datetime string
	format   string
mut:
	current_pos_datetime int
}

@[inline]
fn (p &DateTimeParser) matches_at(chars string) bool {
	end := p.current_pos_datetime + chars.len
	if end > p.datetime.len {
		return false
	}
	for i in 0 .. chars.len {
		if p.datetime[p.current_pos_datetime + i] != chars[i] {
			return false
		}
	}
	return true
}

fn (mut p DateTimeParser) next(length int) !string {
	if p.current_pos_datetime + length > p.datetime.len {
		return error('end of string')
	}
	val := p.datetime[p.current_pos_datetime..p.current_pos_datetime + length]
	p.current_pos_datetime += length
	return val
}

fn (mut p DateTimeParser) peek(length int) !string {
	if p.current_pos_datetime + length > p.datetime.len {
		return error('end of string')
	}
	return p.datetime[p.current_pos_datetime..p.current_pos_datetime + length]
}

fn (mut p DateTimeParser) must_be_int(length int) !int {
	end := p.current_pos_datetime + length
	if end > p.datetime.len {
		return error('end of string')
	}
	mut val := 0
	for i in p.current_pos_datetime .. end {
		ch := p.datetime[i]
		if ch < `0` || ch > `9` {
			return error('expected int, found: ${p.datetime[p.current_pos_datetime..end]}')
		}
		val = val * 10 + int(ch - `0`)
	}
	p.current_pos_datetime = end
	return val
}

fn (mut p DateTimeParser) must_be_int_with_minimum_length(min int, max int, allow_leading_zero bool) !int {
	max_len := max + 1 - min
	start := p.current_pos_datetime
	mut end := start
	for _ in 0 .. max_len {
		if end >= p.datetime.len {
			break
		}
		ch := p.datetime[end]
		if ch < `0` || ch > `9` {
			break
		}
		end++
	}
	if end - start < min {
		return error('expected int with a minimum length of ${min}, found: ${end - start}')
	}
	if !allow_leading_zero && p.datetime[start] == `0` {
		return error('0 is not allowed for this format')
	}
	mut val := 0
	for i in start .. end {
		val = val * 10 + int(p.datetime[i] - `0`)
	}
	p.current_pos_datetime = end
	return val
}

fn (mut p DateTimeParser) must_be_string(must string) ! {
	start := p.current_pos_datetime
	end := p.current_pos_datetime + must.len
	if end > p.datetime.len {
		return error('end of string')
	}
	if !p.matches_at(must) {
		p.current_pos_datetime = end
		return error('invalid string: "${p.datetime[start..end]}"!="${must}" at: ${p.current_pos_datetime}')
	}
	p.current_pos_datetime = end
}

fn (mut p DateTimeParser) must_be_string_one_of(oneof []string) !string {
	for must in oneof {
		val := p.peek(must.len) or { continue }
		if val == must {
			return must
		}
	}
	return error('invalid string: must be one of ${oneof}, at: ${p.current_pos_datetime}')
}

fn (mut p DateTimeParser) must_be_valid_month() !int {
	for v in long_months {
		if p.current_pos_datetime + v.len < p.datetime.len && p.matches_at(v) {
			p.current_pos_datetime += v.len
			return long_months.index(v) + 1
		}
	}
	return error_invalid_time(0, 'invalid month name, at: ${p.current_pos_datetime}')
}

fn (mut p DateTimeParser) must_be_valid_three_letter_month() !int {
	if p.current_pos_datetime + 3 < p.datetime.len {
		for m := 1; m <= long_months.len; m++ {
			token := months_string[(m - 1) * 3..m * 3]
			if p.matches_at(token) {
				p.current_pos_datetime += 3
				return m
			}
		}
	}
	return error_invalid_time(0, 'invalid three letter month, at: ${p.current_pos_datetime}')
}

fn (mut p DateTimeParser) must_be_valid_week_day() !string {
	for v in long_days {
		if p.current_pos_datetime + v.len < p.datetime.len && p.matches_at(v) {
			p.current_pos_datetime += v.len
			return v
		}
	}
	return error_invalid_time(0, 'invalid weekday, at: ${p.current_pos_datetime}')
}

fn (mut p DateTimeParser) must_be_valid_two_letter_week_day() !int {
	if p.current_pos_datetime + 2 < p.datetime.len {
		for d := 1; d <= long_days.len; d++ {
			token := days_string[(d - 1) * 3..d * 3 - 1]
			if p.matches_at(token) {
				p.current_pos_datetime += 2
				return d
			}
		}
	}
	return error_invalid_time(0, 'invalid two letter weekday, at: ${p.current_pos_datetime}')
}

fn (mut p DateTimeParser) must_be_valid_three_letter_week_day() !int {
	if p.current_pos_datetime + 3 < p.datetime.len {
		for d := 1; d <= long_days.len; d++ {
			token := days_string[(d - 1) * 3..d * 3]
			if p.matches_at(token) {
				p.current_pos_datetime += 3
				return d
			}
		}
	}
	return error_invalid_time(0, 'invalid three letter weekday, at: ${p.current_pos_datetime}')
}

fn extract_tokens(s string) ![]string {
	mut tokens := []string{}
	if s.len == 0 {
		return tokens
	}
	mut start := 0
	for i := 1; i < s.len; i++ {
		if s[i] != s[i - 1] {
			tokens << s[start..i]
			start = i
		}
	}
	tokens << s[start..s.len]
	return tokens
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
// d - day of week, 0..6
// c - day of week, 1..7
// dd - day of week, Su..Sa
// ddd - day of week, Sun..Sat
// dddd - day of week, Sunday..Saturday
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
fn (mut p DateTimeParser) parse() !Time {
	mut year_ := 0
	mut month_ := 0
	mut day_in_month := 0
	mut hour_ := 0
	mut minute_ := 0
	mut second_ := 0
	tokens := extract_tokens(p.format) or {
		return error_invalid_time(0, 'malformed format string: ${err}')
	}
	for token in tokens {
		match token {
			'YYYY' {
				year_ = p.must_be_int(4) or {
					return error_invalid_time(0, 'end of string reached before the full year was specified')
				}
			}
			'YY' {
				year_ = now().year / 100 * 100 + p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before the full year was specified')
				}
			}
			'M' {
				month_ = p.must_be_int_with_minimum_length(1, 2, false) or {
					return error_invalid_time(0, 'end of string reached before the month was specified')
				}
				if month_ < 1 || month_ > 12 {
					return error_invalid_time(0, 'month must be  between 1 and 12')
				}
			}
			'MM' {
				month_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before the month was specified')
				}
				if month_ < 1 || month_ > 12 {
					return error_invalid_time(0, 'month must be  between 01 and 12')
				}
			}
			'MMM' {
				month_ = p.must_be_valid_three_letter_month() or { return err }
			}
			'MMMM' {
				month_ = p.must_be_valid_month() or { return err }
			}
			'D' {
				day_in_month = p.must_be_int_with_minimum_length(1, 2, false) or {
					return error_invalid_time(0, 'end of string reached before the day was specified')
				}
				if day_in_month < 1 || day_in_month > 31 {
					return error_invalid_time(0, 'day must be  between 1 and 31')
				}
			}
			'DD' {
				day_in_month = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before the month was specified')
				}
				if day_in_month < 1 || day_in_month > 31 {
					return error_invalid_time(0, 'day must be  between 01 and 31')
				}
			}
			'd' {
				p.must_be_int(1) or { return err }
			}
			'c' {
				p.must_be_int(1) or { return err }
			}
			'dd' {
				p.must_be_valid_two_letter_week_day() or { return err }
			}
			'ddd' {
				p.must_be_valid_three_letter_week_day() or { return err }
			}
			'dddd' {
				p.must_be_valid_week_day() or { return err }
			}
			'H' {
				hour_ = p.must_be_int_with_minimum_length(1, 2, true) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
				if hour_ < 0 || hour_ > 23 {
					return error_invalid_time(0, 'hour must be  between 0 and 23')
				}
			}
			'HH' {
				hour_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
				if hour_ < 0 || hour_ > 23 {
					return error_invalid_time(0, 'hour must be  between 00 and 23')
				}
			}
			'h' {
				hour_ = p.must_be_int_with_minimum_length(1, 2, true) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
				if hour_ < 0 || hour_ > 23 {
					return error_invalid_time(0, 'hour must be  between 0 and 23')
				}
			}
			'hh' {
				hour_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
				if hour_ < 0 || hour_ > 23 {
					return error_invalid_time(0, 'hour must be  between 00 and 23')
				}
			}
			'k' {
				hour_ = p.must_be_int(1) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
				if hour_ < 0 || hour_ > 23 {
					return error_invalid_time(0, 'hour must be  between 0 and 23')
				}
			}
			'kk' {
				hour_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
				if hour_ < 0 || hour_ > 23 {
					return error_invalid_time(0, 'hour must be  between 00 and 23')
				}
			}
			'm' {
				minute_ = p.must_be_int(1) or {
					return error_invalid_time(0, 'end of string reached before minutes where specified')
				}
				if minute_ < 0 || minute_ > 59 {
					return error_invalid_time(0, 'minute must be between 0 and 59')
				}
			}
			'mm' {
				minute_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before minutes where specified')
				}
				if minute_ < 0 || minute_ > 59 {
					return error_invalid_time(0, 'minute must be between 00 and 59')
				}
			}
			's' {
				second_ = p.must_be_int(1) or {
					return error_invalid_time(0, 'end of string reached before seconds where specified')
				}
				if second_ < 0 || second_ > 59 {
					return error_invalid_time(0, 'second must be between 0 and 59')
				}
			}
			'ss' {
				second_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before seconds where specified')
				}
				if second_ < 0 || second_ > 59 {
					return error_invalid_time(0, 'second must be between 00 and 59')
				}
			}
			else {
				p.must_be_string(token) or { return error_invalid_time(0, '${err}') }
			}
		}
	}

	if month_ == 2 {
		feb_days_in_year := if is_leap_year(year_) { 29 } else { 28 }
		if day_in_month > feb_days_in_year {
			return error_invalid_time(0, 'February has only 28 days in the given year')
		}
	} else if day_in_month == 31 && month_ !in [1, 3, 5, 7, 8, 10, 12] {
		month_name := Time{
			month: month_
		}.custom_format('MMMM')
		return error_invalid_time(0, '${month_name} has only 30 days')
	}

	return new(
		year:   year_
		month:  month_
		day:    day_in_month
		hour:   hour_
		minute: minute_
		second: second_
	)
}
