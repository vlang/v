module time

struct DateTime_Parser {
	datetime string
	format   string
mut:
	current_pos_datetime int
}

fn new_datetime_parser(datetime string, format string) DateTime_Parser {
	return DateTime_Parser{
		datetime: datetime
		format: format
	}
}

fn (mut p DateTime_Parser) next(length int) !string {
	if p.current_pos_datetime + length > p.datetime.len {
		return error('end of string')
	}
	val := p.datetime[p.current_pos_datetime..p.current_pos_datetime + length]
	p.current_pos_datetime += length
	return val
}

fn (mut p DateTime_Parser) peek(length int) !string {
	if p.current_pos_datetime + length > p.datetime.len {
		return error('end of string')
	}
	return p.datetime[p.current_pos_datetime - length..p.current_pos_datetime]
}

fn (mut p DateTime_Parser) must_be_int(length int) !int {
	val := p.next(length) or { return err }
	return val.int()
}

fn (mut p DateTime_Parser) must_be_single_int_with_optional_leading_zero() !int {
	mut val := p.next(1) or { return err }
	if val == '0' {
		val += p.next(1) or { return val.int() }
	}
	return val.int()
}

fn (mut p DateTime_Parser) must_be_string(must string) ! {
	val := p.next(must.len) or { return err }
	if val != must {
		return error('invalid string: "${val}"!="${must}"')
	}
}

fn (mut p DateTime_Parser) must_be_string_one_of(oneof []string) !string {
	for _, must in oneof {
		val := p.peek(must.len) or { continue }
		if val == must {
			return must
		}
	}
	return error('invalid string: must be one of ${oneof}, at ${p.current_pos_datetime}')
}

fn (mut p DateTime_Parser) must_be_valid_month() !int {
	for _, v in long_months {
		if p.current_pos_datetime + v.len < p.datetime.len {
			month_name := p.datetime[p.current_pos_datetime..p.current_pos_datetime + v.len]
			if v == month_name {
				p.current_pos_datetime += v.len
				return long_months.index(month_name) + 1
			}
		}
	}
	return error_invalid_time(0, 'invalid month name')
}

fn (mut p DateTime_Parser) must_be_valid_week_day(letters int) !string {
	val := p.next(letters) or { return err }
	for _, v in long_days {
		if v[0..letters] == val {
			return v
		}
	}
	return error_invalid_time(0, 'invalid month name')
}

fn extract_tokens(s string) ![]string {
	mut tokens := []string{}
	mut current := ''
	for r in s {
		if current.contains_only(r.ascii_str()) || current == '' {
			current += r.ascii_str()
		} else {
			tokens << current
			current = r.ascii_str()
		}
	}
	if current != '' {
		tokens << current
	}
	return tokens
}

fn (mut p DateTime_Parser) parse() !Time {
	mut year_ := 0
	mut month_ := 0
	mut day_in_month := 0
	mut hour_ := 0
	mut minute_ := 0
	mut second_ := 0
	tokens := extract_tokens(p.format) or {
		return error_invalid_time(0, 'malformed format string: ${err}')
	}
	for _, token in tokens {
		match token {
			'YYYY' {
				year_ = p.must_be_int(4) or {
					return error_invalid_time(0, 'end of string reached before the full year was specified')
				}
			}
			'YY' {
				year_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before the full year was specified')
				}
			}
			'M' {
				month_ = p.must_be_int(1) or {
					return error_invalid_time(0, 'end of string reached before the month was specified')
				}
			}
			'MM' {
				month_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before the month was specified')
				}
			}
			'MMMM' {
				month_ = p.must_be_valid_month() or { return err }
			}
			'D' {
				day_in_month = p.must_be_int(1) or {
					return error_invalid_time(0, 'end of string reached before the month was specified')
				}
			}
			'DD' {
				day_in_month = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before the month was specified')
				}
			}
			'H' {
				hour_ = p.must_be_int(1) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
			}
			'HH' {
				hour_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
			}
			'h' {
				hour_ = p.must_be_int(1) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
			}
			'hh' {
				hour_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
			}
			'k' {
				hour_ = p.must_be_int(1) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
			}
			'kk' {
				hour_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before hours where specified')
				}
			}
			'm' {
				minute_ = p.must_be_int(1) or {
					return error_invalid_time(0, 'end of string reached before minutes where specified')
				}
			}
			'mm' {
				minute_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before minutes where specified')
				}
			}
			's' {
				second_ = p.must_be_int(1) or {
					return error_invalid_time(0, 'end of string reached before seconds where specified')
				}
			}
			'ss' {
				second_ = p.must_be_int(2) or {
					return error_invalid_time(0, 'end of string reached before seconds where specified')
				}
			}
			else {
				p.must_be_string(token) or { return error_invalid_time(0, '${err}') }
			}
		}
	}

	return new_time(
		year: year_
		month: month_
		day: day_in_month
		hour: hour_
		minute: minute_
		second: second_
	)
}
