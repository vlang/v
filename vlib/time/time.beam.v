// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// BEAM backend time functions
// These functions are translated by the BEAM codegen to Erlang runtime calls
module time

import strconv

// now returns the current local time.
// Codegen translates to: calendar:local_time() + erlang:system_time(nanosecond)
pub fn now() Time {
	// BEAM codegen handles this - translates to Erlang calendar/time functions
	return Time{
		is_local: true
	}
}

// utc returns the current UTC time.
// Codegen translates to: calendar:universal_time() + erlang:system_time(nanosecond)
pub fn utc() Time {
	// BEAM codegen handles this
	return Time{}
}

// local returns t with the location set to local time.
// Codegen translates to: calendar:universal_time_to_local_time()
pub fn (t Time) local() Time {
	if t.is_local {
		return t
	}
	// BEAM codegen handles conversion
	return Time{
		...t
		is_local: true
	}
}

// sleep suspends the execution of the calling thread for a given duration.
// Codegen translates to: timer:sleep(Milliseconds)
pub fn sleep(duration Duration) {
	// BEAM codegen handles this - translates to timer:sleep
}

// ticks returns the number of milliseconds since the UNIX epoch.
// Codegen translates to: erlang:system_time(millisecond)
pub fn ticks() i64 {
	// BEAM codegen handles this
	return 0
}

// sys_mono_now returns a monotonically increasing time in nanoseconds.
// Used by StopWatch for elapsed time measurement.
// Codegen translates to: erlang:monotonic_time(nanosecond)
pub fn sys_mono_now() u64 {
	// BEAM codegen handles this
	return 0
}

// time_with_unix ensures the Time struct has a valid unix timestamp.
// This is a helper used by other time functions.
fn time_with_unix(t Time) Time {
	if t.unix != 0 {
		return t
	}
	// Calculate unix timestamp from components
	// This uses the same algorithm as the shared time.v code
	// BEAM codegen handles the actual calendar calculation
	return Time{
		...t
		unix: calculate_unix_time(t.year, t.month, t.day, t.hour, t.minute, t.second)
	}
}

// calculate_unix_time computes the Unix timestamp from date/time components.
// This is a pure V implementation that doesn't depend on C functions.
fn calculate_unix_time(year int, month int, day int, hour int, minute int, second int) i64 {
	// Days from year 1 to 1970
	mut y := year
	mut m := month
	if m <= 2 {
		y -= 1
		m += 12
	}
	// Calculate days since epoch
	days := 365 * (y - 1970) + (y - 1969) / 4 - (y - 1901) / 100 + (y - 1601) / 400
	// Add days for months (cumulative days before each month)
	days_before_month := [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334]
	mut day_of_year := days_before_month[month - 1] + day
	if month > 2 && is_leap_year(year) {
		day_of_year += 1
	}
	total_days := days + day_of_year - 1
	return i64(total_days) * 86400 + i64(hour) * 3600 + i64(minute) * 60 + i64(second)
}

// strftime formats the Time according to the format string.
// Pure V implementation of strftime-style formatting.
pub fn (t Time) strftime(fmt string) string {
	if fmt == '' {
		return ''
	}
	mut result := ''
	mut i := 0
	for i < fmt.len {
		if fmt[i] == `%` && i + 1 < fmt.len {
			i++
			match fmt[i] {
				`Y` {
					// 4-digit year
					mut ys := '${t.year}'
					for ys.len < 4 {
						ys = '0' + ys
					}
					result += ys
				}
				`m` {
					// 2-digit month
					if t.month < 10 {
						result += '0${t.month}'
					} else {
						result += '${t.month}'
					}
				}
				`d` {
					// 2-digit day
					if t.day < 10 {
						result += '0${t.day}'
					} else {
						result += '${t.day}'
					}
				}
				`H` {
					// 2-digit hour (24h)
					if t.hour < 10 {
						result += '0${t.hour}'
					} else {
						result += '${t.hour}'
					}
				}
				`M` {
					// 2-digit minute
					if t.minute < 10 {
						result += '0${t.minute}'
					} else {
						result += '${t.minute}'
					}
				}
				`S` {
					// 2-digit second
					if t.second < 10 {
						result += '0${t.second}'
					} else {
						result += '${t.second}'
					}
				}
				`I` {
					// 12-hour clock hour
					mut h := t.hour % 12
					if h == 0 {
						h = 12
					}
					if h < 10 {
						result += '0${h}'
					} else {
						result += '${h}'
					}
				}
				`p` {
					// AM/PM
					if t.hour < 12 {
						result += 'AM'
					} else {
						result += 'PM'
					}
				}
				`b` {
					// Abbreviated month name
					result += t.smonth()
				}
				`j` {
					// Day of year (001-366)
					days_before := [0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304,
						334]
					mut doy := days_before[t.month - 1] + t.day
					if t.month > 2 && is_leap_year(t.year) {
						doy += 1
					}
					if doy < 10 {
						result += '00${doy}'
					} else if doy < 100 {
						result += '0${doy}'
					} else {
						result += '${doy}'
					}
				}
				`n` {
					result += '\n'
				}
				`t` {
					result += '\t'
				}
				`%` {
					result += '%'
				}
				`e` {
					// Day of month, space-padded
					if t.day < 10 {
						result += ' ${t.day}'
					} else {
						result += '${t.day}'
					}
				}
				`F` {
					// ISO 8601 date: YYYY-MM-DD
					result += t.strftime('%Y-%m-%d')
				}
				`T` {
					// ISO 8601 time: HH:MM:SS
					result += t.strftime('%H:%M:%S')
				}
				`R` {
					// HH:MM
					result += t.strftime('%H:%M')
				}
				else {
					result += '%'
					result += fmt[i].ascii_str()
				}
			}
		} else {
			result += fmt[i].ascii_str()
		}
		i++
	}
	return result
}

// parse_iso8601 parses an ISO 8601 formatted string into a Time.
// Supports formats: "YYYY-MM-DD", "YYYY-MM-DDTHH:MM:SS", "YYYY-MM-DDTHH:MM:SSZ",
// "YYYY-MM-DDTHH:MM:SS+HH:MM", "YYYY-MM-DDTHH:MM:SS.frac"
pub fn parse_iso8601(s string) !Time {
	if s == '' {
		return error('datetime string is empty')
	}
	// Remove trailing 'Z' timezone indicator
	mut str := s
	if str.len > 0 && (str[str.len - 1] == `Z` || str[str.len - 1] == `z`) {
		str = str[..str.len - 1]
	}
	// Strip timezone offset (+HH:MM or -HH:MM) at end
	if str.len > 6 {
		last6 := str[str.len - 6..]
		if (last6[0] == `+` || last6[0] == `-`) && last6[3] == `:` {
			str = str[..str.len - 6]
		}
	}
	// Split date and time by T or t
	mut date_part := str
	mut time_part := ''
	for ci in 0 .. str.len {
		if str[ci] == `T` || str[ci] == `t` {
			date_part = str[..ci]
			time_part = str[ci + 1..]
			break
		}
	}
	// Parse date
	date_parts := date_part.split('-')
	if date_parts.len != 3 {
		return error('invalid ISO 8601 date format')
	}
	iyear := strconv.atoi(date_parts[0]) or { return error('invalid year') }
	imonth := strconv.atoi(date_parts[1]) or { return error('invalid month') }
	iday := strconv.atoi(date_parts[2]) or { return error('invalid day') }
	// Parse time if present
	mut ihour := 0
	mut iminute := 0
	mut isecond := 0
	mut ins := 0
	if time_part != '' {
		// Strip fractional seconds
		mut tp := time_part
		for fi in 0 .. tp.len {
			if tp[fi] == `.` {
				frac := tp[fi + 1..]
				tp = tp[..fi]
				// Parse fractional part into nanoseconds
				mut frac_val := strconv.atoi(frac) or { 0 }
				mut frac_len := frac.len
				for frac_len < 9 {
					frac_val *= 10
					frac_len++
				}
				for frac_len > 9 {
					frac_val /= 10
					frac_len--
				}
				ins = frac_val
				break
			}
		}
		time_parts := tp.split(':')
		if time_parts.len >= 1 {
			ihour = strconv.atoi(time_parts[0]) or { 0 }
		}
		if time_parts.len >= 2 {
			iminute = strconv.atoi(time_parts[1]) or { 0 }
		}
		if time_parts.len >= 3 {
			isecond = strconv.atoi(time_parts[2]) or { 0 }
		}
	}
	return new(Time{
		year:       iyear
		month:      imonth
		day:        iday
		hour:       ihour
		minute:     iminute
		second:     isecond
		nanosecond: ins
	})
}

// parse_rfc3339 parses an RFC 3339 formatted string into a Time.
// RFC 3339 is a profile of ISO 8601: "2023-01-15T14:30:00Z" or "2023-01-15T14:30:00+05:00"
pub fn parse_rfc3339(s string) !Time {
	if s == '' {
		return error('datetime string is empty')
	}
	// RFC 3339 is a subset of ISO 8601, delegate to that parser
	return parse_iso8601(s)
}

// parse returns the time from a date string in "YYYY-MM-DD HH:mm:ss" format.
// On BEAM: Pure V implementation that parses the standard datetime format.
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

// parse_format parses the string s with a custom format into a Time.
// Supports common format specifiers: %Y, %m, %d, %H, %M, %S
pub fn parse_format(s string, format string) !Time {
	if s == '' {
		return error_invalid_time(0, 'datetime string is empty')
	}
	mut iyear := 0
	mut imonth := 1
	mut iday := 1
	mut ihour := 0
	mut iminute := 0
	mut isecond := 0
	mut si := 0 // index into s
	mut fi := 0 // index into format
	for fi < format.len && si < s.len {
		if format[fi] == `%` && fi + 1 < format.len {
			fi++
			match format[fi] {
				`Y` {
					// 4-digit year
					if si + 4 <= s.len {
						iyear = strconv.atoi(s[si..si + 4]) or {
							return error_invalid_time(0, 'invalid year')
						}
						si += 4
					}
				}
				`m` {
					// 2-digit month
					if si + 2 <= s.len {
						imonth = strconv.atoi(s[si..si + 2]) or {
							return error_invalid_time(0, 'invalid month')
						}
						si += 2
					}
				}
				`d` {
					// 2-digit day
					if si + 2 <= s.len {
						iday = strconv.atoi(s[si..si + 2]) or {
							return error_invalid_time(0, 'invalid day')
						}
						si += 2
					}
				}
				`H` {
					// 2-digit hour
					if si + 2 <= s.len {
						ihour = strconv.atoi(s[si..si + 2]) or {
							return error_invalid_time(0, 'invalid hour')
						}
						si += 2
					}
				}
				`M` {
					// 2-digit minute
					if si + 2 <= s.len {
						iminute = strconv.atoi(s[si..si + 2]) or {
							return error_invalid_time(0, 'invalid minute')
						}
						si += 2
					}
				}
				`S` {
					// 2-digit second
					if si + 2 <= s.len {
						isecond = strconv.atoi(s[si..si + 2]) or {
							return error_invalid_time(0, 'invalid second')
						}
						si += 2
					}
				}
				else {
					// Unknown specifier, skip
					si++
				}
			}
			fi++
		} else {
			// Literal character, skip in both strings
			si++
			fi++
		}
	}
	return new(Time{
		year:   iyear
		month:  imonth
		day:    iday
		hour:   ihour
		minute: iminute
		second: isecond
	})
}
