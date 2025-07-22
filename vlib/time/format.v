// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

import strings

// int_to_byte_array_no_pad fulfill buffer by part
// it doesn't pad with leading zeros for performance reasons
@[direct_array_access]
fn int_to_byte_array_no_pad(value int, mut arr []u8, size int) {
	mut num := value
	if size <= 0 || num < 0 {
		return
	}

	// Start from the end of the array
	mut i := size - 1

	// Convert each digit to a character and store it in the array
	for num > 0 && i >= 0 {
		arr[i] = (num % 10) + `0`
		num /= 10
		i--
	}
}

// format returns a date string in "YYYY-MM-DD HH:mm" format (24h).
@[manualfree]
pub fn (t Time) format() string {
	mut buf := [u8(`0`), `0`, `0`, `0`, `-`, `0`, `0`, `-`, `0`, `0`, ` `, `0`, `0`, `:`, `0`,
		`0`]

	defer {
		unsafe { buf.free() }
	}

	int_to_byte_array_no_pad(t.year, mut buf, 4)
	int_to_byte_array_no_pad(t.month, mut buf, 7)
	int_to_byte_array_no_pad(t.day, mut buf, 10)

	int_to_byte_array_no_pad(t.hour, mut buf, 13)
	int_to_byte_array_no_pad(t.minute, mut buf, 16)

	return buf.bytestr()
}

// format_ss returns a date string in "YYYY-MM-DD HH:mm:ss" format (24h).
@[manualfree]
pub fn (t Time) format_ss() string {
	mut buf := [u8(`0`), `0`, `0`, `0`, `-`, `0`, `0`, `-`, `0`, `0`, ` `, `0`, `0`, `:`, `0`,
		`0`, `:`, `0`, `0`]

	defer {
		unsafe { buf.free() }
	}

	int_to_byte_array_no_pad(t.year, mut buf, 4)
	int_to_byte_array_no_pad(t.month, mut buf, 7)
	int_to_byte_array_no_pad(t.day, mut buf, 10)

	int_to_byte_array_no_pad(t.hour, mut buf, 13)
	int_to_byte_array_no_pad(t.minute, mut buf, 16)
	int_to_byte_array_no_pad(t.second, mut buf, 19)

	return buf.bytestr()
}

// format_ss_milli returns a date string in "YYYY-MM-DD HH:mm:ss.123" format (24h).
@[manualfree]
pub fn (t Time) format_ss_milli() string {
	mut buf := [u8(`0`), `0`, `0`, `0`, `-`, `0`, `0`, `-`, `0`, `0`, ` `, `0`, `0`, `:`, `0`,
		`0`, `:`, `0`, `0`, `.`, `0`, `0`, `0`]

	defer {
		unsafe { buf.free() }
	}

	int_to_byte_array_no_pad(t.year, mut buf, 4)
	int_to_byte_array_no_pad(t.month, mut buf, 7)
	int_to_byte_array_no_pad(t.day, mut buf, 10)

	int_to_byte_array_no_pad(t.hour, mut buf, 13)
	int_to_byte_array_no_pad(t.minute, mut buf, 16)
	int_to_byte_array_no_pad(t.second, mut buf, 19)

	// Extract and format milliseconds
	millis := t.nanosecond / 1_000_000
	int_to_byte_array_no_pad(millis, mut buf, 23)

	return buf.bytestr()
}

// format_ss_micro returns a date string in "YYYY-MM-DD HH:mm:ss.123456" format (24h).
@[manualfree]
pub fn (t Time) format_ss_micro() string {
	mut buf := [u8(`0`), `0`, `0`, `0`, `-`, `0`, `0`, `-`, `0`, `0`, ` `, `0`, `0`, `:`, `0`,
		`0`, `:`, `0`, `0`, `.`, `0`, `0`, `0`, `0`, `0`, `0`]

	defer {
		unsafe { buf.free() }
	}

	int_to_byte_array_no_pad(t.year, mut buf, 4)
	int_to_byte_array_no_pad(t.month, mut buf, 7)
	int_to_byte_array_no_pad(t.day, mut buf, 10)

	int_to_byte_array_no_pad(t.hour, mut buf, 13)
	int_to_byte_array_no_pad(t.minute, mut buf, 16)
	int_to_byte_array_no_pad(t.second, mut buf, 19)

	// Extract and format microseconds
	micros := t.nanosecond / 1_000
	int_to_byte_array_no_pad(micros, mut buf, 26)

	return buf.bytestr()
}

// format_ss_nano returns a date string in "YYYY-MM-DD HH:mm:ss.123456789" format (24h).
@[manualfree]
pub fn (t Time) format_ss_nano() string {
	mut buf := [u8(`0`), `0`, `0`, `0`, `-`, `0`, `0`, `-`, `0`, `0`, ` `, `0`, `0`, `:`, `0`,
		`0`, `:`, `0`, `0`, `.`, `0`, `0`, `0`, `0`, `0`, `0`, `0`, `0`, `0`]

	defer {
		unsafe { buf.free() }
	}

	int_to_byte_array_no_pad(t.year, mut buf, 4)
	int_to_byte_array_no_pad(t.month, mut buf, 7)
	int_to_byte_array_no_pad(t.day, mut buf, 10)

	int_to_byte_array_no_pad(t.hour, mut buf, 13)
	int_to_byte_array_no_pad(t.minute, mut buf, 16)
	int_to_byte_array_no_pad(t.second, mut buf, 19)

	int_to_byte_array_no_pad(t.nanosecond, mut buf, 29) // Adjusted index for 9 digits

	return buf.bytestr()
}

// format_rfc3339 returns a date string in "YYYY-MM-DDTHH:mm:ss.123Z" format (24 hours, see https://www.rfc-editor.org/rfc/rfc3339.html)
// RFC3339 is an Internet profile, based on the ISO 8601 standard for for representation of dates and times using the Gregorian calendar.
// It is intended to improve consistency and interoperability, when representing and using date and time in Internet protocols.
@[manualfree]
pub fn (t Time) format_rfc3339() string {
	mut buf := [u8(`0`), `0`, `0`, `0`, `-`, `0`, `0`, `-`, `0`, `0`, `T`, `0`, `0`, `:`, `0`,
		`0`, `:`, `0`, `0`, `.`, `0`, `0`, `0`, `Z`]

	defer {
		unsafe { buf.free() }
	}

	t_ := time_with_unix(t)
	if t_.is_local {
		utc_time := t_.local_to_utc()
		int_to_byte_array_no_pad(utc_time.year, mut buf, 4)
		int_to_byte_array_no_pad(utc_time.month, mut buf, 7)
		int_to_byte_array_no_pad(utc_time.day, mut buf, 10)
		int_to_byte_array_no_pad(utc_time.hour, mut buf, 13)
		int_to_byte_array_no_pad(utc_time.minute, mut buf, 16)
		int_to_byte_array_no_pad(utc_time.second, mut buf, 19)
		int_to_byte_array_no_pad(utc_time.nanosecond / 1_000_000, mut buf, 23)
	} else {
		int_to_byte_array_no_pad(t_.year, mut buf, 4)
		int_to_byte_array_no_pad(t_.month, mut buf, 7)
		int_to_byte_array_no_pad(t_.day, mut buf, 10)
		int_to_byte_array_no_pad(t_.hour, mut buf, 13)
		int_to_byte_array_no_pad(t_.minute, mut buf, 16)
		int_to_byte_array_no_pad(t_.second, mut buf, 19)
		int_to_byte_array_no_pad(t_.nanosecond / 1_000_000, mut buf, 23)
	}

	return buf.bytestr()
}

// format_rfc3339_micro returns a date string in "YYYY-MM-DDTHH:mm:ss.123456Z" format (24 hours, see https://www.rfc-editor.org/rfc/rfc3339.html)
@[manualfree]
pub fn (t Time) format_rfc3339_micro() string {
	mut buf := [u8(`0`), `0`, `0`, `0`, `-`, `0`, `0`, `-`, `0`, `0`, `T`, `0`, `0`, `:`, `0`,
		`0`, `:`, `0`, `0`, `.`, `0`, `0`, `0`, `0`, `0`, `0`, `Z`]

	defer {
		unsafe { buf.free() }
	}

	t_ := time_with_unix(t)
	if t_.is_local {
		utc_time := t_.local_to_utc()
		int_to_byte_array_no_pad(utc_time.year, mut buf, 4)
		int_to_byte_array_no_pad(utc_time.month, mut buf, 7)
		int_to_byte_array_no_pad(utc_time.day, mut buf, 10)
		int_to_byte_array_no_pad(utc_time.hour, mut buf, 13)
		int_to_byte_array_no_pad(utc_time.minute, mut buf, 16)
		int_to_byte_array_no_pad(utc_time.second, mut buf, 19)
		int_to_byte_array_no_pad(utc_time.nanosecond / 1000, mut buf, 26)
	} else {
		int_to_byte_array_no_pad(t_.year, mut buf, 4)
		int_to_byte_array_no_pad(t_.month, mut buf, 7)
		int_to_byte_array_no_pad(t_.day, mut buf, 10)
		int_to_byte_array_no_pad(t_.hour, mut buf, 13)
		int_to_byte_array_no_pad(t_.minute, mut buf, 16)
		int_to_byte_array_no_pad(t_.second, mut buf, 19)
		int_to_byte_array_no_pad(t_.nanosecond / 1000, mut buf, 26)
	}

	return buf.bytestr()
}

// format_rfc3339_nano returns a date string in "YYYY-MM-DDTHH:mm:ss.123456789Z" format (24 hours, see https://www.rfc-editor.org/rfc/rfc3339.html)
@[manualfree]
pub fn (t Time) format_rfc3339_nano() string {
	mut buf := [u8(`0`), `0`, `0`, `0`, `-`, `0`, `0`, `-`, `0`, `0`, `T`, `0`, `0`, `:`, `0`,
		`0`, `:`, `0`, `0`, `.`, `0`, `0`, `0`, `0`, `0`, `0`, `0`, `0`, `0`, `Z`]

	defer {
		unsafe { buf.free() }
	}

	t_ := time_with_unix(t)
	if t_.is_local {
		utc_time := t_.local_to_utc()
		int_to_byte_array_no_pad(utc_time.year, mut buf, 4)
		int_to_byte_array_no_pad(utc_time.month, mut buf, 7)
		int_to_byte_array_no_pad(utc_time.day, mut buf, 10)
		int_to_byte_array_no_pad(utc_time.hour, mut buf, 13)
		int_to_byte_array_no_pad(utc_time.minute, mut buf, 16)
		int_to_byte_array_no_pad(utc_time.second, mut buf, 19)
		int_to_byte_array_no_pad(utc_time.nanosecond, mut buf, 29)
	} else {
		int_to_byte_array_no_pad(t_.year, mut buf, 4)
		int_to_byte_array_no_pad(t_.month, mut buf, 7)
		int_to_byte_array_no_pad(t_.day, mut buf, 10)
		int_to_byte_array_no_pad(t_.hour, mut buf, 13)
		int_to_byte_array_no_pad(t_.minute, mut buf, 16)
		int_to_byte_array_no_pad(t_.second, mut buf, 19)
		int_to_byte_array_no_pad(t_.nanosecond, mut buf, 29)
	}

	return buf.bytestr()
}

// hhmm returns a date string in "HH:mm" format (24h).
@[manualfree]
pub fn (t Time) hhmm() string {
	mut buf := [u8(`0`), `0`, `:`, `0`, `0`]

	defer {
		unsafe { buf.free() }
	}

	int_to_byte_array_no_pad(t.hour, mut buf, 2)
	int_to_byte_array_no_pad(t.minute, mut buf, 5)

	return buf.bytestr()
}

// hhmmss returns a date string in "HH:mm:ss" format (24h).
@[manualfree]
pub fn (t Time) hhmmss() string {
	mut buf := [u8(`0`), `0`, `:`, `0`, `0`, `:`, `0`, `0`]

	defer {
		unsafe { buf.free() }
	}

	int_to_byte_array_no_pad(t.hour, mut buf, 2)
	int_to_byte_array_no_pad(t.minute, mut buf, 5)
	int_to_byte_array_no_pad(t.second, mut buf, 8)

	return buf.bytestr()
}

// hhmm12 returns a date string in "hh:mm" format (12h).
pub fn (t Time) hhmm12() string {
	return t.get_fmt_time_str(.hhmm12)
}

// ymmdd returns a date string in "YYYY-MM-DD" format.
pub fn (t Time) ymmdd() string {
	return t.get_fmt_date_str(.hyphen, .yyyymmdd)
}

// ddmmy returns a date string in "DD.MM.YYYY" format.
pub fn (t Time) ddmmy() string {
	return t.get_fmt_date_str(.dot, .ddmmyyyy)
}

// md returns a date string in "MMM D" format.
pub fn (t Time) md() string {
	return t.get_fmt_date_str(.space, .mmmd)
}

// TODO: test, improve performance
// appends ordinal suffix to a number
fn ordinal_suffix(n int) string {
	if n > 3 && n < 21 {
		return '${n}th'
	}
	match n % 10 {
		1 {
			return '${n}st'
		}
		2 {
			return '${n}nd'
		}
		3 {
			return '${n}rd'
		}
		else {
			return '${n}th'
		}
	}
}

const tokens_2 = ['MM', 'Mo', 'DD', 'Do', 'YY', 'ss', 'kk', 'NN', 'mm', 'hh', 'HH', 'ii', 'ZZ',
	'dd', 'Qo', 'QQ', 'wo', 'ww']
const tokens_3 = ['MMM', 'DDD', 'ZZZ', 'ddd']
const tokens_4 = ['MMMM', 'DDDD', 'DDDo', 'dddd', 'YYYY']

// custom_format returns a date with custom format
//
// | Category         | Token | Output                                 |
// |:-----------------|:------|:---------------------------------------|
// |          Era     | N     | BC AD                                  |
// |                  | NN    | Before Christ, Anno Domini             |
// |         Year     | YY    | 70 71 ... 29 30                        |
// |                  | YYYY  | 1970 1971 ... 2029 2030                |
// |      Quarter     | Q     | 1 2 3 4                                |
// |                  | QQ    | 01 02 03 04                            |
// |                  | Qo    | 1st 2nd 3rd 4th                        |
// |        Month     | M     | 1 2 ... 11 12                          |
// |                  | Mo    | 1st 2nd ... 11th 12th                  |
// |                  | MM    | 01 02 ... 11 12                        |
// |                  | MMM   | Jan Feb ... Nov Dec                    |
// |                  | MMMM  | January February ... November December |
// | Week of Year     | w     | 1 2 ... 52 53                          |
// |                  | wo    | 1st 2nd ... 52nd 53rd                  |
// |                  | ww    | 01 02 ... 52 53                        |
// | Day of Month     | D     | 1 2 ... 30 31                          |
// |                  | Do    | 1st 2nd ... 30th 31st                  |
// |                  | DD    | 01 02 ... 30 31                        |
// |  Day of Year     | DDD   | 1 2 ... 364 365                        |
// |                  | DDDo  | 1st 2nd ... 364th 365th                |
// |                  | DDDD  | 001 002 ... 364 365                    |
// |  Day of Week     | d     | 0 1 ... 5 6 (Sun-Sat)                  |
// |                  | c     | 1 2 ... 6 7 (Mon-Sun)                  |
// |                  | dd    | Su Mo ... Fr Sa                        |
// |                  | ddd   | Sun Mon ... Fri Sat                    |
// |                  | dddd  | Sunday Monday ... Friday Saturday      |
// |        AM/PM     | A     | AM PM                                  |
// |                  | a     | am pm                                  |
// |         Hour     | H     | 0 1 ... 22 23                          |
// |                  | HH    | 00 01 ... 22 23                        |
// |                  | h     | 1 2 ... 11 12                          |
// |                  | hh    | 01 02 ... 11 12                        |
// |                  | i     | 0 1 ... 11 12 1 ... 11                 |
// |                  | ii    | 00 01 ... 11 12 01 ... 11              |
// |                  | k     | 1 2 ... 23 24                          |
// |                  | kk    | 01 02 ... 23 24                        |
// |       Minute     | m     | 0 1 ... 58 59                          |
// |                  | mm    | 00 01 ... 58 59                        |
// |       Second     | s     | 0 1 ... 58 59                          |
// |                  | ss    | 00 01 ... 58 59                        |
// |       Offset     | Z     | -7 -6 ... +5 +6                        |
// |                  | ZZ    | -0700 -0600 ... +0500 +0600            |
// |                  | ZZZ   | -07:00 -06:00 ... +05:00 +06:00        |
//
// Usage:
// ```v
// println(time.now().custom_format('MMMM Mo YY N kk:mm:ss A')) // output like: January 1st 22 AD 13:45:33 PM
// ```
pub fn (t Time) custom_format(s string) string {
	mut tokens := []string{}
	for i := 0; i < s.len; {
		for j := 4; j > 0; j-- {
			if i > s.len - j {
				continue
			}
			if j == 1 || (j == 2 && s[i..i + j] in tokens_2)
				|| (j == 3 && s[i..i + j] in tokens_3)
				|| (j == 4 && s[i..i + j] in tokens_4) {
				tokens << s[i..i + j]
				i += (j - 1)
				break
			}
		}
		i++
	}
	mut sb := strings.new_builder(128)

	for token in tokens {
		match token {
			'M' {
				sb.write_string(t.month.str())
			}
			'MM' {
				sb.write_string('${t.month:02}')
			}
			'Mo' {
				sb.write_string(ordinal_suffix(t.month))
			}
			'MMM' {
				sb.write_string(long_months[t.month - 1][0..3])
			}
			'MMMM' {
				sb.write_string(long_months[t.month - 1])
			}
			'D' {
				sb.write_string(t.day.str())
			}
			'DD' {
				sb.write_string('${t.day:02}')
			}
			'Do' {
				sb.write_string(ordinal_suffix(t.day))
			}
			'DDD' {
				sb.write_string((t.year_day()).str())
			}
			'DDDD' {
				sb.write_string('${t.year_day():03}')
			}
			'DDDo' {
				sb.write_string(ordinal_suffix(t.year_day()))
			}
			'd' {
				sb.write_string('${t.day_of_week() % 7}')
			}
			'dd' {
				sb.write_string(long_days[t.day_of_week() - 1][0..2])
			}
			'ddd' {
				sb.write_string(long_days[t.day_of_week() - 1][0..3])
			}
			'dddd' {
				sb.write_string(long_days[t.day_of_week() - 1])
			}
			'YY' {
				sb.write_string(t.year.str()[2..4])
			}
			'YYYY' {
				sb.write_string(t.year.str())
			}
			'H' {
				sb.write_string(t.hour.str())
			}
			'HH' {
				sb.write_string('${t.hour:02}')
			}
			'h' {
				h := (t.hour + 11) % 12 + 1
				sb.write_string(h.str())
			}
			'hh' {
				h := (t.hour + 11) % 12 + 1
				sb.write_string('${h:02}')
			}
			'i' {
				h := if t.hour > 12 { t.hour - 12 } else { t.hour }
				sb.write_string(h.str())
			}
			'ii' {
				h := if t.hour > 12 { t.hour - 12 } else { t.hour }
				sb.write_string('${h:02}')
			}
			'm' {
				sb.write_string(t.minute.str())
			}
			'mm' {
				sb.write_string('${t.minute:02}')
			}
			's' {
				sb.write_string(t.second.str())
			}
			'ss' {
				sb.write_string('${t.second:02}')
			}
			'k' {
				sb.write_string((t.hour + 1).str())
			}
			'kk' {
				sb.write_string('${(t.hour + 1):02}')
			}
			'w' {
				sb.write_string('${t.week_of_year():.0}')
			}
			'ww' {
				sb.write_string('${t.week_of_year():02.0}')
			}
			'wo' {
				sb.write_string(ordinal_suffix(t.week_of_year()))
			}
			'Q' {
				sb.write_string('${(t.month % 4) + 1}')
			}
			'QQ' {
				sb.write_string('${(t.month % 4) + 1:02}')
			}
			'Qo' {
				sb.write_string(ordinal_suffix((t.month % 4) + 1))
			}
			'c' {
				sb.write_string('${t.day_of_week()}')
			}
			'N' {
				// TODO: integrate BC
				sb.write_string('AD')
			}
			'NN' {
				// TODO: integrate Before Christ
				sb.write_string('Anno Domini')
			}
			'Z' {
				mut hours := offset() / seconds_per_hour
				if hours >= 0 {
					sb.write_string('+${hours}')
				} else {
					hours = -hours
					sb.write_string('-${hours}')
				}
			}
			'ZZ' {
				// TODO: update if minute differs?
				mut hours := offset() / seconds_per_hour
				if hours >= 0 {
					sb.write_string('+${hours:02}00')
				} else {
					hours = -hours
					sb.write_string('-${hours:02}00')
				}
			}
			'ZZZ' {
				// TODO: update if minute differs?
				mut hours := offset() / seconds_per_hour
				if hours >= 0 {
					sb.write_string('+${hours:02}:00')
				} else {
					hours = -hours
					sb.write_string('-${hours:02}:00')
				}
			}
			'a' {
				if t.hour < 12 {
					sb.write_string('am')
				} else {
					sb.write_string('pm')
				}
			}
			'A' {
				if t.hour < 12 {
					sb.write_string('AM')
				} else {
					sb.write_string('PM')
				}
			}
			else {
				sb.write_string(token)
			}
		}
	}
	return sb.str()
}

// clean returns a date string in a clean form.
// It has the following format:
// - a date string in "HH:mm" format (24h) for current day
// - a date string in "MMM D HH:mm" format (24h) for date of current year
// - a date string formatted with format function for other dates
pub fn (t Time) clean() string {
	znow := now()
	// Today
	if t.month == znow.month && t.year == znow.year && t.day == znow.day {
		return t.get_fmt_time_str(.hhmm24)
	}
	// This year
	if t.year == znow.year {
		return t.get_fmt_str(.space, .hhmm24, .mmmd)
	}
	return t.format()
}

// clean12 returns a date string in a clean form.
// It has the following format:
// - a date string in "hh:mm" format (12h) for current day
// - a date string in "MMM D hh:mm" format (12h) for date of current year
// - a date string formatted with format function for other dates
pub fn (t Time) clean12() string {
	znow := now()
	// Today
	if t.month == znow.month && t.year == znow.year && t.day == znow.day {
		return t.get_fmt_time_str(.hhmm12)
	}
	// This year
	if t.year == znow.year {
		return t.get_fmt_str(.space, .hhmm12, .mmmd)
	}
	return t.format()
}

// get_fmt_time_str returns a date string with specified FormatTime type.
pub fn (t Time) get_fmt_time_str(fmt_time FormatTime) string {
	if fmt_time == .no_time {
		return ''
	}
	tp := if t.hour > 11 { 'p.m.' } else { 'a.m.' }
	hour_ := if t.hour > 12 {
		t.hour - 12
	} else if t.hour == 0 {
		12
	} else {
		t.hour
	}
	return match fmt_time {
		.hhmm12 { '${hour_}:${t.minute:02d} ${tp}' }
		.hhmm24 { '${t.hour:02d}:${t.minute:02d}' }
		.hhmmss12 { '${hour_}:${t.minute:02d}:${t.second:02d} ${tp}' }
		.hhmmss24 { '${t.hour:02d}:${t.minute:02d}:${t.second:02d}' }
		.hhmmss24_milli { '${t.hour:02d}:${t.minute:02d}:${t.second:02d}.${(t.nanosecond / 1_000_000):03d}' }
		.hhmmss24_micro { '${t.hour:02d}:${t.minute:02d}:${t.second:02d}.${(t.nanosecond / 1_000):06d}' }
		.hhmmss24_nano { '${t.hour:02d}:${t.minute:02d}:${t.second:02d}.${t.nanosecond:06d}' }
		else { 'unknown enumeration ${fmt_time}' }
	}
}

// get_fmt_time_str returns a date string with specified
// FormatDelimiter and FormatDate type.
pub fn (t Time) get_fmt_date_str(fmt_dlmtr FormatDelimiter, fmt_date FormatDate) string {
	if fmt_date == .no_date {
		return ''
	}
	month := t.smonth()
	year := '${(t.year % 100):02d}'
	mut res := match fmt_date {
		.ddmmyy { '${t.day:02d}|${t.month:02d}|${year}' }
		.ddmmyyyy { '${t.day:02d}|${t.month:02d}|${t.year:04d}' }
		.mmddyy { '${t.month:02d}|${t.day:02d}|${year}' }
		.mmddyyyy { '${t.month:02d}|${t.day:02d}|${t.year:04d}' }
		.mmmd { '${month}|${t.day}' }
		.mmmdd { '${month}|${t.day:02d}' }
		.mmmddyy { '${month}|${t.day:02d}|${year}' }
		.mmmddyyyy { '${month}|${t.day:02d}|${t.year:04d}' }
		.yyyymmdd { '${t.year:04d}|${t.month:02d}|${t.day:02d}' }
		.yymmdd { '${year}|${t.month:02d}|${t.day:02d}' }
		else { 'unknown enumeration ${fmt_date}' }
	}
	del := match fmt_dlmtr {
		.dot { '.' }
		.hyphen { '-' }
		.slash { '/' }
		.space { ' ' }
		.no_delimiter { '' }
	}
	res = res.replace('|', del)
	return res
}

// get_fmt_str returns a date string with specified FormatDelimiter, FormatTime type, and FormatDate type.
pub fn (t Time) get_fmt_str(fmt_dlmtr FormatDelimiter, fmt_time FormatTime, fmt_date FormatDate) string {
	if fmt_date == .no_date {
		if fmt_time == .no_time {
			// saving one function call although it's checked in
			// t.get_fmt_time_str(fmt_time) in the beginning
			return ''
		} else {
			return t.get_fmt_time_str(fmt_time)
		}
	} else {
		if fmt_time != .no_time {
			dstr := t.get_fmt_date_str(fmt_dlmtr, fmt_date)
			tstr := t.get_fmt_time_str(fmt_time)
			return '${dstr} ${tstr}'
		} else {
			return t.get_fmt_date_str(fmt_dlmtr, fmt_date)
		}
	}
}

// This is just a TEMPORARY function for cookies and their expire dates
pub fn (t Time) utc_string() string {
	day_str := t.weekday_str()
	month_str := t.smonth()
	utc_string := '${day_str}, ${t.day} ${month_str} ${t.year} ${t.hour:02d}:${t.minute:02d}:${t.second:02d} UTC'
	return utc_string
}

// http_header_string returns a date string in the format used in HTTP headers, as defined in RFC 2616.
// e.g. "Sun, 06 Nov 1994 08:49:37 GMT"
@[manualfree]
pub fn (t Time) http_header_string() string {
	day_str := t.weekday_str()
	month_str := t.smonth()

	mut buf := [day_str[0], day_str[1], day_str[2], `,`, ` `, `0`, `0`, ` `, month_str[0], month_str[1],
		month_str[2], ` `, `0`, `0`, `0`, `0`, ` `, `0`, `0`, `:`, `0`, `0`, `:`, `0`, `0`, ` `,
		`G`, `M`, `T`]

	defer {
		unsafe { buf.free() }
	}

	int_to_byte_array_no_pad(t.day, mut buf, 7)
	int_to_byte_array_no_pad(t.year, mut buf, 16)
	int_to_byte_array_no_pad(t.hour, mut buf, 19)
	int_to_byte_array_no_pad(t.minute, mut buf, 22)
	int_to_byte_array_no_pad(t.second, mut buf, 25)

	http_header_string := buf.bytestr()

	return http_header_string
}

// mceil returns the least integer value greater than or equal to x.
fn mceil(x f64) f64 {
	if x > 0 {
		return 1 + int(x)
	}
	if x < 0 {
		return -int(-x)
	}
	return 0
}
