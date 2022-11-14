// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

import strings

// format returns a date string in "YYYY-MM-DD HH:mm" format (24h).
pub fn (t Time) format() string {
	return '${t.year:04d}-${t.month:02d}-${t.day:02d} ${t.hour:02d}:${t.minute:02d}'
}

// format_ss returns a date string in "YYYY-MM-DD HH:mm:ss" format (24h).
pub fn (t Time) format_ss() string {
	return '${t.year:04d}-${t.month:02d}-${t.day:02d} ${t.hour:02d}:${t.minute:02d}:${t.second:02d}'
}

// format_ss_milli returns a date string in "YYYY-MM-DD HH:mm:ss.123" format (24h).
pub fn (t Time) format_ss_milli() string {
	return '${t.year:04d}-${t.month:02d}-${t.day:02d} ${t.hour:02d}:${t.minute:02d}:${t.second:02d}.${(t.microsecond / 1000):03d}'
}

// format_ss_micro returns a date string in "YYYY-MM-DD HH:mm:ss.123456" format (24h).
pub fn (t Time) format_ss_micro() string {
	return '${t.year:04d}-${t.month:02d}-${t.day:02d} ${t.hour:02d}:${t.minute:02d}:${t.second:02d}.${t.microsecond:06d}'
}

// hhmm returns a date string in "HH:mm" format (24h).
pub fn (t Time) hhmm() string {
	return '${t.hour:02d}:${t.minute:02d}'
}

// hhmmss returns a date string in "HH:mm:ss" format (24h).
pub fn (t Time) hhmmss() string {
	return '${t.hour:02d}:${t.minute:02d}:${t.second:02d}'
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

const (
	tokens_2 = ['MM', 'DD', 'Do', 'YY', 'ss', 'kk', 'NN', 'mm', 'hh', 'HH', 'ZZ', 'dd', 'Qo', 'QQ',
		'wo', 'ww']
	tokens_3 = ['MMM', 'DDD', 'ZZZ', 'ddd']
	tokens_4 = ['MMMM', 'DDDD', 'DDDo', 'dddd', 'YYYY']
)

// custom_format returns a date with custom format
//
// |  | Token  | Output |
// | ----------:  | :------ | :--------- |
// | **Month**   | M | 1 2 ... 11 12 |
// |  | Mo | 1st 2nd ... 11th 12th |
// |  | MM | 01 02 ... 11 12 |
// |  | MMM | 	Jan Feb ... Nov Dec |
// |  | MMMM | January February ... November December |
// | **Quarter**  | Q | 1 2 3 4 |
// |  | QQ | 01 02 03 04 |
// |  | Qo | 1st 2nd 3rd 4th |
// | **Day of Month**  | D | 1 2 ... 30 31 |
// |  | Do | 1st 2nd ... 30th 31st |
// |  | DD | 01 02 ... 30 31 |
// | **Day of Year**  | DDD | 1 2 ... 364 365 |
// |  | DDDo | 1st 2nd ... 364th 365th |
// |  | DDDD | 001 002 ... 364 365 |
// | **Day of Week**  | d | 0 1 ... 5 6 (Sun-Sat) |
// |  | c | 1 2 ... 6 7 (Mon-Sun) |
// |  | dd | Su Mo ... Fr Sa |
// |  | ddd | Sun Mon ... Fri Sat |
// |  | dddd | Sunday Monday ... Friday Saturday |
// | **Week of Year**  | w | 1 2 ... 52 53 |
// |  | wo | 1st 2nd ... 52nd 53rd |
// |  | ww | 01 02 ... 52 53 |
// | **Year**  | YY | 70 71 ... 29 30 |
// |  | YYYY | 1970 1971 ... 2029 2030 |
// | **Era**  | N | BC AD |
// |  | NN | Before Christ, Anno Domini |
// | **AM/PM**  | A | AM PM |
// |  | a | am pm |
// | **Hour**  | H | 0 1 ... 22 23 |
// |  | HH | 00 01 ... 22 23 |
// |  | h | 1 2 ... 11 12 |
// |  | hh | 01 02 ... 11 12 |
// |  | k | 1 2 ... 23 24 |
// |  | kk | 01 02 ... 23 24 |
// | **Minute**  | m | 0 1 ... 58 59 |
// |  | mm | 00 01 ... 58 59 |
// | **Second**  | s | 0 1 ... 58 59 |
// |  | ss | 00 01 ... 58 59 |
// | **Offset**  | Z | -7 -6 ... +5 +6 |
// |  | ZZ | -0700 -0600 ... +0500 +0600 |
// |  | ZZZ | -07:00 -06:00 ... +05:00 +06:00 |
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
			if j == 1 || (j == 2 && s[i..i + j] in time.tokens_2)
				|| (j == 3 && s[i..i + j] in time.tokens_3)
				|| (j == 4 && s[i..i + j] in time.tokens_4) {
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
				sb.write_string((t.day + days_before[t.month - 1] + int(is_leap_year(t.year))).str())
			}
			'DDDD' {
				sb.write_string('${t.day + days_before[t.month - 1] + int(is_leap_year(t.year)):03}')
			}
			'DDDo' {
				sb.write_string(ordinal_suffix(t.day + days_before[t.month - 1] +
					int(is_leap_year(t.year))))
			}
			'd' {
				sb.write_string(t.day_of_week().str())
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
				sb.write_string((t.hour % 12).str())
			}
			'hh' {
				sb.write_string('${(t.hour % 12):02}')
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
				sb.write_string('${mceil((t.day + days_before[t.month - 1] +
					int(is_leap_year(t.year))) / 7):.0}')
			}
			'ww' {
				sb.write_string('${mceil((t.day + days_before[t.month - 1] +
					int(is_leap_year(t.year))) / 7):02.0}')
			}
			'wo' {
				sb.write_string(ordinal_suffix(int(mceil((t.day + days_before[t.month - 1] +
					int(is_leap_year(t.year))) / 7))))
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
				sb.write_string('${t.day_of_week() + 1}')
			}
			'N' {
				// TODO integrate BC
				sb.write_string('AD')
			}
			'NN' {
				// TODO integrate Before Christ
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
				// TODO update if minute differs?
				mut hours := offset() / seconds_per_hour
				if hours >= 0 {
					sb.write_string('+${hours:02}00')
				} else {
					hours = -hours
					sb.write_string('-${hours:02}00')
				}
			}
			'ZZZ' {
				// TODO update if minute differs?
				mut hours := offset() / seconds_per_hour
				if hours >= 0 {
					sb.write_string('+${hours:02}:00')
				} else {
					hours = -hours
					sb.write_string('-${hours:02}:00')
				}
			}
			'a' {
				if t.hour > 12 {
					sb.write_string('pm')
				} else {
					sb.write_string('am')
				}
			}
			'A' {
				if t.hour > 12 {
					sb.write_string('PM')
				} else {
					sb.write_string('AM')
				}
			}
			else {
				sb.write_string(token)
			}
		}
	}
	return sb.str()
}

// clean returns a date string in a following format:
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

// clean12 returns a date string in a following format:
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
		.hhmmss24_milli { '${t.hour:02d}:${t.minute:02d}:${t.second:02d}.${(t.microsecond / 1000):03d}' }
		.hhmmss24_micro { '${t.hour:02d}:${t.minute:02d}:${t.second:02d}.${t.microsecond:06d}' }
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

// get_fmt_str returns a date string with specified FormatDelimiter,
// FormatTime type, and FormatDate type.
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
