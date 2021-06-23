// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

// format returns a date string in "YYYY-MM-DD HH:MM" format (24h).
pub fn (t Time) format() string {
	return t.get_fmt_str(.hyphen, .hhmm24, .yyyymmdd)
}

// format_ss returns a date string in "YYYY-MM-DD HH:MM:SS" format (24h).
pub fn (t Time) format_ss() string {
	return t.get_fmt_str(.hyphen, .hhmmss24, .yyyymmdd)
}

// format_ss_milli returns a date string in "YYYY-MM-DD HH:MM:SS.123" format (24h).
pub fn (t Time) format_ss_milli() string {
	return t.get_fmt_str(.hyphen, .hhmmss24_milli, .yyyymmdd)
}

// format_ss_micro returns a date string in "YYYY-MM-DD HH:MM:SS.123456" format (24h).
pub fn (t Time) format_ss_micro() string {
	return t.get_fmt_str(.hyphen, .hhmmss24_micro, .yyyymmdd)
}

// hhmm returns a date string in "HH:MM" format (24h).
pub fn (t Time) hhmm() string {
	return t.get_fmt_time_str(.hhmm24)
}

// hhmmss returns a date string in "HH:MM:SS" format (24h).
pub fn (t Time) hhmmss() string {
	return t.get_fmt_time_str(.hhmmss24)
}

// hhmm12 returns a date string in "HH:MM" format (12h).
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

// clean returns a date string in a following format:
// - a date string in "HH:MM" format (24h) for current day
// - a date string in "MMM D HH:MM" format (24h) for date of current year
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
// - a date string in "HH:MM" format (12h) for current day
// - a date string in "MMM D HH:MM" format (12h) for date of current year
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
		.hhmm12 { '$hour_:${t.minute:02d} $tp' }
		.hhmm24 { '${t.hour:02d}:${t.minute:02d}' }
		.hhmmss12 { '$hour_:${t.minute:02d}:${t.second:02d} $tp' }
		.hhmmss24 { '${t.hour:02d}:${t.minute:02d}:${t.second:02d}' }
		.hhmmss24_milli { '${t.hour:02d}:${t.minute:02d}:${t.second:02d}.${(t.microsecond / 1000):03d}' }
		.hhmmss24_micro { '${t.hour:02d}:${t.minute:02d}:${t.second:02d}.${t.microsecond:06d}' }
		else { 'unknown enumeration $fmt_time' }
	}
}

// get_fmt_time_str returns a date string with specified
// FormatDelimiter and FormatDate type.
pub fn (t Time) get_fmt_date_str(fmt_dlmtr FormatDelimiter, fmt_date FormatDate) string {
	if fmt_date == .no_date {
		return ''
	}
	month := '$t.smonth()'
	year := '${(t.year % 100):02d}'
	mut res := match fmt_date {
		.ddmmyy { '${t.day:02d}|${t.month:02d}|$year' }
		.ddmmyyyy { '${t.day:02d}|${t.month:02d}|${t.year:04d}' }
		.mmddyy { '${t.month:02d}|${t.day:02d}|$year' }
		.mmddyyyy { '${t.month:02d}|${t.day:02d}|${t.year:04d}' }
		.mmmd { '$month|$t.day' }
		.mmmdd { '$month|${t.day:02d}' }
		.mmmddyy { '$month|${t.day:02d}|$year' }
		.mmmddyyyy { '$month|${t.day:02d}|${t.year:04d}' }
		.yyyymmdd { '${t.year:04d}|${t.month:02d}|${t.day:02d}' }
		.yymmdd { '$year|${t.month:02d}|${t.day:02d}' }
		else { 'unknown enumeration $fmt_date' }
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
			return t.get_fmt_date_str(fmt_dlmtr, fmt_date) + ' ' + t.get_fmt_time_str(fmt_time)
		} else {
			return t.get_fmt_date_str(fmt_dlmtr, fmt_date)
		}
	}
}

// This is just a TEMPORARY function for cookies and their expire dates
pub fn (t Time) utc_string() string {
	day_str := t.weekday_str()
	month_str := t.smonth()
	utc_string := '$day_str, $t.day $month_str $t.year ${t.hour:02d}:${t.minute:02d}:${t.second:02d} UTC'
	return utc_string
}
