// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

// format_clean returns a date string in a following format:
//  - a date string in "HH:MM" format (24h) for current day
//  - a date string in "MMM D HH:MM" format (24h) for date of current year
//  - a date string formatted with format function for other dates
pub fn (t Time) format_clean() string {
	now := time.now()
	// Today
	if t.month == now.month && t.year == now.year && t.day == now.day {
		return t.format_time(.hhmm24)
	}
	// This year
	if t.year == now.year {
		return t.format(.space, .hhmm24, .mmmd)
	}
	return t.format(.hyphen, .hhmm24, .yyyymmdd)
}

// format_clean12 returns a date string in a following format:
//  - a date string in "HH:MM" format (12h) for current day
//  - a date string in "MMM D HH:MM" format (12h) for date of current year
//  - a date string formatted with format function for other dates
pub fn (t Time) format_clean12() string {
	now := time.now()
	// Today
	if t.month == now.month && t.year == now.year && t.day == now.day {
		return t.format_time(.hhmm12)
	}
	// This year
	if t.year == now.year {
		return t.format(.space, .hhmm12, .mmmd)
	}
	return t.format(.hyphen, .hhmm24, .yyyymmdd)
}

// format_time returns a date string with specified FormatTime type.
pub fn (t Time) format_time(fmt_time FormatTime) string {
	if fmt_time == .no_time {
		return ''
	}
	tp := if t.hour > 11 { 'p.m.' } else { 'a.m.' }
	hour := if t.hour > 12 { t.hour - 12 } else if t.hour == 0 { 12 } else { t.hour }
	return match fmt_time {
		.hhmm12{
			'$hour:${t.minute:02d} $tp'
		}
		.hhmm24{
			'${t.hour:02d}:${t.minute:02d}'
		}
		.hhmmss12{
			'$hour:${t.minute:02d}:${t.second:02d} $tp'
		}
		.hhmmss24{
			'${t.hour:02d}:${t.minute:02d}:${t.second:02d}'
		}
		else {
			'unknown enumeration $fmt_time'}
	}
}

// format_date returns a date string with specified
// FormatDelimiter and FormatDate type.
pub fn (t Time) format_date(fmt_dlmtr FormatDelimiter, fmt_date FormatDate) string {
	if fmt_date == .no_date {
		return ''
	}
	month := '${t.smonth()}'
	year := t.year.str()[2..]
mut 	res := match fmt_date {
		.ddmmyy{
			'${t.day:02d}|${t.month:02d}|$year'
		}
		.ddmmyyyy{
			'${t.day:02d}|${t.month:02d}|${t.year}'
		}
		.mmddyy{
			'${t.month:02d}|${t.day:02d}|$year'
		}
		.mmddyyyy{
			'${t.month:02d}|${t.day:02d}|${t.year}'
		}
		.mmmd{
			'$month|${t.day}'
		}
		.mmmdd{
			'$month|${t.day:02d}'
		}
		.mmmddyyyy{
			'$month|${t.day:02d}|${t.year}'
		}
		.yyyymmdd{
			'${t.year}|${t.month:02d}|${t.day:02d}'
		}
		else {
			'unknown enumeration $fmt_date'}}
	res = res.replace('|', match fmt_dlmtr {
		.dot{
			'.'
		}
		.hyphen{
			'-'
		}
		.slash{
			'/'
		}
		.space{
			' '
		}
		else {
			'unknown enumeration $fmt_dlmtr'}})
	return res
}

// format returns a date string with specified FormatDelimiter,
// FormatTime type, and FormatDate type.
pub fn (t Time) format(fmt_dlmtr FormatDelimiter, fmt_time FormatTime, fmt_date FormatDate) string {
	if fmt_date == .no_date {
		if fmt_time == .no_time {
			// saving one function call although it's checked in
			// t.get_fmt_time_str(fmt_time) in the beginning
			return ''
		}
		else {
			return t.format_time(fmt_time)
		}
	}
	else {
		if fmt_time != .no_time {
			return t.format_date(fmt_dlmtr, fmt_date) + ' ' + t.format_time(fmt_time)
		}
		else {
			return t.format_date(fmt_dlmtr, fmt_date)
		}
	}
}
