// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

// unix returns a time struct from Unix time.
pub fn unix(abs i64) Time {
	// Split into day and time
	mut day_offset := abs / seconds_per_day
	if abs % seconds_per_day < 0 {
		// Compensate for round towards zero on integers as we want floored instead
		day_offset--
	}
	year, month, day := calculate_date_from_offset(day_offset)
	hr, min, sec := calculate_time_from_offset(abs % seconds_per_day)
	return Time{
		year: year
		month: month
		day: day
		hour: hr
		minute: min
		second: sec
		unix: abs
	}
}

// unix2 returns a time struct from Unix time and microsecond value
pub fn unix2(abs i64, microsecond int) Time {
	// Split into day and time
	mut day_offset := abs / seconds_per_day
	if abs % seconds_per_day < 0 {
		// Compensate for round towards zero on integers as we want floored instead
		day_offset--
	}
	year, month, day := calculate_date_from_offset(day_offset)
	hr, min, sec := calculate_time_from_offset(abs % seconds_per_day)
	return Time{
		year: year
		month: month
		day: day
		hour: hr
		minute: min
		second: sec
		microsecond: microsecond
		unix: abs
	}
}

fn calculate_date_from_offset(day_offset_ i64) (int, int, int) {
	mut day_offset := day_offset_

	// source: http://howardhinnant.github.io/date_algorithms.html#civil_from_days

	// shift from 1970-01-01 to 0000-03-01
	day_offset += 719468 // int(days_per_400_years * 1970 / 400 - (28+31))

	mut era := 0
	if day_offset >= 0 {
		era = int(day_offset / days_per_400_years)
	} else {
		era = int((day_offset - days_per_400_years - 1) / days_per_400_years)
	}
	// doe(day of era) [0, 146096]
	doe := day_offset - era * days_per_400_years
	// yoe(year of era) [0, 399]
	yoe := (doe - doe / (days_per_4_years - 1) + doe / days_per_100_years - doe / (days_per_400_years - 1)) / days_in_year
	// year number
	mut y := int(yoe + era * 400)
	// doy (day of year), with year beginning Mar 1 [0, 365]
	doy := doe - (days_in_year * yoe + yoe / 4 - yoe / 100)

	mp := (5 * doy + 2) / 153
	d := int(doy - (153 * mp + 2) / 5 + 1)
	mut m := int(mp)
	if mp < 10 {
		m += 3
	} else {
		m -= 9
	}
	if m <= 2 {
		y += 1
	}
	return y, m, d
}

fn calculate_time_from_offset(second_offset_ i64) (int, int, int) {
	mut second_offset := second_offset_
	if second_offset < 0 {
		second_offset += seconds_per_day
	}
	hour_ := second_offset / seconds_per_hour
	second_offset %= seconds_per_hour
	min := second_offset / seconds_per_minute
	second_offset %= seconds_per_minute
	return int(hour_), int(min), int(second_offset)
}
