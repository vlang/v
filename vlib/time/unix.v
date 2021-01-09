// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

// unix returns a time struct from Unix time.
pub fn unix(abs int) Time {
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
		unix: u64(abs)
	}
}

// unix2 returns a time struct from Unix time and microsecond value
pub fn unix2(abs int, microsecond int) Time {
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
		unix: u64(abs)
	}
}

fn calculate_date_from_offset(day_offset_ int) (int, int, int) {
	mut day_offset := day_offset_
	// Move offset to year 2001 as it's the start of a new 400-year cycle
	// Code below this rely on the fact that the day_offset is lined up with the 400-year cycle
	// 1970-2000 (inclusive) has 31 years (8 of which are leap years)
	mut year := 2001
	day_offset -= 31 * 365 + 8
	// Account for 400 year cycle
	year += (day_offset / days_per_400_years) * 400
	day_offset %= days_per_400_years
	// Account for 100 year cycle
	if day_offset == days_per_100_years * 4 {
		year += 300
		day_offset -= days_per_100_years * 3
	} else {
		year += (day_offset / days_per_100_years) * 100
		day_offset %= days_per_100_years
	}
	// Account for 4 year cycle
	if day_offset == days_per_4_years * 25 {
		year += 96
		day_offset -= days_per_4_years * 24
	} else {
		year += (day_offset / days_per_4_years) * 4
		day_offset %= days_per_4_years
	}
	// Account for every year
	if day_offset == 365 * 4 {
		year += 3
		day_offset -= 365 * 3
	} else {
		year += (day_offset / 365)
		day_offset %= 365
	}
	if day_offset < 0 {
		year--
		if is_leap_year(year) {
			day_offset += 366
		} else {
			day_offset += 365
		}
	}
	if is_leap_year(year) {
		if day_offset > 31 + 29 - 1 {
			// After leap day; pretend it wasn't there.
			day_offset--
		} else if day_offset == 31 + 29 - 1 {
			// Leap day.
			return year, 2, 29
		}
	}
	mut estimated_month := day_offset / 31
	for day_offset >= days_before[estimated_month + 1] {
		estimated_month++
	}
	for day_offset < days_before[estimated_month] {
		if estimated_month == 0 {
			break
		}
		estimated_month--
	}
	day_offset -= days_before[estimated_month]
	return year, estimated_month + 1, day_offset + 1
}

fn calculate_time_from_offset(second_offset_ int) (int, int, int) {
	mut second_offset := second_offset_
	if second_offset < 0 {
		second_offset += seconds_per_day
	}
	hour := second_offset / seconds_per_hour
	second_offset %= seconds_per_hour
	min := second_offset / seconds_per_minute
	second_offset %= seconds_per_minute
	return hour, min, second_offset
}
