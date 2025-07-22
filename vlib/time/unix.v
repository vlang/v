// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module time

// unix returns a Time calculated from the given Unix timestamp in seconds since 1970-01-01 .
pub fn unix(epoch i64) Time {
	return unix_nanosecond(epoch, 0)
}

// unix_milli returns a Time calculated from the given Unix timestamp in milliseconds since 1970-01-01 .
pub fn unix_milli(ms i64) Time {
	return ts_to_time_impl(ms, 1_000, 1_000_000)
}

// unix_micro returns a Time calculated from the given Unix timestamp in microseconds since 1970-01-01 .
pub fn unix_micro(us i64) Time {
	return ts_to_time_impl(us, 1_000_000, 1_000)
}

// unix_nano returns a Time calculated from the given Unix timestamp in nanoseconds since 1970-01-01 .
pub fn unix_nano(ns i64) Time {
	return ts_to_time_impl(ns, 1_000_000_000, 1)
}

fn ts_to_time_impl(value i64, down i64, up i64) Time {
	epoch := value / down
	remainder := (value % down) * up
	return unix_nanosecond(epoch, int(remainder))
}

// unix_microsecond returns a Time struct, given an Unix timestamp in seconds, and a microsecond value.
pub fn unix_microsecond(epoch i64, microsecond int) Time {
	return unix_nanosecond(epoch, microsecond * 1000)
}

// unix_nanosecond returns a Time struct given a Unix timestamp in seconds and a nanosecond value.
pub fn unix_nanosecond(abs_unix_timestamp i64, nanosecond int) Time {
	// Split into day and time
	mut day_offset := abs_unix_timestamp / seconds_per_day
	if abs_unix_timestamp % seconds_per_day < 0 {
		// Compensate for rounding towards zero on integers as we want floored instead
		day_offset--
	}
	year, month, day := calculate_date_from_day_offset(day_offset)
	hour_, minute_, second_ := calculate_time_from_second_offset(abs_unix_timestamp % seconds_per_day)
	return Time{
		year:       year
		month:      month
		day:        day
		hour:       hour_
		minute:     minute_
		second:     second_
		nanosecond: nanosecond
		unix:       abs_unix_timestamp
	}
}

// calculate_date_from_day_offset returns the year, month, and day based on the given day offset.
fn calculate_date_from_day_offset(day_offset_ i64) (int, int, int) {
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

	// day_of_era => [0..146096]
	day_of_era := day_offset - era * days_per_400_years

	// year_of_era => [0..399]
	year_of_era := (day_of_era - day_of_era / (days_per_4_years - 1) +
		day_of_era / days_per_100_years - day_of_era / (days_per_400_years - 1)) / days_in_year

	mut year := int(year_of_era + era * 400)

	// day_of_year => with year beginning Mar 1 [0..365]
	day_of_year := day_of_era - (days_in_year * year_of_era + year_of_era / 4 - year_of_era / 100)

	month_position := (5 * day_of_year + 2) / 153
	day := int(day_of_year - (153 * month_position + 2) / 5 + 1)
	mut month := int(month_position)

	if month_position < 10 {
		month += 3
	} else {
		month -= 9
	}

	if month <= 2 {
		year += 1
	}

	return year, month, day
}

// calculate_time_from_second_offset returns the hour, minute, and second
// based on the given second offset.
fn calculate_time_from_second_offset(second_offset_ i64) (int, int, int) {
	mut second_offset := second_offset_
	if second_offset < 0 {
		second_offset += seconds_per_day
	}
	hour_ := second_offset / seconds_per_hour
	second_offset %= seconds_per_hour
	minute_ := second_offset / seconds_per_minute
	second_offset %= seconds_per_minute
	return int(hour_), int(minute_), int(second_offset)
}
