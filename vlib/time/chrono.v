module time

// days_from_unix_epoch - return the number of days since the
// Unix epoch 1970-01-01. A detailed description of the algorithm here
// is in: http://howardhinnant.github.io/date_algorithms.html
// Note that it will return negative values for days before 1970-01-01.
pub fn days_from_unix_epoch(year int, month int, day int) int {
	y := if month <= 2 { year - 1 } else { year }
	era := y / 400
	year_of_the_era := y - era * 400 // [0, 399]
	day_of_year := (153 * (month + (if month > 2 { -3 } else { 9 })) + 2) / 5 + day - 1 // [0, 365]
	day_of_the_era := year_of_the_era * 365 + year_of_the_era / 4 - year_of_the_era / 100 +
		day_of_year // [0, 146096]
	return era * 146097 + day_of_the_era - 719468
}

// days_from_civil - return the number of days since the
// Unix epoch 1970-01-01.
// deprecated: use time.days_from_unix_epoch instead
[deprecated: 'use time.days_from_unix_epoch instead']
[deprecated_after: '2022-11-23']
pub fn days_from_civil(year int, month int, day int) int {
	return days_from_unix_epoch(year, month, day)
}

// (time Time) days_from_unix_epoch - return the number of days since the
// Unix epoch 1970-01-01. A detailed description of the algorithm here
// is in: http://howardhinnant.github.io/date_algorithms.html
// Note that it will return negative values for days before 1970-01-01.
pub fn (time Time) days_from_unix_epoch() int {
	y := if time.month <= 2 { time.year - 1 } else { time.year }
	era := y / f64(400)
	year_of_the_era := y - era * 400 // [0, 399]
	day_of_year := (153 * (time.month + (if time.month > 2 { -3 } else { 9 })) + 2) / f64(5) +
		time.day - 1 // [0, 365]
	day_of_the_era := year_of_the_era * 365 + year_of_the_era / 4 - year_of_the_era / f64(100) +
		day_of_year // [0, 146096]
	return int(era) * 146097 + int(day_of_the_era) - 719468
}

// date_from_days_after_unix_epoch - convert days after unix epoch
// in Time format
pub fn date_from_days_after_unix_epoch(days int) Time {
	// This is a shift which aligns this algorithm with all known implementations of std::chrono::system_clock.
	// It makes the serial date 0 be equivalent to 1970-01-01 instead of 0000-03-01.
	z := days + 719468

	era := (if z >= 0 { z } else { z - 146096 }) / 146097

	day_of_the_era := z - era * 146097 // [0, 146096]

	year_of_the_era := (day_of_the_era - day_of_the_era / 1460 + day_of_the_era / 36524 - day_of_the_era / 146096) / 365

	year := year_of_the_era + era * 400

	day_of_year := day_of_the_era - (365 * year_of_the_era + year_of_the_era / 4 - year_of_the_era / 100)

	mp := (5 * day_of_year + 2) / 153 // [0, 11]
	println('mp: $mp')

	day := day_of_year - (153 * mp + 2) / 5 + 1
	month := int(if mp < 10 { mp + 3 } else { mp - 9 })

	time_result := Time{
		year: year + if month <= 2 { 1 } else { 0 }
		month: month
		day: day
	}

	return time_result
}
