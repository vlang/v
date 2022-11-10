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
