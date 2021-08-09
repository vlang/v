module time

// days_from_civil - return the number of days since the
// Unix epoch 1970-01-01. A detailed description of the algorithm here
// is in: http://howardhinnant.github.io/date_algorithms.html
// Note that it will return negative values for days before 1970-01-01.
pub fn days_from_civil(oy int, m int, d int) int {
	y := if m <= 2 { oy - 1 } else { oy }
	era := y / 400
	yoe := y - era * 400 // [0, 399]
	doy := (153 * (m + (if m > 2 { -3 } else { 9 })) + 2) / 5 + d - 1 // [0, 365]
	doe := yoe * 365 + yoe / 4 - yoe / 100 + doy // [0, 146096]
	return era * 146097 + doe - 719468
}

// portable_timegm does the same as C._mkgmtime, but unlike it,
// can work with dates before the Unix epoch of 1970-01-01 .
pub fn portable_timegm(t &C.tm) i64 {
	mut year := t.tm_year + 1900
	mut month := t.tm_mon // 0-11
	if month > 11 {
		year += month / 12
		month %= 12
	} else if month < 0 {
		years_diff := (11 - month) / 12
		year -= years_diff
		month += 12 * years_diff
	}
	days_since_1970 := i64(days_from_civil(year, month + 1, t.tm_mday))
	return 60 * (60 * (24 * days_since_1970 + t.tm_hour) + t.tm_min) + t.tm_sec
}
