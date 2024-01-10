module time

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
	days_since_1970 := i64(days_from_unix_epoch(year, month + 1, t.tm_mday))
	return 60 * (60 * (24 * days_since_1970 + t.tm_hour) + t.tm_min) + t.tm_sec
}
