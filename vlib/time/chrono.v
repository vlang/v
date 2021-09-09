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
