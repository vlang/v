import time

fn test_add_to_day_in_the_previous_century() ? {
	a := time.parse_iso8601('1900-01-01')?
	aa := a.add_days(180)
	dump(a.debug())
	dump(aa.debug())
	assert aa.ymmdd() == '1900-06-30'
}

fn test_add_to_day_in_the_past() ? {
	a := time.parse_iso8601('1990-03-01')?
	aa := a.add_days(180)
	assert aa.ymmdd() == '1990-08-28'
}

fn test_add_to_day_in_the_recent_past() ? {
	a := time.parse_iso8601('2021-03-01')?
	aa := a.add_days(180)
	assert aa.ymmdd() == '2021-08-28'
}

fn test_add_to_day_in_the_future_1() ? {
	a := time.parse_iso8601('3000-11-01')?
	aa := a.add_days(180)
	assert aa.ymmdd() == '3001-04-30'
}

fn test_add_to_day_in_the_future_2() ? {
	a := time.parse_iso8601('3000-12-30')?
	aa := a.add_days(180)
	assert aa.ymmdd() == '3001-06-28'
}
