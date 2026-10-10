module time

fn assert_new_checked_fails(t Time, expected string) {
	new_checked(t) or {
		assert err.msg() == expected, 'got `${err.msg()}`, want `${expected}`'
		return
	}
	assert false, 'expected `${t.str()}` to be rejected'
}

fn test_time_new_fills_in_a_default_month_and_day() {
	t := Time.new(Time{ year: 2024 })
	assert t.year == 2024
	assert t.month == 1
	assert t.day == 1
	assert t.hour == 0
	assert t.minute == 0
	assert t.second == 0
	assert t.nanosecond == 0
	assert t.is_local == false
}

fn test_time_new_equals_the_free_function() {
	with_all_fields := Time{
		year:       2024
		month:      3
		day:        10
		hour:       5
		minute:     6
		second:     7
		nanosecond: 891011
	}
	assert new(with_all_fields).str() == Time.new(with_all_fields).str()
	assert new(with_all_fields).unix() == Time.new(with_all_fields).unix()
}

fn test_time_new_computes_unix_from_the_calendar_fields() {
	assert Time.new(Time{ year: 1970, month: 1, day: 1 }).unix() == 0
	assert Time.new(Time{ year: 1970, month: 1, day: 2 }).unix() == 86400
	assert Time.new(Time{ year: 1969, month: 1, day: 1 }).unix() == -31536000
	assert Time.new(Time{ year: 2000, month: 1, day: 1 }).unix() == 946684800
	assert Time.new(Time{ year: 2000, month: 2, day: 29 }).unix() == 951782400
	assert Time.new(Time{ year: 2100, month: 3, day: 1 }).unix() == 4107542400
	assert Time.new(Time{ year: 2024, month: 3, day: 10, hour: 5, minute: 6, second: 7 }).unix() == 1710047167
	assert Time.new(Time{ year: 2024, month: 3, day: 10, hour: 5, minute: 6, second: 7 }).str() == '2024-03-10 05:06:07'
}

// 2000 is a leap year and 2100 is not, so the two spans below are the two
// ends of the rule: January to March is 60 days long in a leap year, 59
// otherwise.
fn test_time_new_handles_the_leap_year_edge_dates() {
	leap := Time.new(Time{ year: 2000, month: 2, day: 29 })
	assert leap.year == 2000
	assert leap.month == 2
	assert leap.day == 29
	assert Time.new(Time{ year: 2000, month: 3, day: 1 }).unix() -
		Time.new(Time{ year: 2000, month: 1, day: 1 }).unix() == 60 * 86400
	assert Time.new(Time{ year: 2100, month: 3, day: 1 }).unix() -
		Time.new(Time{ year: 2100, month: 1, day: 1 }).unix() == 59 * 86400
}

fn test_time_new_accepts_the_extreme_years() {
	assert Time.new(Time{ year: 9999, month: 12, day: 31, hour: 23, minute: 59, second: 59 }).unix() == 253402300799
	assert Time.new(Time{ year: -9999, month: 1, day: 1 }).unix() == -377705116800
	assert Time.new(Time{ year: 1, month: 1, day: 1 }).unix() == -62135596800
	assert Time.new(Time{}).unix() == -62167132800
}

// When `unix` is already set, `Time.new` keeps it rather than recomputing it
// from the calendar fields, which are left untouched even if the two disagree.
fn test_time_new_preserves_an_already_set_unix_value() {
	t := Time{ year: 2024, month: 3, day: 10, unix: 12345 }
	assert Time.new(t).unix() == 12345
	assert Time.new(t).str() == '2024-03-10 00:00:00'
}

// Round-tripping a `Time` through `new` must not move the calendar fields, and
// the unix value must match the one computed from those fields.
fn test_time_new_round_trips_through_unix_dates() {
	for unix_ts in [i64(0), 1, 86400, 1078058096, 1564366499, 1781531121, 4107542400] {
		built := unix_nanosecond(unix_ts, 0)
		round_tripped := Time.new(built)
		assert round_tripped.str() == built.str(), '${built.str()} -> ${round_tripped.str()}'
		assert round_tripped.unix() == unix_ts, '${unix_ts} -> ${round_tripped.unix()}'
	}
}

// `Time.new` panics on these; `new_checked` reports the same message as an
// error, so it is the cheaper way to pin the validation down.
fn test_new_checked_reports_the_same_validation_as_new() {
	assert_new_checked_fails(Time{ year: 10000 }, 'invalid time: year must be between -9999 and 9999')
	assert_new_checked_fails(Time{ year: -10000 }, 'invalid time: year must be between -9999 and 9999')
	assert_new_checked_fails(Time{ year: 2024, month: 13 }, 'invalid time: month must be between 1 and 12')
	assert_new_checked_fails(Time{ year: 2024, month: 2, day: 30 },
		'invalid time: day must be between 1 and 29 for year 2024, month 2')
	assert_new_checked_fails(Time{ year: 2024, minute: 60 }, 'invalid time: minute must be between 0 and 59')
	assert_new_checked_fails(Time{ year: 2024, second: 60 }, 'invalid time: second must be between 0 and 59')
	assert_new_checked_fails(Time{ year: 2024, second: -1 }, 'invalid time: second must be between 0 and 59')
	assert_new_checked_fails(Time{ year: 2024, nanosecond: 1_000_000_000 },
		'invalid time: nanosecond must be between 0 and 999999999')
	assert_new_checked_fails(Time{ year: 2024, nanosecond: -1 },
		'invalid time: nanosecond must be between 0 and 999999999')
}

fn test_new_checked_agrees_with_new_for_valid_times() {
	for fields in [Time{ year: 2024 }, Time{ year: 2000, month: 2, day: 29 },
		Time{ year: 1, month: 1, day: 1 },
		Time{ year: 9999, month: 12, day: 31, hour: 23, minute: 59, second: 59 }] {
		assert new_checked(fields) or { Time{} }.str() == Time.new(fields).str()
	}
}
