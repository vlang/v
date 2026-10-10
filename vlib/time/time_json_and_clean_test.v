import time

// A fixed instant well outside the current year, so the branches of
// `clean()`/`clean12()` that consult `now()` take the same path on every run.
fn fixed_instant() time.Time {
	return time.new(time.Time{
		year:       2001
		month:      2
		day:        3
		hour:       4
		minute:     5
		second:     6
		nanosecond: 0
	})
}

fn test_clean_falls_back_to_format_for_a_year_that_is_not_the_current_one() {
	t := fixed_instant()
	assert t.clean() == t.format()
	assert t.clean12() == t.format()
	assert t.clean() == '2001-02-03 04:05'
}

fn test_clean_uses_a_short_form_inside_the_current_year() {
	year := time.now().year
	// Two dates in the current year: at most one of them can be today, and the
	// branch taken for a date that *is* today is asserted separately below.
	dates := [
		time.new(time.Time{
			year:   year
			month:  2
			day:    14
			hour:   9
			minute: 7
			second: 0
		}),
		time.new(time.Time{
			year:   year
			month:  8
			day:    23
			hour:   16
			minute: 45
			second: 0
		}),
	]
	for t in dates {
		if t.day == time.now().day {
			continue
		}
		assert t.clean() == '${t.smonth()} ${t.day} ${t.hour:02d}:${t.minute:02d}'
		assert t.clean12().len > 0
	}
}

fn test_clean_uses_a_time_only_form_for_today() {
	now := time.now()
	t := time.new(time.Time{
		year:   now.year
		month:  now.month
		day:    now.day
		hour:   1
		minute: 2
		second: 0
	})
	assert t.clean() == '01:02'
	assert t.clean12() == '1:02 a.m.'
}

fn test_to_json_quotes_the_rfc3339_form() {
	t := fixed_instant()
	assert t.to_json() == '"' + t.format_rfc3339() + '"'
	assert t.to_json() == '"2001-02-03T04:05:06.000Z"'
}

fn test_from_json_number_accepts_a_unix_timestamp_string() {
	t := fixed_instant()
	mut decoded := time.Time{}
	decoded.from_json_number('${t.unix()}')!
	assert decoded.unix() == t.unix()
	assert decoded.unix_nano() == t.unix_nano()
}

fn test_from_json_string_accepts_rfc3339_and_a_unix_timestamp() {
	t := fixed_instant()

	mut decoded := time.Time{}
	decoded.from_json_string(t.format_rfc3339())!
	assert decoded.unix() == t.unix()
	assert decoded.unix_nano() == t.unix_nano()

	mut from_unix := time.Time{}
	from_unix.from_json_string('${t.unix()}')!
	assert from_unix.unix() == t.unix()

	mut bad := time.Time{}
	bad.from_json_string('not a time') or {
		assert err.msg() == 'Expected iso8601/rfc3339/unix time but got: not a time'
		return
	}
	assert false, 'from_json_string() should have rejected a non-time string'
}

fn test_from_json_string_rejects_the_quoted_form_that_to_json_produces() {
	// NOTE: `to_json()` returns the rfc3339 form wrapped in quotes, but
	// `from_json_string()` does not strip quotes, so the pair does not round
	// trip on its own. json2 hands the decoder the unquoted value, so this is
	// only reachable by calling the pair directly.
	t := fixed_instant()
	mut decoded := time.Time{}
	decoded.from_json_string(t.to_json()) or {
		assert err.msg() == 'Expected iso8601/rfc3339/unix time but got: ${t.to_json()}'
		return
	}
	assert false, 'from_json_string() should have rejected the quoted form'
}

fn test_stopwatch_restart_zeros_the_elapsed_time() {
	mut sw := time.new_stopwatch()
	time.sleep(10 * time.millisecond)
	before := sw.elapsed()
	sw.restart()
	after := sw.elapsed()
	// restart() re-reads the clock, so the new elapsed time cannot exceed the
	// one measured before it.
	assert after <= before
}

fn test_ticks_is_a_monotonic_millisecond_clock() {
	first := time.ticks()
	time.sleep(3 * time.millisecond)
	second := time.ticks()
	assert second > first
}

fn test_long_weekday_str_names_the_day() {
	assert fixed_instant().long_weekday_str() == 'Saturday'
	assert time.new(time.Time{
		year:  2024
		month: 2
		day:   29
	}).long_weekday_str() == 'Thursday'
}

fn test_duration_sys_milliseconds_truncates_to_whole_milliseconds() {
	assert time.Duration(2 * time.second).sys_milliseconds() == 2000
	assert time.Duration(3 * time.millisecond).sys_milliseconds() == 3
	// Below a whole millisecond truncates towards zero.
	assert time.Duration(999 * time.microsecond).sys_milliseconds() == 0
	// Durations meant as "wait for ever" are signalled with -1. The boundary
	// itself is still a normal timeout, so only strictly larger values map to -1.
	assert time.Duration(2147483647 * time.millisecond).sys_milliseconds() == 2147483647
	assert time.Duration(2147483648 * time.millisecond).sys_milliseconds() == -1
	// A negative timeout is reported as 0, matching Unix poll() semantics.
	assert time.Duration(-1 * time.second).sys_milliseconds() == 0
}
