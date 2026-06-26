module pg

import time

// These tests exercise the PostgreSQL TIMESTAMP/TIMESTAMPTZ text decoder
// directly, so they do not require a running PostgreSQL server.

fn test_plain_timestamp() {
	t := pg_parse_timestamp('2024-01-15 14:00:00')!
	assert t.year == 2024
	assert t.month == 1
	assert t.day == 15
	assert t.hour == 14
	assert t.minute == 0
	assert t.second == 0
	assert t.nanosecond == 0
}

fn test_fractional_seconds_microsecond_precision() {
	t := pg_parse_timestamp('2024-01-15 14:00:00.123456')!
	assert t.hour == 14
	assert t.second == 0
	// 123456 microseconds preserved as nanoseconds
	assert t.nanosecond == 123456000
}

fn test_timestamptz_utc_offset() {
	// `+00` offset -> already UTC, value unchanged.
	t := pg_parse_timestamp('2024-01-15 13:00:00.123456+00')!
	assert t.year == 2024
	assert t.month == 1
	assert t.day == 15
	assert t.hour == 13
	assert t.minute == 0
	assert t.second == 0
	assert t.nanosecond == 123456000
}

fn test_timestamptz_positive_offset_normalized_to_utc() {
	// 14:00 at +01:00 -> 13:00 UTC
	t := pg_parse_timestamp('2024-01-15 14:00:00.123456+01:00')!
	assert t.hour == 13
	assert t.minute == 0
	assert t.nanosecond == 123456000
}

fn test_timestamptz_positive_short_offset() {
	// 14:00 at +02 -> 12:00 UTC
	t := pg_parse_timestamp('2024-01-15 14:00:00+02')!
	assert t.hour == 12
	assert t.minute == 0
}

fn test_timestamptz_offset_with_minutes() {
	// 14:00 at +02:30 -> 11:30 UTC
	t := pg_parse_timestamp('2024-01-15 14:00:00+02:30')!
	assert t.hour == 11
	assert t.minute == 30
}

fn test_timestamptz_negative_offset_normalized_to_utc() {
	// 14:00 at -05:00 -> 19:00 UTC
	t := pg_parse_timestamp('2024-01-15 14:00:00-05')!
	assert t.hour == 19
	assert t.minute == 0
}

fn test_timestamptz_negative_offset_with_minutes_day_rollover() {
	// 23:00 at -05:30 -> 04:30 UTC next day
	t := pg_parse_timestamp('2024-01-15 23:00:00-05:30')!
	assert t.day == 16
	assert t.hour == 4
	assert t.minute == 30
}

fn test_timestamptz_z_suffix() {
	t := pg_parse_timestamp('2024-01-15 14:00:00.5Z')!
	assert t.hour == 14
	assert t.nanosecond == 500000000
}

fn test_infinity_returns_clear_error() {
	pg_parse_timestamp('infinity') or {
		assert err.msg().contains('infinity')
		return
	}
	assert false, 'expected an error for the special value `infinity`'
}

fn test_negative_infinity_returns_clear_error() {
	pg_parse_timestamp('-infinity') or {
		assert err.msg().contains('infinity')
		return
	}
	assert false, 'expected an error for the special value `-infinity`'
}

fn test_issue_27556_example() {
	// The exact value from the issue: stored as '2024-01-15 14:00:00.123456+01:00'
	// with the session in UTC, PostgreSQL returns it as below.
	t := pg_parse_timestamp('2024-01-15 13:00:00.123456+00')!
	expected := time.new(
		year:       2024
		month:      1
		day:        15
		hour:       13
		minute:     0
		second:     0
		nanosecond: 123456000
		is_local:   false
	)
	assert t.unix() == expected.unix()
	assert t.nanosecond == expected.nanosecond
}
