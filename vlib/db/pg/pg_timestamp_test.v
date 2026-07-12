// vtest build: !windows && !musl?
module pg

import orm
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

fn test_bc_suffix_returns_clear_error() {
	pg_parse_timestamp('0001-01-01 00:00:00 BC') or {
		assert err.msg().contains('BC')
		return
	}
	assert false, 'expected an error for a BC timestamp value'
}

fn test_bc_suffix_with_offset_returns_clear_error() {
	pg_parse_timestamp('0044-03-15 12:00:00+00 BC') or {
		assert err.msg().contains('BC')
		return
	}
	assert false, 'expected an error for a BC TIMESTAMPTZ value'
}

fn test_garbage_seconds_suffix_is_rejected() {
	// `string.int()` would silently keep the `00` prefix; strict parsing must reject it.
	pg_parse_timestamp('2024-01-15 14:00:00 XY') or { return }
	assert false, 'expected an error for a trailing non-numeric suffix'
}

fn test_unix_timestamp_path_decodes_integer() {
	// Mirror the ORM fallback: a bare integer string is a Unix timestamp.
	p := val_to_primitive('1700000000', orm.time_)!
	t := p as time.Time
	assert t.unix() == 1700000000
}

fn test_infinity_via_val_to_primitive_errors() {
	// `infinity` has no date/time punctuation; it must still reach the parser and
	// error instead of decoding to the Unix epoch.
	val_to_primitive('infinity', orm.time_) or {
		assert err.msg().contains('infinity')
		return
	}
	assert false, 'expected an error decoding `infinity` via val_to_primitive'
}

fn test_year_out_of_range_returns_error_not_panic() {
	// PostgreSQL accepts years past 9999, which `time.new` cannot represent. The
	// decoder must return an error rather than panicking.
	pg_parse_timestamp('10000-01-01 00:00:00') or {
		assert err.msg().contains('out of range')
		return
	}
	assert false, "expected an error for a year beyond V's supported range"
}

fn test_out_of_range_via_val_to_primitive_errors() {
	val_to_primitive('10000-01-01 00:00:00', orm.time_) or {
		assert err.msg().contains('out of range')
		return
	}
	assert false, 'expected an error decoding an out-of-range timestamp'
}

fn test_offset_pushes_year_out_of_range_returns_error() {
	// A valid local timestamp that normalizes to year 10000 in UTC must still error,
	// not silently bypass the range guard via the offset shift.
	pg_parse_timestamp('9999-12-31 23:30:00-01') or {
		assert err.msg().contains('out of range')
		return
	}
	assert false, 'expected an error when the UTC offset pushes the year out of range'
}

fn test_offset_within_range_still_decodes() {
	// A near-boundary value that stays in range after normalization must succeed.
	t := pg_parse_timestamp('9999-12-31 22:30:00-01')!
	assert t.year == 9999
	assert t.month == 12
	assert t.day == 31
	assert t.hour == 23
	assert t.minute == 30
}

fn test_offset_normalizes_into_bc_returns_error() {
	// `0001-01-01 00:30:00+01` normalizes to `0000-12-31 23:30:00` (year 0 == 1 BC),
	// which is unrepresentable and must be rejected rather than silently accepted.
	pg_parse_timestamp('0001-01-01 00:30:00+01') or {
		assert err.msg().contains('BC')
		return
	}
	assert false, 'expected an error when the UTC offset normalizes into a BC/year-0 date'
}

fn test_lower_boundary_offset_within_range_still_decodes() {
	// `0001-01-01 01:30:00+01` normalizes to `0001-01-01 00:30:00` (year 1), still valid.
	t := pg_parse_timestamp('0001-01-01 01:30:00+01')!
	assert t.year == 1
	assert t.month == 1
	assert t.day == 1
	assert t.hour == 0
	assert t.minute == 30
}

fn test_year_zero_literal_returns_bc_error() {
	// A direct year-0 value (no ` BC` suffix) is still unrepresentable.
	pg_parse_timestamp('0000-01-01 00:00:00') or {
		assert err.msg().contains('BC')
		return
	}
	assert false, 'expected a BC/year-0 error for a literal year-0 timestamp'
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
