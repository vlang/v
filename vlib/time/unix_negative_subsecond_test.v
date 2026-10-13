import time

// A timestamp before 1970 with a sub-second part belongs to the second below it:
// the second count is rounded down, and the sub-second part is in 0 .. 999_999_999 .

struct FixedPoint {
	value      i64    // given to unix_milli, unix_micro or unix_nano
	unix       i64    // expected seconds since 1970-01-01
	nanosecond int    // expected sub-second part
	text       string // expected format_ss_nano()
}

const milli_points = [
	FixedPoint{-1, -1, 999_000_000, '1969-12-31 23:59:59.999000000'},
	FixedPoint{-999, -1, 1_000_000, '1969-12-31 23:59:59.001000000'},
	FixedPoint{-1_000, -1, 0, '1969-12-31 23:59:59.000000000'},
	FixedPoint{-1_001, -2, 999_000_000, '1969-12-31 23:59:58.999000000'},
	FixedPoint{-1_500, -2, 500_000_000, '1969-12-31 23:59:58.500000000'},
	FixedPoint{-59_999, -60, 1_000_000, '1969-12-31 23:59:00.001000000'},
	FixedPoint{-60_000, -60, 0, '1969-12-31 23:59:00.000000000'},
	FixedPoint{-60_001, -61, 999_000_000, '1969-12-31 23:58:59.999000000'},
	FixedPoint{-86_399_999, -86_400, 1_000_000, '1969-12-31 00:00:00.001000000'},
	FixedPoint{-86_400_000, -86_400, 0, '1969-12-31 00:00:00.000000000'},
	FixedPoint{-86_400_001, -86_401, 999_000_000, '1969-12-30 23:59:59.999000000'},
	FixedPoint{0, 0, 0, '1970-01-01 00:00:00.000000000'},
	FixedPoint{1, 0, 1_000_000, '1970-01-01 00:00:00.001000000'},
	FixedPoint{999, 0, 999_000_000, '1970-01-01 00:00:00.999000000'},
	FixedPoint{1_000, 1, 0, '1970-01-01 00:00:01.000000000'},
	FixedPoint{1_001, 1, 1_000_000, '1970-01-01 00:00:01.001000000'},
	FixedPoint{1_500, 1, 500_000_000, '1970-01-01 00:00:01.500000000'},
	FixedPoint{59_999, 59, 999_000_000, '1970-01-01 00:00:59.999000000'},
	FixedPoint{60_000, 60, 0, '1970-01-01 00:01:00.000000000'},
	FixedPoint{60_001, 60, 1_000_000, '1970-01-01 00:01:00.001000000'},
	FixedPoint{86_399_999, 86_399, 999_000_000, '1970-01-01 23:59:59.999000000'},
	FixedPoint{86_400_000, 86_400, 0, '1970-01-02 00:00:00.000000000'},
	FixedPoint{86_400_001, 86_400, 1_000_000, '1970-01-02 00:00:00.001000000'},
]

const micro_points = [
	FixedPoint{-1, -1, 999_999_000, '1969-12-31 23:59:59.999999000'},
	FixedPoint{-999, -1, 999_001_000, '1969-12-31 23:59:59.999001000'},
	FixedPoint{-1_000, -1, 999_000_000, '1969-12-31 23:59:59.999000000'},
	FixedPoint{-1_001, -1, 998_999_000, '1969-12-31 23:59:59.998999000'},
	FixedPoint{-1_500, -1, 998_500_000, '1969-12-31 23:59:59.998500000'},
	FixedPoint{-999_999, -1, 1_000, '1969-12-31 23:59:59.000001000'},
	FixedPoint{-1_000_000, -1, 0, '1969-12-31 23:59:59.000000000'},
	FixedPoint{-1_000_001, -2, 999_999_000, '1969-12-31 23:59:58.999999000'},
	FixedPoint{-59_999_999, -60, 1_000, '1969-12-31 23:59:00.000001000'},
	FixedPoint{-60_000_000, -60, 0, '1969-12-31 23:59:00.000000000'},
	FixedPoint{-60_000_001, -61, 999_999_000, '1969-12-31 23:58:59.999999000'},
	FixedPoint{-86_399_999_999, -86_400, 1_000, '1969-12-31 00:00:00.000001000'},
	FixedPoint{-86_400_000_000, -86_400, 0, '1969-12-31 00:00:00.000000000'},
	FixedPoint{-86_400_000_001, -86_401, 999_999_000, '1969-12-30 23:59:59.999999000'},
	FixedPoint{1, 0, 1_000, '1970-01-01 00:00:00.000001000'},
	FixedPoint{999, 0, 999_000, '1970-01-01 00:00:00.000999000'},
	FixedPoint{1_000, 0, 1_000_000, '1970-01-01 00:00:00.001000000'},
	FixedPoint{1_001, 0, 1_001_000, '1970-01-01 00:00:00.001001000'},
	FixedPoint{1_500, 0, 1_500_000, '1970-01-01 00:00:00.001500000'},
	FixedPoint{999_999, 0, 999_999_000, '1970-01-01 00:00:00.999999000'},
	FixedPoint{1_000_000, 1, 0, '1970-01-01 00:00:01.000000000'},
	FixedPoint{1_000_001, 1, 1_000, '1970-01-01 00:00:01.000001000'},
	FixedPoint{59_999_999, 59, 999_999_000, '1970-01-01 00:00:59.999999000'},
	FixedPoint{60_000_000, 60, 0, '1970-01-01 00:01:00.000000000'},
	FixedPoint{60_000_001, 60, 1_000, '1970-01-01 00:01:00.000001000'},
	FixedPoint{86_399_999_999, 86_399, 999_999_000, '1970-01-01 23:59:59.999999000'},
	FixedPoint{86_400_000_000, 86_400, 0, '1970-01-02 00:00:00.000000000'},
	FixedPoint{86_400_000_001, 86_400, 1_000, '1970-01-02 00:00:00.000001000'},
]

const nano_points = [
	FixedPoint{-1, -1, 999_999_999, '1969-12-31 23:59:59.999999999'},
	FixedPoint{-999, -1, 999_999_001, '1969-12-31 23:59:59.999999001'},
	FixedPoint{-1_000, -1, 999_999_000, '1969-12-31 23:59:59.999999000'},
	FixedPoint{-1_001, -1, 999_998_999, '1969-12-31 23:59:59.999998999'},
	FixedPoint{-1_500, -1, 999_998_500, '1969-12-31 23:59:59.999998500'},
	FixedPoint{-999_999_999, -1, 1, '1969-12-31 23:59:59.000000001'},
	FixedPoint{-1_000_000_000, -1, 0, '1969-12-31 23:59:59.000000000'},
	FixedPoint{-1_000_000_001, -2, 999_999_999, '1969-12-31 23:59:58.999999999'},
	FixedPoint{-59_999_999_999, -60, 1, '1969-12-31 23:59:00.000000001'},
	FixedPoint{-60_000_000_000, -60, 0, '1969-12-31 23:59:00.000000000'},
	FixedPoint{-60_000_000_001, -61, 999_999_999, '1969-12-31 23:58:59.999999999'},
	FixedPoint{-86_399_999_999_999, -86_400, 1, '1969-12-31 00:00:00.000000001'},
	FixedPoint{-86_400_000_000_000, -86_400, 0, '1969-12-31 00:00:00.000000000'},
	FixedPoint{-86_400_000_000_001, -86_401, 999_999_999, '1969-12-30 23:59:59.999999999'},
	FixedPoint{1, 0, 1, '1970-01-01 00:00:00.000000001'},
	FixedPoint{999, 0, 999, '1970-01-01 00:00:00.000000999'},
	FixedPoint{1_000, 0, 1_000, '1970-01-01 00:00:00.000001000'},
	FixedPoint{1_001, 0, 1_001, '1970-01-01 00:00:00.000001001'},
	FixedPoint{1_500, 0, 1_500, '1970-01-01 00:00:00.000001500'},
	FixedPoint{999_999_999, 0, 999_999_999, '1970-01-01 00:00:00.999999999'},
	FixedPoint{1_000_000_000, 1, 0, '1970-01-01 00:00:01.000000000'},
	FixedPoint{1_000_000_001, 1, 1, '1970-01-01 00:00:01.000000001'},
	FixedPoint{59_999_999_999, 59, 999_999_999, '1970-01-01 00:00:59.999999999'},
	FixedPoint{60_000_000_000, 60, 0, '1970-01-01 00:01:00.000000000'},
	FixedPoint{60_000_000_001, 60, 1, '1970-01-01 00:01:00.000000001'},
	FixedPoint{86_399_999_999_999, 86_399, 999_999_999, '1970-01-01 23:59:59.999999999'},
	FixedPoint{86_400_000_000_000, 86_400, 0, '1970-01-02 00:00:00.000000000'},
	FixedPoint{86_400_000_000_001, 86_400, 1, '1970-01-02 00:00:00.000000001'},
	// the first and the last instant that a nanosecond timestamp can hold
	FixedPoint{min_i64, -9_223_372_037, 145_224_192, '1677-09-21 00:12:43.145224192'},
	FixedPoint{max_i64, 9_223_372_036, 854_775_807, '2262-04-11 23:47:16.854775807'},
]

enum Resolution {
	milli
	micro
	nano
}

const resolutions = [Resolution.milli, .micro, .nano]

fn (r Resolution) per_second() i64 {
	return match r {
		.milli { i64(1_000) }
		.micro { i64(1_000_000) }
		.nano { i64(1_000_000_000) }
	}
}

fn (r Resolution) time_of(timestamp i64) time.Time {
	return match r {
		.milli { time.unix_milli(timestamp) }
		.micro { time.unix_micro(timestamp) }
		.nano { time.unix_nano(timestamp) }
	}
}

fn (r Resolution) timestamp_of(t time.Time) i64 {
	return match r {
		.milli { t.unix_milli() }
		.micro { t.unix_micro() }
		.nano { t.unix_nano() }
	}
}

fn (r Resolution) check(points []FixedPoint) {
	for p in points {
		t := r.time_of(p.value)
		assert t.unix() == p.unix, '${r} ${p.value}'
		assert t.nanosecond == p.nanosecond, '${r} ${p.value}'
		assert t.format_ss_nano() == p.text, '${r} ${p.value}'
		assert r.timestamp_of(t) == p.value, '${r} ${p.value}'
	}
}

fn test_unix_milli_of_minus_one_is_the_last_millisecond_of_1969() {
	t := time.unix_milli(-1)
	assert t.year == 1969
	assert t.month == 12
	assert t.day == 31
	assert t.hour == 23
	assert t.minute == 59
	assert t.second == 59
	assert t.nanosecond == 999_000_000
	assert t.unix() == -1
	assert t.unix_milli() == -1
	assert t.unix_micro() == -1_000
	assert t.unix_nano() == -1_000_000
	assert t.format_ss() == '1969-12-31 23:59:59'
	assert t.format_ss_milli() == '1969-12-31 23:59:59.999'
	assert t.format_ss_micro() == '1969-12-31 23:59:59.999000'
	assert t.format_ss_nano() == '1969-12-31 23:59:59.999000000'
	assert t.format_rfc3339() == '1969-12-31T23:59:59.999Z'
	assert t.format_rfc3339_micro() == '1969-12-31T23:59:59.999000Z'
	assert t.format_rfc3339_nano() == '1969-12-31T23:59:59.999000000Z'
	assert t.get_fmt_time_str(.hhmmss24_milli) == '23:59:59.999'
}

fn test_unix_milli_fixed_points() {
	Resolution.milli.check(milli_points)
}

fn test_unix_micro_fixed_points() {
	Resolution.micro.check(micro_points)
}

fn test_unix_nano_fixed_points() {
	Resolution.nano.check(nano_points)
}

fn test_a_coarser_resolution_of_a_negative_timestamp_is_rounded_down() {
	assert time.unix_milli(-1).unix() == -1
	assert time.unix_milli(-1_001).unix() == -2
	assert time.unix_micro(-1).unix() == -1
	assert time.unix_micro(-1).unix_milli() == -1
	assert time.unix_micro(-1_000).unix_milli() == -1
	assert time.unix_micro(-1_001).unix_milli() == -2
	assert time.unix_nano(-1).unix() == -1
	assert time.unix_nano(-1).unix_milli() == -1
	assert time.unix_nano(-1).unix_micro() == -1
	assert time.unix_nano(-1_000).unix_micro() == -1
	assert time.unix_nano(-1_001).unix_micro() == -2
}

// timestamp_spread returns every timestamp of -3000 .. 3000, the ones around the second, minute,
// hour, day and year boundaries on both sides of 1970, and a pseudo-random spread over 5 years.
fn timestamp_spread(per_second i64) []i64 {
	mut values := []i64{cap: 50_000}
	for i in 0 .. 6_001 {
		values << i64(i) - 3_000
	}
	for seconds in [i64(1), 2, 59, 60, 61, 3_599, 3_600, 86_399, 86_400, 86_401, 31_536_000] {
		for delta in [i64(-1), 0, 1] {
			values << seconds * per_second + delta
			values << -seconds * per_second + delta
		}
	}
	limit := u64(5 * 365 * 86_400) * u64(per_second)
	mut state := u64(0x9e3779b97f4a7c15)
	for _ in 0 .. 20_000 {
		state = state * u64(6364136223846793005) + u64(1442695040888963407)
		value := i64((state >> 11) % limit)
		values << value
		values << -value
	}
	return values
}

fn test_round_trip_and_floor_over_a_spread_of_timestamps() {
	for r in resolutions {
		per_second := r.per_second()
		to_nanoseconds := 1_000_000_000 / per_second
		for value in timestamp_spread(per_second) {
			t := r.time_of(value)
			assert r.timestamp_of(t) == value, '${r} ${value}'
			// the second count is the floor: 0 <= value - seconds * per_second < per_second
			sub_second := value - t.unix() * per_second
			assert sub_second >= 0 && sub_second < per_second, '${r} ${value}'
			assert i64(t.nanosecond) == sub_second * to_nanoseconds, '${r} ${value}'
			// Time.add reaches the same instant from the epoch, with the same fields
			added := time.unix(0).add(value * to_nanoseconds * time.nanosecond)
			assert t == added, '${r} ${value}'
			assert t.format_ss_nano() == added.format_ss_nano(), '${r} ${value}'
		}
	}
}

fn test_round_trip_at_the_limits_of_i64() {
	for value in [min_i64, min_i64 + 1, min_i64 + 999, max_i64 - 999, max_i64 - 1, max_i64] {
		for r in resolutions {
			t := r.time_of(value)
			assert t.nanosecond >= 0 && t.nanosecond < 1_000_000_000, '${r} ${value}'
			assert r.timestamp_of(t) == value, '${r} ${value}'
		}
	}
}

struct Carry {
	epoch      i64
	sub_second int    // the microsecond or nanosecond argument
	unix       i64    // expected seconds since 1970-01-01
	nanosecond int    // expected sub-second part
	text       string // expected format_ss_nano()
}

fn test_unix_nanosecond_carries_a_nanosecond_outside_of_a_second_into_the_seconds() {
	for c in [
		Carry{0, -1, -1, 999_999_999, '1969-12-31 23:59:59.999999999'},
		Carry{0, -1_000_000_000, -1, 0, '1969-12-31 23:59:59.000000000'},
		Carry{0, -1_000_000_001, -2, 999_999_999, '1969-12-31 23:59:58.999999999'},
		Carry{0, 1_000_000_000, 1, 0, '1970-01-01 00:00:01.000000000'},
		Carry{0, 1_999_999_999, 1, 999_999_999, '1970-01-01 00:00:01.999999999'},
		Carry{10, -1, 9, 999_999_999, '1970-01-01 00:00:09.999999999'},
		Carry{-10, -1, -11, 999_999_999, '1969-12-31 23:59:49.999999999'},
		Carry{-10, 2_000_000_001, -8, 1, '1969-12-31 23:59:52.000000001'},
		Carry{86_399, 1_000_000_000, 86_400, 0, '1970-01-02 00:00:00.000000000'},
		Carry{-86_400, -1, -86_401, 999_999_999, '1969-12-30 23:59:59.999999999'},
		// a nanosecond inside of a second is kept as it is
		Carry{0, 0, 0, 0, '1970-01-01 00:00:00.000000000'},
		Carry{0, 999_999_999, 0, 999_999_999, '1970-01-01 00:00:00.999999999'},
		Carry{-1, 999_999_999, -1, 999_999_999, '1969-12-31 23:59:59.999999999'},
		Carry{-86_400, 1, -86_400, 1, '1969-12-31 00:00:00.000000001'},
	] {
		t := time.unix_nanosecond(c.epoch, c.sub_second)
		assert t.unix() == c.unix, '${c.epoch} ${c.sub_second}'
		assert t.nanosecond == c.nanosecond, '${c.epoch} ${c.sub_second}'
		assert t.format_ss_nano() == c.text, '${c.epoch} ${c.sub_second}'
	}
	assert time.unix_nanosecond(0, -1) == time.unix_nano(-1)
	assert time.unix_nanosecond(5, -1_500_000_000) == time.unix_milli(3_500)
}

fn test_unix_microsecond_carries_a_microsecond_outside_of_a_second_into_the_seconds() {
	for c in [
		Carry{0, -1, -1, 999_999_000, '1969-12-31 23:59:59.999999000'},
		Carry{0, -1_000_000, -1, 0, '1969-12-31 23:59:59.000000000'},
		Carry{0, -1_000_001, -2, 999_999_000, '1969-12-31 23:59:58.999999000'},
		Carry{0, 1_000_000, 1, 0, '1970-01-01 00:00:01.000000000'},
		Carry{0, 1_999_999, 1, 999_999_000, '1970-01-01 00:00:01.999999000'},
		Carry{10, -1, 9, 999_999_000, '1970-01-01 00:00:09.999999000'},
		Carry{-10, -1, -11, 999_999_000, '1969-12-31 23:59:49.999999000'},
		// more microseconds than fit a 32 bit count of nanoseconds
		Carry{0, 2_147_484, 2, 147_484_000, '1970-01-01 00:00:02.147484000'},
		Carry{0, 2_100_000_000, 2_100, 0, '1970-01-01 00:35:00.000000000'},
		Carry{0, -2_100_000_000, -2_100, 0, '1969-12-31 23:25:00.000000000'},
		// a microsecond inside of a second is kept as it is
		Carry{0, 0, 0, 0, '1970-01-01 00:00:00.000000000'},
		Carry{0, 999_999, 0, 999_999_000, '1970-01-01 00:00:00.999999000'},
		Carry{-1, 999_999, -1, 999_999_000, '1969-12-31 23:59:59.999999000'},
	] {
		t := time.unix_microsecond(c.epoch, c.sub_second)
		assert t.unix() == c.unix, '${c.epoch} ${c.sub_second}'
		assert t.nanosecond == c.nanosecond, '${c.epoch} ${c.sub_second}'
		assert t.format_ss_nano() == c.text, '${c.epoch} ${c.sub_second}'
	}
	assert time.unix_microsecond(0, -1) == time.unix_micro(-1)
}

fn test_unix_nanosecond_to_local_carries_a_nanosecond_outside_of_a_second() {
	// a fixed +01:00 location, which does not need the time zone database of the system
	parsed := time.parse_rfc3339('1970-01-01T00:00:00+01:00')!
	loc := parsed.location() or { panic('missing location') }
	before := loc.unix_nanosecond_to_local(0, -1)!
	assert before.unix() == -1
	assert before.nanosecond == 999_999_999
	assert before.format_ss_nano() == '1970-01-01 00:59:59.999999999'
	assert before.format_rfc3339_nano() == '1969-12-31T23:59:59.999999999Z'
	assert before == time.unix_nano(-1).in(loc)!
	after := loc.unix_nanosecond_to_local(-3_601, 1_500_000_000)!
	assert after.unix() == -3_600
	assert after.nanosecond == 500_000_000
	assert after.format_ss_nano() == '1970-01-01 00:00:00.500000000'
	in_range := loc.unix_nanosecond_to_local(-1, 999_999_999)!
	assert in_range.unix() == -1
	assert in_range.nanosecond == 999_999_999
	assert in_range == before
}
