module time

fn test_unix_microsecond_at_the_epoch() {
	epoch := unix_microsecond(0, 0)
	assert epoch.str() == '1970-01-01 00:00:00'
	assert epoch.unix() == 0
	assert epoch.nanosecond == 0
	assert unix_microsecond(0, 1).nanosecond == 1000
	assert unix_microsecond(0, 1).unix() == 0
}

fn test_unix_microsecond_scales_microseconds_into_nanoseconds() {
	assert unix_microsecond(0, 123456).nanosecond == 123456000
	assert unix_microsecond(0, 999999).nanosecond == 999999000
	assert unix_microsecond(1078058096, 0).nanosecond == 0
	assert unix_microsecond(1078058096, 1).nanosecond == 1000
	assert unix_microsecond(1078058096, 500000).nanosecond == 500000000
	assert unix_microsecond(4107542400, 500000).nanosecond == 500000000
}

fn test_unix_microsecond_keeps_the_epoch_component() {
	assert unix_microsecond(1078058096, 999999).unix() == 1078058096
	assert unix_microsecond(1564366499, 123456).unix() == 1564366499
	assert unix_microsecond(4107542400, 0).unix() == 4107542400
	assert unix_microsecond(1781531121, 42).unix() == 1781531121
}

fn test_unix_microsecond_renders_the_wall_clock() {
	assert unix_microsecond(1078058096, 999999).str() == '2004-02-29 12:34:56'
	assert unix_microsecond(1564366499, 123456).str() == '2019-07-29 02:14:59'
	assert unix_microsecond(4107542400, 500000).str() == '2100-03-01 00:00:00'
	assert unix_microsecond(0, 999999).str() == '1970-01-01 00:00:00'
	assert unix_microsecond(1, 0).str() == '1970-01-01 00:00:01'
}

fn test_unix_microsecond_goes_negative_before_the_epoch() {
	before_epoch := unix_microsecond(-1, 999999)
	assert before_epoch.str() == '1969-12-31 23:59:59'
	assert before_epoch.unix() == -1
	assert before_epoch.nanosecond == 999999000
	two_before := unix_microsecond(-2, 1)
	assert two_before.str() == '1969-12-31 23:59:58'
	assert two_before.unix() == -2
	assert two_before.nanosecond == 1000
}

// `unix_microsecond` splits the epoch and the fraction apart, while
// `unix_micro` takes a single packed microsecond count. They must agree.
fn test_unix_microsecond_agrees_with_unix_micro() {
	for unix_ts in [i64(0), 1, 999999, 1078058096, 1564366499, 1781531121, 4107542400] {
		for us in [0, 1, 123456, 500000, 999999] {
			split := unix_microsecond(unix_ts, us)
			packed := unix_micro(unix_ts * 1000000 + i64(us))
			assert split.str() == packed.str(), '${unix_ts}.${us}: ${split.str()} != ${packed.str()}'
			assert split.nanosecond == packed.nanosecond, '${unix_ts}.${us}: ${split.nanosecond} != ${packed.nanosecond}'
			assert split.unix() == packed.unix(), '${unix_ts}.${us}: ${split.unix()} != ${packed.unix()}'
		}
	}
}

// A sub-second component must never leak into the epoch, so every microsecond
// value has to land inside the same whole second.
fn test_unix_microsecond_never_crosses_a_second_boundary() {
	for us in [0, 1, 2, 500000, 999998, 999999] {
		assert unix_microsecond(1078058096, us).unix() == 1078058096
	}
	assert unix_microsecond(1078058096 + 1, 0).unix() == 1078058097
}
