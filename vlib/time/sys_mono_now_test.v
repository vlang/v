module time

const one_second_ns = i64(second)

fn test_sys_mono_now_never_goes_backwards() {
	mut previous := sys_mono_now()
	for _ in 0 .. 1000 {
		current := sys_mono_now()
		assert current >= previous, 'went from ${previous} back to ${current}'
		previous = current
	}
}

fn test_sys_mono_now_advances_over_a_known_nap() {
	nap := 50 * millisecond
	before := sys_mono_now()
	sleep(nap)
	elapsed := i64(sys_mono_now() - before)
	// Windows sleep granularity and scheduler jitter both inflate the wait, so
	// only the lower bound is tight.
	assert elapsed >= i64(nap) / 2, 'a 50 ms nap measured ${elapsed} ns'
	assert elapsed <= 10 * i64(nap), 'a 50 ms nap measured ${elapsed} ns'
}

fn test_sys_mono_now_counts_nanoseconds() {
	nap := 100 * millisecond
	before_wall := now().unix_nano()
	before_mono := sys_mono_now()
	sleep(nap)
	elapsed_wall := now().unix_nano() - before_wall
	elapsed_mono := i64(sys_mono_now() - before_mono)
	assert elapsed_mono >= i64(nap) / 2, 'a 100 ms nap measured ${elapsed_mono} ns'
	// Two independent clocks reading the same nap must land within a few
	// seconds of each other; a ns counter read as us, or a unit error in the
	// magnitude, lands orders of magnitude away.
	assert elapsed_mono > i64(nap) / 10 && elapsed_mono < 10 * i64(nap)
	assert abs_ns(elapsed_wall - elapsed_mono) < 5 * one_second_ns
}

fn test_sys_mono_now_measures_short_spans() {
	before := sys_mono_now()
	mut sum := u64(0)
	for i in 0 .. 1000 {
		sum += u64(i)
	}
	elapsed := i64(sys_mono_now() - before)
	assert sum == 499500
	// Non-trivial work doesn't take seconds, and a clock in the wrong unit
	// reports zero.
	assert elapsed >= 0
	assert elapsed < one_second_ns
}

fn test_sys_mono_now_survives_a_long_running_loop() {
	before := sys_mono_now()
	mut acc := u64(0)
	for i in 0 .. 200 {
		for j in 0 .. 5000 {
			acc += u64(i * 5000 + j)
		}
	}
	elapsed := i64(sys_mono_now() - before)
	assert acc == 499999500000
	assert elapsed < 30 * one_second_ns
}

fn abs_ns(x i64) i64 {
	return if x < 0 { -x } else { x }
}
