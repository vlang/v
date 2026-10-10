module seed

fn distinct_count[T](samples int, generator fn () T) int {
	mut seen := map[T]bool{}
	for _ in 0 .. samples {
		seen[generator()] = true
	}
	return seen.len
}

fn test_time_seed_array_returns_the_requested_count() {
	for count in [0, 1, 2, 5, 16] {
		data := time_seed_array(count)
		assert data.len == count, 'asked for ${count}, got ${data.len}'
	}
}

fn test_time_seed_array_advances_between_calls() {
	mut seen := map[u32]bool{}
	for _ in 0 .. 128 {
		for value in time_seed_array(2) {
			seen[value] = true
		}
	}
	assert seen.len >= 8, '128 calls produced only ${seen.len} distinct seeds'
}

// The seed comes from `time.sys_mono_now()` through the LCG, so successive
// calls must not all collapse onto one value.
fn test_time_seed_32_advances_between_calls() {
	distinct := distinct_count(128, time_seed_32)
	assert distinct >= 8, '128 calls produced only ${distinct} distinct seeds'
}

fn test_time_seed_64_advances_between_calls() {
	distinct := distinct_count(128, time_seed_64)
	assert distinct >= 8, '128 calls produced only ${distinct} distinct seeds'
}

// `time_seed_64` composes two array entries as `lower | upper << 32`, so both
// halves can be read back out of it.
fn test_time_seed_64_is_composed_of_two_u32_halves() {
	value := time_seed_64()
	lower := u32(value & 0xffff_ffff)
	upper := u32(value >> 32)
	assert u64(lower) | (u64(upper) << 32) == value
}
