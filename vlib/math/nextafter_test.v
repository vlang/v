module math

// nextafter(x, x) = x, and moving towards a greater y always increases the value by
// exactly one representable step (one ulp), which for 1.0 is 2**-52.
fn test_nextafter_steps_one_ulp_upwards() {
	assert nextafter(1.0, 1.0) == 1.0
	up := nextafter(1.0, 2.0)
	assert up == 1.0000000000000002
	assert up - 1.0 == pow(2.0, -52.0)
	assert up > 1.0
	assert nextafter(1.5, 2.0) - 1.5 == pow(2.0, -52.0)
	assert nextafter(2.0, 3.0) - 2.0 == pow(2.0, -51.0)
	assert nextafter(0.5, 1.0) - 0.5 == pow(2.0, -53.0)
}

fn test_nextafter_steps_one_ulp_downwards() {
	down := nextafter(1.0, 0.0)
	assert down == 0.9999999999999999
	assert 1.0 - down == pow(2.0, -53.0)
	assert down < 1.0
	// A step below 2.0 stays inside the [1, 2) binade, where the spacing is still 2**-52,
	// whereas a step above 2.0 lands in [2, 4) where it doubles to 2**-51.
	assert 2.0 - nextafter(2.0, 0.0) == pow(2.0, -52.0)
	assert 0.5 - nextafter(0.5, 0.0) == pow(2.0, -54.0)
}

// A step towards y and a step straight back must be exact inverses, so a round trip
// through the neighbouring value returns the starting value.
fn test_nextafter_round_trips() {
	for x in [f64(1.0), 1.5, 2.0, 3.75, 0.5, 1024.0, 1e-300] {
		assert nextafter(nextafter(x, inf(1)), x) == x
		assert nextafter(nextafter(x, inf(-1)), x) == x
	}
	// Each further step moves strictly further away from x.
	mut v := 1.0
	for _ in 0 .. 5 {
		prev := v
		v = nextafter(v, 2.0)
		assert v > prev
	}
	// Five steps up take five steps to come back.
	assert nextafter(v, 1.0) < v
	mut back := v
	for _ in 0 .. 5 {
		back = nextafter(back, 1.0)
	}
	assert back == 1.0
}

// The direction is chosen by the target, not by the sign of x, so a negative x moves
// away from zero when y is more negative.
fn test_nextafter_direction_follows_the_target() {
	assert nextafter(-1.0, 0.0) == -0.9999999999999999
	assert nextafter(-1.0, 0.0) > -1.0
	assert nextafter(-1.0, -2.0) == -1.0000000000000002
	assert nextafter(-1.0, -2.0) < -1.0
	assert nextafter(1.0, inf(-1)) < 1.0
	assert nextafter(1.0, inf(1)) > 1.0
	assert nextafter(-1.0, inf(1)) > -1.0
}

fn test_nextafter_special_cases() {
	assert is_nan(nextafter(nan(), 1.0))
	assert is_nan(nextafter(1.0, nan()))
	assert nextafter(inf(1), inf(1)) == inf(1)
	assert nextafter(inf(-1), inf(-1)) == inf(-1)
	// Stepping away from the largest finite value saturates at infinity.
	assert nextafter(max_f64, inf(1)) == inf(1)
	assert nextafter(inf(-1), 0.0) == -max_f64
	// 0.0 and -0.0 compare equal, so nextafter(0, -0) returns x unchanged.
	assert nextafter(0.0, -0.0) == 0.0
}

// Zero is not a normal value, so the first step away from it is a subnormal with the
// sign of the target.
fn test_nextafter_leaves_zero_into_the_subnormals() {
	pos := nextafter(0.0, 1.0)
	assert pos == 5e-324
	assert pos > 0.0
	assert !signbit(pos)
	neg := nextafter(0.0, -1.0)
	assert neg == -5e-324
	assert neg < 0.0
	assert signbit(neg)
	assert f64_bits(pos) == 1
	assert f64_bits(neg) == sign_mask | 1
	// Stepping back towards zero from the smallest subnormal yields a positive zero.
	back := nextafter(pos, -1.0)
	assert back == 0.0
	assert !signbit(back)
}

fn test_nextafter32_steps_one_ulp() {
	assert nextafter32(f32(1.0), f32(1.0)) == f32(1.0)
	up := nextafter32(f32(1.0), f32(2.0))
	assert up == f32(1.0000001)
	assert up > f32(1.0)
	assert u32(f32_bits(up)) == u32(f32_bits(f32(1.0))) + 1
	down := nextafter32(f32(1.0), f32(0.0))
	assert down == f32(0.99999994)
	assert down < f32(1.0)
	assert u32(f32_bits(f32(1.0))) - u32(f32_bits(down)) == u32(1)
}

fn test_nextafter32_special_cases() {
	assert is_nan(f64(nextafter32(f32(nan()), f32(1.0))))
	assert is_nan(f64(nextafter32(f32(1.0), f32(nan()))))
	assert nextafter32(f32(inf(1)), f32(inf(1))) == f32(inf(1))
	assert nextafter32(f32(inf(-1)), f32(inf(-1))) == f32(inf(-1))
	assert nextafter32(f32(max_f32), f32(inf(1))) == f32(inf(1))
	assert nextafter32(f32(inf(-1)), f32(0.0)) == f32(-max_f32)
	assert nextafter32(f32(0.0), f32(-0.0)) == f32(0.0)
}

fn test_nextafter32_leaves_zero_into_the_subnormals() {
	pos := nextafter32(f32(0.0), f32(1.0))
	assert pos == f32(1e-45)
	assert pos > f32(0.0)
	assert u32(f32_bits(pos)) == u32(1)
	neg := nextafter32(f32(0.0), f32(-1.0))
	assert u32(f32_bits(neg)) == u32(1) | u32(0x80000000)
	assert neg < f32(0.0)
	back := nextafter32(pos, f32(-1.0))
	assert back == f32(0.0)
	assert !signbit(f64(back))
}

fn test_nextafter32_round_trips() {
	for x in [f32(1.0), 1.5, 2.0, 3.75, 0.5, 1024.0] {
		assert nextafter32(nextafter32(x, f32(inf(1))), x) == x
		assert nextafter32(nextafter32(x, f32(inf(-1))), x) == x
	}
}

// The f32 and f64 variants must agree on the direction and on the subnormal entry.
fn test_nextafter_and_nextafter32_agree() {
	assert nextafter(1.0, 2.0) > 1.0
	assert nextafter32(f32(1.0), f32(2.0)) > f32(1.0)
	assert nextafter(1.0, 0.0) < 1.0
	assert nextafter32(f32(1.0), f32(0.0)) < f32(1.0)
	assert f64_bits(nextafter(0.0, 1.0)) == 1
	assert u32(f32_bits(nextafter32(f32(0.0), f32(1.0)))) == u32(1)
}
