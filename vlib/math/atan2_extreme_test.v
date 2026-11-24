module math

fn test_atan2_underflowed_quotient_retains_quadrant() {
	tiny := f64_from_bits(1)
	for x in [-2.0, -22.0, -max_f64] {
		assert atan2(tiny, x) == pi
		assert atan2(-tiny, x) == -pi
	}
	for x in [2.0, 22.0, max_f64] {
		assert atan2(tiny, x) == 0
		assert !signbit(atan2(tiny, x))
		assert atan2(-tiny, x) == 0
		assert signbit(atan2(-tiny, x))
	}
}
