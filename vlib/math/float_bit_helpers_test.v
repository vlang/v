import math

const f32_samples = [f32(1.0), f32(-2.5), f32(0.0), f32(-0.0), f32(math.inf(1)), f32(math.inf(-1)),
	f32(3.14159), f32(1e-30), f32(1e30)]

const f64_samples = [f64(1.0), f64(-2.5), f64(0.0), f64(-0.0), f64(math.inf(1)), f64(math.inf(-1)),
	f64(3.14159), f64(1e-300), f64(1e300)]

fn test_f32_bits_known_patterns() {
	assert math.f32_bits(f32(1.0)) == u32(0x3f800000)
	assert math.f32_bits(f32(-2.5)) == u32(0xc0200000)
	assert math.f32_bits(f32(0.0)) == u32(0x00000000)
	assert math.f32_bits(f32(-0.0)) == u32(0x80000000)
	assert math.f32_bits(f32(math.inf(1))) == u32(0x7f800000)
	assert math.f32_bits(f32(math.inf(-1))) == u32(0xff800000)
}

fn test_f32_from_bits_known_patterns() {
	assert math.f32_from_bits(u32(0x3f800000)) == f32(1.0)
	assert math.f32_from_bits(u32(0xc0200000)) == f32(-2.5)
	assert math.f32_from_bits(u32(0x00000000)) == f32(0.0)
	assert math.f32_from_bits(u32(0x7f800000)) == math.inf(1)
	assert math.f32_from_bits(u32(0xff800000)) == math.inf(-1)
}

fn test_f32_bits_round_trip() {
	for x in f32_samples {
		assert math.f32_from_bits(math.f32_bits(x)) == x
		assert math.f32_bits(math.f32_from_bits(math.f32_bits(x))) == math.f32_bits(x)
	}
}

// NaN has many bit patterns, so f32_from_bits(f32_bits(nan)) is checked on the
// pattern rather than by ==, which is false for NaN.
fn test_f32_nan_pattern_round_trip() {
	nan_pattern := math.f32_bits(f32(math.nan()))
	nan_value := math.f32_from_bits(nan_pattern)
	assert u32(nan_pattern) & u32(0x7f800000) == u32(0x7f800000)
	assert u32(nan_pattern) & u32(0x007fffff) != 0
	assert nan_value != nan_value
	assert math.f32_bits(nan_value) == nan_pattern
}

fn test_f64_bits_known_patterns() {
	assert math.f64_bits(1.0) == u64(0x3ff0000000000000)
	assert math.f64_bits(-2.5) == u64(0xc004000000000000)
	assert math.f64_bits(0.0) == u64(0x0000000000000000)
	assert math.f64_bits(math.inf(1)) == u64(0x7ff0000000000000)
	assert math.f64_bits(math.inf(-1)) == u64(0xfff0000000000000)
	assert math.f64_bits(math.nan()) == u64(0x7ff8000000000001)
}

fn test_f64_from_bits_known_patterns() {
	assert math.f64_from_bits(u64(0x3ff0000000000000)) == 1.0
	assert math.f64_from_bits(u64(0xc004000000000000)) == -2.5
	assert math.f64_from_bits(u64(0x7ff0000000000000)) == math.inf(1)
	assert math.f64_from_bits(u64(0xfff0000000000000)) == math.inf(-1)
}

fn test_f64_bits_round_trip() {
	for x in f64_samples {
		assert math.f64_from_bits(math.f64_bits(x)) == x
		assert math.f64_bits(math.f64_from_bits(math.f64_bits(x))) == math.f64_bits(x)
	}
}

fn test_f64_nan_pattern_round_trip() {
	nan_pattern := math.f64_bits(math.nan())
	nan_value := math.f64_from_bits(nan_pattern)
	assert nan_pattern & u64(0x7ff0000000000000) == u64(0x7ff0000000000000)
	assert nan_pattern & u64(0x000fffffffffffff) != 0
	assert nan_value != nan_value
	assert math.f64_bits(nan_value) == nan_pattern
}

fn test_get_high_word() {
	assert math.get_high_word(1.0) == u32(0x3ff00000)
	assert math.get_high_word(-1.0) == u32(0xbff00000)
	assert math.get_high_word(0.0) == u32(0x00000000)
	assert math.get_high_word(math.inf(1)) == u32(0x7ff00000)
	assert math.get_high_word(2.0) == u32(0x40000000)
	for x in f64_samples {
		assert math.get_high_word(x) == u32(math.f64_bits(x) >> 32)
	}
}

fn test_with_set_low_word() {
	assert math.f64_bits(math.with_set_low_word(1.0, 0)) == u64(0x3ff0000000000000)
	assert math.f64_bits(math.with_set_low_word(1.0, 0xffffffff)) == u64(0x3ff00000ffffffff)
	assert math.with_set_low_word(1.0, 0) == 1.0
	// only the low 32 bits are replaced
	for x in f64_samples {
		assert math.get_high_word(math.with_set_low_word(x, 0)) == math.get_high_word(x)
		assert math.with_set_low_word(x, u32(math.f64_bits(x))) == x
	}
}

fn test_with_set_high_word() {
	assert math.with_set_high_word(0.0, u32(0x3ff00000)) == 1.0
	assert math.with_set_high_word(0.0, u32(0x40000000)) == 2.0
	assert math.with_set_high_word(1.0, u32(0x80000000)) == -0.0
	assert math.f64_bits(math.with_set_high_word(1.0, u32(0x80000000))) == u64(0x8000000000000000)
	// only the high 32 bits are replaced; the low word survives untouched
	for x in f64_samples {
		hi := math.get_high_word(x)
		assert math.with_set_high_word(x, hi) == x
		assert u32(math.f64_bits(math.with_set_high_word(x, u32(0x7ff00000)))) == u32(math.f64_bits(x))
	}
}
