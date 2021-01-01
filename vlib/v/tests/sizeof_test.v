import math

struct S1 {
	i voidptr
}

fn test_math_sizeof() {
	r := math.f32_from_bits(sizeof(int))
	assert r > 5.6e-45 && r < 5.7e-45
}

fn test_sizeof() {
	assert sizeof(`€`) == 2
	assert sizeof(S1) in [u32(4), 8]
}
