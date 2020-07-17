import math

fn test_sizeof() {
	r := math.f32_from_bits(sizeof(int))
	assert r > 5.6e-45 && r < 5.7e-45
}
