import math

fn test_f64_to_u64_boundary_values() {
	max_u64 := u64(-1)
	pow_63 := math.pow(2, 63)
	pow_64 := math.pow(2, 64)
	high_step := pow_63 + 2048.0

	assert u64(-1.0) == 0
	assert u64(pow_63) == u64(1) << 63
	assert u64(high_step) == (u64(1) << 63) + u64(2048)
	assert u64(pow_64) == max_u64
	assert u64(f64(max_u64)) == max_u64
}
