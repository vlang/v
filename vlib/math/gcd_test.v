module math

fn test_gcd_basic() {
	assert gcd(54, 24) == 6
	assert gcd(24, 54) == 6
	assert gcd(0, 5) == 5
	assert gcd(5, 0) == 5
	assert gcd(0, 0) == 0
	assert gcd(13, 13) == 13
	assert gcd(17, 19) == 1
}

fn test_gcd_signs() {
	assert gcd(-54, 24) == 6
	assert gcd(54, -24) == 6
	assert gcd(-54, -24) == 6
	assert gcd_many([12, 18, 24]) == 6
	assert gcd_many([]int{}) == 0
}

fn test_gcd_min_edge() {
	// |min| is unrepresentable: saturates to max.
	assert gcd(min_i8, i8(0)) == 127
	assert gcd(min_i8, min_i8) == 127
	assert gcd(u8(200), u8(150)) == 50
}

fn test_lcm_basic() {
	assert lcm(21, 6) == 42
	assert lcm(0, 5) == 0
	assert lcm(5, 0) == 0
	assert lcm(-4, 6) == 12
	assert lcm(-4, -6) == 12
	assert lcm_many([2, 3, 4]) == 12
	assert lcm_many([]int{}) == 1
}

fn test_lcm_saturates() {
	assert lcm(u8(200), u8(150)) == 255
	assert lcm(i32(2000000000), i32(3)) == 2147483647
	assert lcm(max_u64, u64(2)) == max_u64
}

fn test_gcd_many_preserves_unsaturated_magnitude() {
	assert gcd_many([min_i8, i8(0), i8(2)]) == 2
	assert gcd_many([min_i8, i8(2), i8(0)]) == 2
	assert gcd_many([i8(0), min_i8, i8(2)]) == 2
	assert gcd_many([min_i64, i64(0), i64(16)]) == 16
	assert gcd_many([min_i8]) == max_i8
	assert lcm(min_i8, min_i8) == max_i8
}
