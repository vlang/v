import math

fn test_gcd_returns_correct_values() {
	assert math.gcd(6, 9) == 3
	assert math.gcd(6, -9) == 3
	assert math.gcd(-6, -9) == 3
	assert math.gcd(0, 0) == 0
}

fn test_lcm_returns_correct_values() {
	assert math.lcm(2, 3) == 6
	assert math.lcm(-2, 3) == 6
	assert math.lcm(-2, -3) == 6
	assert math.lcm(0, 0) == 0
}
