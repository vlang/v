import math

fn test_gcd_and_lcm() {
	assert math.gcd(6, 9) == 3
	assert math.gcd(6, -9) == 3

	assert math.lcm(2, 3) == 6
	assert math.lcm(-2, 3) == 6
}
