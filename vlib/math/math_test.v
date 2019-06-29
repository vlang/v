import math

fn test_gcd() {
	assert math.gcd(6, 9) == 3
	assert math.gcd(6, -9) == 3
	assert math.gcd(-6, -9) == 3
	assert math.gcd(0, 0) == 0
}

fn test_lcm() {
	assert math.lcm(2, 3) == 6
	assert math.lcm(-2, 3) == 6
	assert math.lcm(-2, -3) == 6
	assert math.lcm(0, 0) == 0
}
