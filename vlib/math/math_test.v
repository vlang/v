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

fn test_digits() {
	digits_in_10th_base := math.digits(125, 10)
	assert digits_in_10th_base[0] == 5
	assert digits_in_10th_base[1] == 2
	assert digits_in_10th_base[2] == 1

	digits_in_16th_base := math.digits(15, 16)
	assert digits_in_16th_base[0] == 15

	negative_digits := math.digits(-4, 2)
	assert negative_digits[2] == -1
}

/* 
fn test_factorial() {
	assert math.factorial(12) == 479001600
	assert math.factorial(5) == 120
	assert math.factorial(0) == 1
}
*/ 

fn test_erf() {
	assert math.erf(0) == 0
	assert math.erf(1.5) + math.erf(-1.5) == 0
	assert math.erfc(0) == 1
	assert math.erf(2.5) + math.erfc(2.5) == 1
	assert math.erfc(3.6) + math.erfc(-3.6) == 2
}

fn test_gamma() {
	assert math.gamma(1) == 1
	assert math.gamma(5) == 24
	assert math.log_gamma(4.5) == math.log(math.gamma(4.5))
}
