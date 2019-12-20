module math

fn test_gcd() {
	assert gcd(6, 9) == 3
	assert gcd(6, -9) == 3
	assert gcd(-6, -9) == 3
	assert gcd(0, 0) == 0
}

fn test_lcm() {
	assert lcm(2, 3) == 6
	assert lcm(-2, 3) == 6
	assert lcm(-2, -3) == 6
	assert lcm(0, 0) == 0
}

fn test_digits() {
	digits_in_10th_base := digits(125, 10)
	assert digits_in_10th_base[0] == 5
	assert digits_in_10th_base[1] == 2
	assert digits_in_10th_base[2] == 1

	digits_in_16th_base := digits(15, 16)
	assert digits_in_16th_base[0] == 15

	negative_digits := digits(-4, 2)
	assert negative_digits[2] == -1
}

fn test_factorial() {
	assert factorial(12) == 479001600
	assert factorial(5) == 120
	assert factorial(0) == 1
}

fn test_erf() {
	assert erf(0) == 0
	assert erf(1.5) + erf(-1.5) == 0
	assert erfc(0) == 1
	assert erf(2.5) + erfc(2.5) == 1
	assert erfc(3.6) + erfc(-3.6) == 2
}

fn test_gamma() {
	assert gamma(1) == 1
	assert gamma(5) == 24
	sval := '2.453737'
	assert log_gamma(4.5).str() == sval
	assert log(gamma(4.5)).str() == sval
	assert abs( log_gamma(4.5) - log(gamma(4.5)) ) < 0.000001
	// assert log_gamma(4.5) == log(gamma(4.5)) /* <-- fails on alpine/musl
}

fn test_mod() {
	assert 4 % 2 == 0
	x := 2
	assert u64(5) % x == 1
	mut a := 10
	a %= 2
	assert a == 0
}
