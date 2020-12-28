module math

fn tst_res(str1 string, str2 string) bool {
	if (math.abs(str1.f64() - str2.f64())) < 1e-5 {
		return true
	}
	return false
}

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

fn test_max() {
	assert typeof(max(int(0), int(1))) == 'int'
	assert max(0, 1) == 1
	assert max(1, 0) == 1
	assert max(0.1, 0.2) == 0.2
	assert max(0.2, 0.1) == 0.2
}

fn test_min() {
	assert typeof(min(int(0), int(1))) == 'int'
	assert min(0, 1) == 0
	assert min(1, 0) == 0
	assert min(0.1, 0.2) == 0.1
	assert min(0.2, 0.1) == 0.1
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
	assert tst_res(log_gamma(4.5).str(), sval)
	assert tst_res(log(gamma(4.5)).str(), sval)
	//assert log_gamma(4.5).str() == sval
	//assert log(gamma(4.5)).str() == sval
	assert abs( log_gamma(4.5) - log(gamma(4.5)) ) < 0.000001
	// assert log_gamma(4.5) == log(gamma(4.5)) /* <-- fails on alpine/musl
}

fn test_mod() {
	assert 4 % 2 == 0
	x := u64(2)
	assert u64(5) % x == 1
	mut a := 10
	a %= 2
	assert a == 0
}

fn test_copysign() {
	assert copysign(5, -7) == -5.0
	assert copysign(-5, 7) == 5.0
	assert copysign(-5, -7) == -5.0
	assert copysign(10, 0) == 10.0
	assert copysign(10, 10) == 10.0
}
