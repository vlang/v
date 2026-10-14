import math.big

fn test_bin_str() {
	assert big.one_int.bin_str() == '1'
	assert big.two_int.bin_str() == '10'
	assert big.three_int.bin_str() == '11'
	assert big.integer_from_int(255).bin_str() == '11111111'
	assert big.integer_from_int(256).bin_str() == '100000000'
	assert big.integer_from_int(-255).bin_str() == '-11111111'
	assert big.zero_int.bin_str() == '0'
}

fn test_abs() {
	assert big.integer_from_int(0).abs() == big.zero_int
	assert big.integer_from_int(9).abs() == big.integer_from_int(9)
	assert big.integer_from_int(-9).abs() == big.integer_from_int(9)
	assert big.zero_int.abs() == big.zero_int
}

fn test_abs_cmp_compares_magnitudes_only() {
	assert big.integer_from_int(3).abs_cmp(big.integer_from_int(5)) == -1
	assert big.integer_from_int(5).abs_cmp(big.integer_from_int(3)) == 1
	assert big.integer_from_int(-5).abs_cmp(big.integer_from_int(3)) == 1
	assert big.integer_from_int(-5).abs_cmp(big.integer_from_int(-3)) == 1
	assert big.integer_from_int(4).abs_cmp(big.integer_from_int(4)) == 0
	assert big.integer_from_int(-7).abs_cmp(big.integer_from_int(-7)) == 0
	assert big.zero_int.abs_cmp(big.zero_int) == 0
}

fn test_div_checked() {
	q := big.integer_from_int(17).div_checked(big.integer_from_int(5)) or { panic(err) }
	assert q == big.integer_from_int(3)
	assert big.integer_from_int(-17).div_checked(big.integer_from_int(5)) or { panic(err) } == big.integer_from_int(-3)
	assert big.zero_int.div_checked(big.two_int) or { panic(err) } == big.zero_int
	big.integer_from_int(17).div_checked(big.zero_int) or {
		assert err.msg() == 'math.big: Cannot divide by zero'
		return
	}
	assert false, 'div_checked(17, 0) should have failed'
}

fn test_mod_euclid_checked() {
	assert big.integer_from_int(7).mod_euclid_checked(big.integer_from_int(3)) or { panic(err) } == big.integer_from_int(1)
	assert big.integer_from_int(-7).mod_euclid_checked(big.integer_from_int(3)) or { panic(err) } == big.integer_from_int(2)
	assert big.integer_from_int(-6).mod_euclid_checked(big.integer_from_int(3)) or { panic(err) } == big.zero_int
	assert big.zero_int.mod_euclid_checked(big.integer_from_int(3)) or { panic(err) } == big.zero_int
	big.integer_from_int(-7).mod_euclid_checked(big.zero_int) or {
		assert err.msg() == 'math.big: Cannot divide by zero'
		return
	}
	assert false, 'mod_euclid_checked(-7, 0) should have failed'
}

// NOTE: the documentation says the result of mod_euclid is non-negative for a positive
// divisor; for a negative divisor the observed behaviour is also non-negative because
// divisor.abs() is added. The result is not the Euclidean remainder with the divisor's sign.
fn test_mod_euclid_checked_negative_divisor_keeps_current_behaviour() {
	assert big.integer_from_int(-7).mod_euclid_checked(big.integer_from_int(-3)) or { panic(err) } == big.integer_from_int(2)
	assert big.integer_from_int(7).mod_euclid_checked(big.integer_from_int(-3)) or { panic(err) } == big.integer_from_int(1)
}

fn test_isqrt_checked() {
	assert big.zero_int.isqrt_checked() or { panic(err) } == big.zero_int
	assert big.one_int.isqrt_checked() or { panic(err) } == big.one_int
	assert big.integer_from_int(144).isqrt_checked() or { panic(err) } == big.integer_from_int(12)
	assert big.integer_from_int(145).isqrt_checked() or { panic(err) } == big.integer_from_int(12)
	assert big.integer_from_int(1000000).isqrt_checked() or { panic(err) } == big.integer_from_int(1000)
	big.integer_from_int(-4).isqrt_checked() or {
		assert err.msg() == 'math.big: Cannot calculate square root of negative integer'
		return
	}
	assert false, 'isqrt_checked(-4) should have failed'
}

// NOTE: the doc comment calls the result "the closest integer square root", but the
// observed behaviour is the floor: isqrt_checked(145) == 12, not 13.
fn test_isqrt_checked_is_floor() {
	for n in [2, 3, 5, 10, 17, 99, 101, 1000] {
		r := big.integer_from_int(n).isqrt_checked() or { panic(err) }
		assert r * r <= big.integer_from_int(n)
		assert (r + big.one_int) * (r + big.one_int) > big.integer_from_int(n)
	}
}

fn test_from_json_number_and_to_json() {
	assert big.zero_int.to_json() == '0'
	assert big.integer_from_int(-5).to_json() == '-5'

	mut a := big.zero_int
	a.from_json_number('42') or { panic(err) }
	assert a == big.integer_from_int(42)
	assert a.to_json() == '42'

	mut b := big.zero_int
	b.from_json_number('-123456789012345678901234567890') or { panic(err) }
	assert b == big.integer_from_string('-123456789012345678901234567890') or { panic(err) }
	assert b.signum == -1
	assert b.to_json() == '-123456789012345678901234567890'

	mut c := big.zero_int
	c.from_json_number('1.5') or {
		assert err.msg() == 'expected integer but got real number'
		return
	}
	assert false, 'from_json_number("1.5") should have failed'
}

fn test_is_probably_prime_checked() {
	assert big.two_int.is_probably_prime_checked(5) or { panic(err) }
	assert big.integer_from_int(7919).is_probably_prime_checked(5) or { panic(err) }
	assert !big.integer_from_int(7917).is_probably_prime_checked(5) or { panic(err) }
	assert !big.integer_from_int(561).is_probably_prime_checked(5) or { panic(err) }
	assert !big.integer_from_int(1).is_probably_prime_checked(5) or { panic(err) }
	assert !big.integer_from_int(0).is_probably_prime_checked(5) or { panic(err) }
	assert !big.integer_from_int(-3).is_probably_prime_checked(5) or { panic(err) }
}
