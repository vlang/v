import math.big

fn test_modular_powers_negative_bases() {
	for value in -40 .. 0 {
		base := big.integer_from_int(value)
		for modulus in 1 .. 20 {
			m := big.integer_from_int(modulus)
			mut expected := 1 % modulus
			for exponent in 0 .. 12 {
				want := big.integer_from_int(expected)
				assert base.mod_pow(u64(exponent), m) == want
				assert base.mod_pow(u64(exponent), m.neg()) == want
				assert base.big_mod_pow(big.integer_from_int(exponent), m)! == want
				assert base.big_mod_pow(big.integer_from_int(exponent), m.neg())! == want
				expected = ((expected * value) % modulus + modulus) % modulus
			}
		}
	}
}

fn test_modular_powers_negative_base_large_exponents() {
	exponent := big.integer_from_string('18446744073709551617')!
	// Odd, power-of-two and other even moduli exercise every windowed algorithm.
	for modulus in [97, 128, 100] {
		m := big.integer_from_int(modulus)
		for value in [-3, -1, -97, -128] {
			base := big.integer_from_int(value)
			mut e := u64(0xffffffffffffffff)
			mut x := ((value % modulus) + modulus) % modulus
			mut expected := 1
			for e > 0 {
				if e & 1 != 0 {
					expected = (expected * x) % modulus
				}
				x = (x * x) % modulus
				e >>= 1
			}
			// 2^64 + 1 = (2^64 - 1) + 2.
			expected = (expected * value * value) % modulus
			want := big.integer_from_int(expected)
			assert base.big_mod_pow(exponent, m)! == want
			assert base.big_mod_pow(exponent, m.neg())! == want
		}
	}
}

fn test_modular_powers_large_positive_base_power_of_two_moduli() {
	exponent := big.integer_from_string('18446744073709551617')!
	// The high bits do not affect a power-of-two remainder. Keep the base much
	// larger than the modulus to exercise the mask-based binary exponentiation path.
	high := big.one_int.left_shift(600000)
	for bits in [u32(7), 60, 61, 120] {
		m := big.one_int.left_shift(bits)
		for value in [0, 1, 3, 127, 128] {
			residue := big.integer_from_int(value)
			base := high + residue
			// 2^64 + 1 = (2^64 - 1) + 2, with the reference reduced before powering.
			expected := (residue.mod_pow(u64(0xffffffffffffffff), m) * residue * residue) % m
			positive := base.big_mod_pow(exponent, m)!
			negative := base.big_mod_pow(exponent, m.neg())!
			assert positive == expected, '${bits} ${value}: ${positive} != ${expected}'
			assert negative == expected, '${bits} ${value}: ${negative} != ${expected}'
		}
	}
}

fn test_mod_inverse_negative_operands() {
	m := big.integer_from_int(97)
	assert big.integer_from_int(-3).mod_inverse(m)! == big.integer_from_int(32)
	for value in -200 .. 0 {
		a := big.integer_from_int(value)
		if a.gcd(m) == big.one_int {
			inverse := a.mod_inverse(m)!
			assert inverse >= big.zero_int
			assert inverse < m
			assert (a * inverse).mod_euclid(m) == big.one_int
		}
	}
	if _ := big.integer_from_int(-3).mod_inverse(m.neg()) {
		assert false, 'negative moduli must be rejected'
	}
	if _ := big.integer_from_int(-6).mod_inverse(big.integer_from_int(9)) {
		assert false, 'non-coprime operands must be rejected'
	}
}
