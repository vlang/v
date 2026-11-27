module big

fn check_decimal(value Integer) {
	old := if value.signum == 0 { '0' } else { value.general_radix_str(10) }
	assert value.str() == old
	assert value.radix_str(10) == old
	assert (integer_from_string(old) or { panic(err) }) == value
}

fn test_decimal_boundaries() {
	check_decimal(zero_int)
	for exp in [0, 1, 8, 9, 10, 17, 18, 19, 20, 99, 100, 143, 144, 145, 287, 288, 289, 575, 576,
		577, 1000, 1151, 1152, 1153, 2000, 10_000] {
		power := integer_from_int(10).pow(u32(exp))
		for value in [power - one_int, power, power + one_int] {
			check_decimal(value)
			check_decimal(value.neg())
		}
	}
	for limbs in [1, 2, 3, 15, 16, 17, 31, 32, 33, 63, 64, 65, 69, 70, 71, 359, 360, 361, 554] {
		power := one_int.left_shift(u32(limbs * digit_bits))
		for value in [power - one_int, power, power + one_int] {
			check_decimal(value)
			check_decimal(value.neg())
		}
	}
}

fn test_decimal_large_boundaries() {
	// These values have a known decimal text, so they need no comparison with the slower formatter.
	exp := 20_000
	power := integer_from_int(10).pow(u32(exp))
	assert (power - one_int).str() == '9'.repeat(exp)
	assert power.str() == '1' + '0'.repeat(exp)
	assert (power + one_int).str() == '1' + '0'.repeat(exp - 1) + '1'
	assert power.neg().str() == '-1' + '0'.repeat(exp)
}

fn test_decimal_random() {
	mut seed := u64(0x6a09e667f3bcc909)
	for limbs in [1, 2, 6, 15, 16, 17, 31, 32, 33, 56, 111, 554] {
		for _ in 0 .. 5 {
			mut digits := []u64{len: limbs}
			for i in 0 .. limbs {
				seed ^= seed << 13
				seed ^= seed >> 7
				seed ^= seed << 17
				digits[i] = seed & max_digit
			}
			digits[limbs - 1] |= 1
			check_decimal(Integer{ digits: digits, signum: 1 })
			check_decimal(Integer{ digits: digits, signum: -1 })
		}
	}
}

fn test_decimal_other_radices() {
	for text in ['0', '1', '-1', '1234567890123456789012345678901234567890', '-1000000000000000001'] {
		value := integer_from_string(text)!
		assert value.str() == text
		for radix in 2 .. 37 {
			old := if value.signum == 0 { '0' } else { value.general_radix_str(radix) }
			assert value.radix_str(u32(radix)) == old
			assert integer_from_radix(old, u32(radix))! == value
		}
		assert value.radix_str(0) == '0'
	}
}

fn test_decimal_split_boundaries() {
	for exp in [144, 288, 576, 1152, 2304] {
		power := integer_from_int(10).pow(u32(exp))
		for limbs in [1, 2, power.digits.len - 1, power.digits.len] {
			q := one_int.left_shift(u32(limbs * digit_bits))
			for quotient in [q - one_int, q, q + one_int] {
				for remainder in [zero_int, one_int, power - one_int] {
					check_decimal(quotient * power + remainder)
				}
			}
		}
	}
}

fn test_decimal_signum_values() {
	for signum in [-2, -1, 0, 1, 2] {
		for value in [Integer{ signum: signum }, integer_from_bytes([u8(123)], signum: signum)] {
			old := if signum == 0 { '0' } else { value.general_radix_str(10) }
			assert value.str() == old
		}
	}
}

fn test_decimal_non_normalized_digits() {
	for limbs in [1, 2, 3, 15, 16, 17, 32] {
		for low in [u64(0), 1, 123, 999_999_999, 1_000_000_000, max_digit] {
			for signum in [-2, -1, 0, 1, 2] {
				mut digits := []u64{len: limbs}
				digits[0] = low
				value := Integer{ digits: digits, signum: signum }
				before := value.digits.clone()
				expected := if signum == 0 {
					'0'
				} else if signum == -1 {
					'-${low}'
				} else {
					low.str()
				}
				assert value.str() == expected
				assert value.radix_str(10) == expected
				assert value.digits == before
			}
		}
	}
}
