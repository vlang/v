import hash

fn wymum_reference(a u64, b u64) u64 {
	full := u128(a) * u128(b)
	return u64(full >> 64) ^ u64(full)
}

// wymum is documented as "multiply and mix of a and b", which is exactly the
// 128 bit product's high word xor its low word.
fn test_wymum_matches_u128_product() {
	for a in [u64(0), 1, 2, 0xff, 0x1234567890abcdef, u64(1) << 32, u64(1) << 63,
		u64(18446744073709551615)] {
		for b in [u64(0), 1, 0x0fedcba098765432, u64(1) << 32, u64(18446744073709551615)] {
			assert hash.wymum(a, b) == wymum_reference(a, b)
		}
	}
}

fn test_wymum_is_commutative() {
	for a in [u64(1), 0xdeadbeef, u64(1) << 40, u64(18446744073709551615)] {
		for b in [u64(3), 0xfeedface, u64(1) << 33, u64(18446744073709551614)] {
			assert hash.wymum(a, b) == hash.wymum(b, a)
		}
	}
}

fn test_wymum_zero_operands() {
	assert hash.wymum(0, 0) == 0
	assert hash.wymum(0, u64(12345)) == 0
	assert hash.wymum(u64(12345), 0) == 0
	assert hash.wymum(u64(18446744073709551615), u64(18446744073709551615)) == u64(18446744073709551615)
}
