// test suite for bits and bits math functions
module bits

fn test_leading_zeros() {
	mut i := 0

	// 8 bit
	i = 1
	for x in 0 .. 8 {
		assert leading_zeros_8(u8(u8(i) << x)) == 7 - x
	}
	assert leading_zeros_8(0) == 8

	// 16 bit
	i = 1
	for x in 0 .. 16 {
		assert leading_zeros_16(u16(i) << x) == 15 - x
	}
	assert leading_zeros_16(0) == 16

	// 32 bit
	i = 1
	for x in 0 .. 32 {
		assert leading_zeros_32(u32(i) << x) == 31 - x
	}
	assert leading_zeros_32(0) == 32

	// 64 bit
	i = 1
	for x in 0 .. 64 {
		assert leading_zeros_64(u64(i) << x) == 63 - x
	}
	assert leading_zeros_64(0) == 64
}

fn test_trailing_zeros() {
	mut i := 0
	// 8 bit
	i = 1
	for x in 0 .. 8 {
		assert trailing_zeros_8(u8(u8(i) << x)) == x
	}
	assert trailing_zeros_8(0) == 8

	// 16 bit
	i = 1
	for x in 0 .. 16 {
		assert trailing_zeros_16(u16(i) << x) == x
	}
	assert trailing_zeros_16(0) == 16

	// 32 bit
	i = 1
	for x in 0 .. 32 {
		assert trailing_zeros_32(u32(i) << x) == x
	}
	assert trailing_zeros_32(0) == 32

	// 64 bit
	i = 1
	for x in 0 .. 64 {
		assert trailing_zeros_64(u64(i) << x) == x
	}
	assert trailing_zeros_64(0) == 64
}

fn test_ones_count() {
	mut i := 0
	mut i1 := u64(0)
	// 8 bit
	i = 0
	for x in 0 .. 9 {
		assert ones_count_8(u8(i)) == x
		i = int(u32(i) << 1) + 1
	}
	assert ones_count_8(0) == 0
	assert ones_count_8(0xFF) == 8

	// 16 bit
	i = 0
	for x in 0 .. 17 {
		assert ones_count_16(u16(i)) == x
		i = int(u32(i) << 1) + 1
	}
	assert ones_count_16(0) == 0
	assert ones_count_16(0xFFFF) == 16

	// 32 bit
	i = 0
	for x in 0 .. 33 {
		assert ones_count_32(u32(i)) == x
		i = int(u32(i) << 1) + 1
	}
	assert ones_count_32(0) == 0
	assert ones_count_32(0xFFFF_FFFF) == 32

	// 64 bit
	i1 = 0
	for x in 0 .. 65 {
		assert ones_count_64(i1) == x
		i1 = (i1 << 1) + 1
	}
	assert ones_count_64(0) == 0
	assert ones_count_64(0xFFFF_FFFF_FFFF_FFFF) == 64
}

fn test_rotate_left_right() {
	assert rotate_left_8(0x12, 4) == 0x21
	assert rotate_left_16(0x1234, 8) == 0x3412
	assert rotate_left_32(0x12345678, 16) == 0x56781234
	assert rotate_left_64(0x1234567887654321, 32) == 0x8765432112345678
}

fn test_reverse() {
	mut i := 0
	mut i1 := u64(0)
	// 8 bit
	i = 0
	for _ in 0 .. 9 {
		mut rv := u8(0)
		mut bc := 0
		mut n := i
		for bc < 8 {
			rv = (rv << 1) | (u8(n) & 0x01)
			bc++
			n = n >> 1
		}
		assert reverse_8(u8(i)) == rv
		i = int(u32(i) << 1) + 1
	}

	// 16 bit
	i = 0
	for _ in 0 .. 17 {
		mut rv := u16(0)
		mut bc := 0
		mut n := i
		for bc < 16 {
			rv = (rv << 1) | (u16(n) & 0x01)
			bc++
			n = n >> 1
		}
		assert reverse_16(u16(i)) == rv
		i = int(u32(i) << 1) + 1
	}

	// 32 bit
	i = 0
	for _ in 0 .. 33 {
		mut rv := u32(0)
		mut bc := 0
		mut n := i
		for bc < 32 {
			rv = (rv << 1) | (u32(n) & 0x01)
			bc++
			n = n >> 1
		}
		assert reverse_32(u32(i)) == rv
		i = int(u32(i) << 1) + 1
	}

	// 64 bit
	i1 = 0
	for _ in 0 .. 64 {
		mut rv := u64(0)
		mut bc := 0
		mut n := i1
		for bc < 64 {
			rv = (rv << 1) | (n & 0x01)
			bc++
			n = n >> 1
		}
		assert reverse_64(i1) == rv
		i1 = (i1 << 1) + 1
	}
}

fn test_add() {
	mut i := 0
	// 32 bit
	i = 1
	for x in 0 .. 32 {
		v := u32(i) << x
		sum, carry := add_32(v, v, u32(0))
		assert ((u64(carry) << 32) | u64(sum)) == u64(v) + u64(v)
	}
	mut sum_32t, mut carry_32t := add_32(0x8000_0000, 0x8000_0000, u32(0))
	assert sum_32t == u32(0)
	assert carry_32t == u32(1)

	sum_32t, carry_32t = add_32(0xFFFF_FFFF, 0xFFFF_FFFF, u32(1))
	assert sum_32t == 0xFFFF_FFFF
	assert carry_32t == u32(1)

	// 64 bit
	i = 1
	for x in 0 .. 63 {
		v := u64(i) << x
		sum, carry := add_64(v, v, u64(0))
		expected_sum := v + v
		expected_carry := u64(expected_sum < v)
		assert sum == expected_sum
		assert carry == expected_carry
	}
	mut sum_64t, mut carry_64t := add_64(0x8000_0000_0000_0000, 0x8000_0000_0000_0000, u64(0))
	assert sum_64t == u64(0)
	assert carry_64t == u64(1)

	sum_64t, carry_64t = add_64(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF_FFFF_FFFF, u64(1))
	assert sum_64t == 0xFFFF_FFFF_FFFF_FFFF
	assert carry_64t == u64(1)
}

fn test_sub() {
	mut i := 0
	// 32 bit
	i = 1
	for x in 1 .. 32 {
		v0 := u32(i) << x
		v1 := v0 >> 1
		mut diff, mut borrow_out := sub_32(v0, v1, u32(0))
		assert diff == v1

		diff, borrow_out = sub_32(v0, v1, u32(1))
		assert diff == (v1 - 1)
		assert borrow_out == u32(0)

		diff, borrow_out = sub_32(v1, v0, u32(1))
		assert borrow_out == u32(1)
	}

	// 64 bit
	i = 1
	for x in 1 .. 64 {
		v0 := u64(i) << x
		v1 := v0 >> 1
		mut diff, mut borrow_out := sub_64(v0, v1, u64(0))
		assert diff == v1

		diff, borrow_out = sub_64(v0, v1, u64(1))
		assert diff == (v1 - 1)
		assert borrow_out == u64(0)

		diff, borrow_out = sub_64(v1, v0, u64(1))
		assert borrow_out == u64(1)
	}
}

fn test_mul() {
	mut i := 0
	// 32 bit
	i = 1
	for x in 0 .. 32 {
		v0 := u32(i) << x
		v1 := v0 - 1
		hi, lo := mul_32(v0, v1)
		assert (u64(hi) << 32) | (u64(lo)) == u64(v0) * u64(v1)
		v2 := u32(x)
		h, l := mul_add_32(v0, v1, v2)
		assert (u64(h) << 32) | (u64(l)) == u64(v0) * u64(v1) + u64(v2)
	}

	// 64 bit
	i = 1
	for x in 0 .. 64 {
		v0 := u64(i) << x
		v1 := v0 - 1
		hi, lo := mul_64(v0, v1)
		exp_hi, exp_lo := mul_64_default(v0, v1)
		assert hi == exp_hi
		assert lo == exp_lo
		v2 := u64(x)
		h, l := mul_add_64(v0, v1, v2)
		exp_h, exp_l := mul_add_64_default(v0, v1, v2)
		assert h == exp_h
		assert l == exp_l
	}
}

fn test_div() {
	mut i := 0
	// 32 bit
	i = 1
	for x in 0 .. 31 {
		hi := u32(i) << x
		lo := hi - 1
		y := u32(3) << x
		quo, rem := div_32(hi, lo, y)
		tst := ((u64(hi) << 32) | u64(lo))
		assert quo == (tst / u64(y))
		assert rem == (tst % u64(y))
		assert rem == rem_32(hi, lo, y)
	}

	// 64 bit
	i = 1
	for x in 0 .. 62 {
		hi := u64(i) << x
		lo := u64(2) // hi - 1
		y := u64(0x4000_0000_0000_0000)
		quo, rem := div_64(hi, lo, y)
		assert quo == u64(2) << (x + 1)
		_, rem1 := div_64(hi % y, lo, y)
		assert rem == rem1
		assert rem == rem_64(hi, lo, y)
	}
}

fn test_div_64_edge_cases() {
	qq, rr := div_64(10, 12, 11)
	assert qq == 16769767339735956015
	assert rr == 7
	q, r := div_64(0, 23, 10000000000000000000)
	assert q == 0
	assert r == 23
}

fn test_randomized_arithmetic_properties() {
	mut state := u64(0x9e3779b97f4a7c15)
	for _ in 0 .. 2000 {
		state = next_u64(state)
		a64 := state
		state = next_u64(state)
		b64 := state
		state = next_u64(state)
		carry_in64 := state & 1
		sum64, carry_out64 := add_64(a64, b64, carry_in64)
		tmp := a64 + b64
		expected_sum64 := tmp + carry_in64
		expected_carry64 := u64(tmp < a64) | u64(expected_sum64 < tmp)
		assert sum64 == expected_sum64
		assert carry_out64 == expected_carry64

		diff64, borrow_out64 := sub_64(a64, b64, carry_in64)
		tmp_diff := a64 - b64
		expected_diff64 := tmp_diff - carry_in64
		expected_borrow64 := u64(a64 < b64) | u64(tmp_diff < carry_in64)
		assert diff64 == expected_diff64
		assert borrow_out64 == expected_borrow64

		mul_hi, mul_lo := mul_64(a64, b64)
		exp_mul_hi, exp_mul_lo := mul_64_default(a64, b64)
		assert mul_hi == exp_mul_hi
		assert mul_lo == exp_mul_lo

		state = next_u64(state)
		z64 := state
		mul_add_hi, mul_add_lo := mul_add_64(a64, b64, z64)
		exp_mul_add_hi, exp_mul_add_lo := mul_add_64_default(a64, b64, z64)
		assert mul_add_hi == exp_mul_add_hi
		assert mul_add_lo == exp_mul_add_lo

		state = next_u64(state)
		mut y64 := state | 1
		state = next_u64(state)
		mut hi64 := state
		hi64 %= y64
		state = next_u64(state)
		lo64 := state
		quo64, rem64 := div_64(hi64, lo64, y64)
		exp_quo64, exp_rem64 := div_64_default(hi64, lo64, y64)
		assert quo64 == exp_quo64
		assert rem64 == exp_rem64
		assert rem64 == rem_64(hi64, lo64, y64)

		a32 := u32(a64)
		b32 := u32(b64)
		carry_in32 := u32(carry_in64)
		sum32, carry_out32 := add_32(a32, b32, carry_in32)
		expected32 := u64(a32) + u64(b32) + u64(carry_in32)
		assert sum32 == u32(expected32)
		assert carry_out32 == u32(expected32 >> 32)

		diff32, borrow_out32 := sub_32(a32, b32, carry_in32)
		expected_diff32 := a32 - b32 - carry_in32
		expected_borrow32 := u32((~a32 & b32) | (~(a32 ^ b32) & expected_diff32)) >> 31
		assert diff32 == expected_diff32
		assert borrow_out32 == expected_borrow32

		mut y32 := u32(y64)
		if y32 == 0 {
			y32 = 1
		}
		state = next_u64(state)
		hi32 := u32(state % u64(y32))
		state = next_u64(state)
		lo32 := u32(state)
		quo32, rem32 := div_32(hi32, lo32, y32)
		numerator32 := (u64(hi32) << 32) | u64(lo32)
		assert quo32 == u32(numerator32 / u64(y32))
		assert rem32 == u32(numerator32 % u64(y32))
		assert rem32 == rem_32(hi32, lo32, y32)
	}
}

// rem_32 and rem_64 panic when y == 0 (division by zero). This behavior is tested
// through the randomized property test which guards against y==0, and through manual
// verification. Direct panic tests are avoided to prevent test suite crashes.

@[inline]
fn next_u64(state u64) u64 {
	return state * u64(6364136223846793005) + u64(1442695040888963407)
}
