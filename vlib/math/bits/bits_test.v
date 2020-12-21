//
// test suite for bits and bits math functions
//
module bits

fn test_bits() {
	mut i := 0
	mut i1 := u64(0)
	//
	// --- LeadingZeros ---
	//
	// 8 bit
	i = 1
	for x in 0 .. 8 {
		// C.printf("x:%02x lz: %d cmp: %d\n", i << x, leading_zeros_8(i << x), 7-x)
		assert leading_zeros_8(byte(i << x)) == 7 - x
	}
	// 16 bit
	i = 1
	for x in 0 .. 16 {
		// C.printf("x:%04x lz: %d cmp: %d\n", u16(i) << x, leading_zeros_16(u16(i) << x), 15-x)
		assert leading_zeros_16(u16(i) << x) == 15 - x
	}
	// 32 bit
	i = 1
	for x in 0 .. 32 {
		// C.printf("x:%08x lz: %d cmp: %d\n", u32(i) << x, leading_zeros_32(u32(i) << x), 31-x)
		assert leading_zeros_32(u32(i) << x) == 31 - x
	}
	// 64 bit
	i = 1
	for x in 0 .. 64 {
		// C.printf("x:%016llx lz: %llu cmp: %d\n", u64(i) << x, leading_zeros_64(u64(i) << x), 63-x)
		assert leading_zeros_64(u64(i) << x) == 63 - x
	}
	//
	// --- ones_count ---
	//
	// 8 bit
	i = 0
	for x in 0 .. 9 {
		// C.printf("x:%02x lz: %llu cmp: %d\n", byte(i), ones_count_8(byte(i)), x)
		assert ones_count_8(byte(i)) == x
		i = (i << 1) + 1
	}
	// 16 bit
	i = 0
	for x in 0 .. 17 {
		// C.printf("x:%04x lz: %llu cmp: %d\n", u16(i), ones_count_16(u16(i)), x)
		assert ones_count_16(u16(i)) == x
		i = (i << 1) + 1
	}
	// 32 bit
	i = 0
	for x in 0 .. 33 {
		// C.printf("x:%08x lz: %llu cmp: %d\n", u32(i), ones_count_32(u32(i)), x)
		assert ones_count_32(u32(i)) == x
		i = (i << 1) + 1
	}
	// 64 bit
	i1 = 0
	for x in 0 .. 65 {
		// C.printf("x:%016llx lz: %llu cmp: %d\n", u64(i1), ones_count_64(u64(i1)), x)
		assert ones_count_64(i1) == x
		i1 = (i1 << 1) + 1
	}
	//
	// --- rotate_left/right ---
	//
	assert rotate_left_8(0x12, 4) == 0x21
	assert rotate_left_16(0x1234, 8) == 0x3412
	assert rotate_left_32(0x12345678, 16) == 0x56781234
	assert rotate_left_64(0x1234567887654321, 32) == 0x8765432112345678
	//
	// --- reverse ---
	//
	// 8 bit
	i = 0
	for _ in 0 .. 9 {
		mut rv := byte(0)
		mut bc := 0
		mut n := i
		for bc < 8 {
			rv = (rv << 1) | (byte(n) & 0x01)
			bc++
			n = n >> 1
		}
		// C.printf("x:%02x lz: %llu cmp: %d\n", byte(i), reverse_8(byte(i)), rv)
		assert reverse_8(byte(i)) == rv
		i = (i << 1) + 1
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
		// C.printf("x:%04x lz: %llu cmp: %d\n", u16(i), reverse_16(u16(i)), rv)
		assert reverse_16(u16(i)) == rv
		i = (i << 1) + 1
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
		// C.printf("x:%08x lz: %llu cmp: %d\n", u32(i), reverse_32(u32(i)), rv)
		assert reverse_32(u32(i)) == rv
		i = (i << 1) + 1
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
		// C.printf("x:%016llx lz: %016llx cmp: %016llx\n", u64(i1), reverse_64(u64(i1)), rv)
		assert reverse_64(i1) == rv
		i1 = (i1 << 1) + 1
	}
	//
	// --- add ---
	//
	// 32 bit
	i = 1
	for x in 0 .. 32 {
		v := u32(i) << x
		sum, carry := add_32(v, v, u32(0))
		// C.printf("x:%08x [%llu,%llu] %llu\n", u32(i) << x, sum, carry, u64(v) + u64(v))
		assert ((u64(carry) << 32) | u64(sum)) == u64(v) + u64(v)
	}
	mut sum_32t, mut carry_32t := add_32(0x80000000, 0x80000000, u32(0))
	assert sum_32t == u32(0)
	assert carry_32t == u32(1)
	sum_32t, carry_32t = add_32(0xFFFFFFFF, 0xFFFFFFFF, u32(1))
	assert sum_32t == 0xFFFFFFFF
	assert carry_32t == u32(1)
	// 64 bit
	i = 1
	for x in 0 .. 63 {
		v := u64(i) << x
		sum, carry := add_64(v, v, u64(0))
		// C.printf("x:%16x [%llu,%llu] %llu\n", u64(i) << x, sum, carry, u64(v >> 32) + u64(v >> 32))
		assert ((carry << 32) | sum) == v + v
	}
	mut sum_64t, mut carry_64t := add_64(0x8000000000000000, 0x8000000000000000, u64(0))
	assert sum_64t == u64(0)
	assert carry_64t == u64(1)
	sum_64t, carry_64t = add_64(0xFFFFFFFFFFFFFFFF, 0xFFFFFFFFFFFFFFFF, u64(1))
	assert sum_64t == 0xFFFFFFFFFFFFFFFF
	assert carry_64t == u64(1)
	//
	// --- sub ---
	//
	// 32 bit
	i = 1
	for x in 1 .. 32 {
		v0 := u32(i) << x
		v1 := v0 >> 1
		mut diff, mut borrow_out := sub_32(v0, v1, u32(0))
		// C.printf("x:%08x [%llu,%llu] %08x\n", u32(i) << x, diff, borrow_out, v0 - v1)
		assert diff == v1
		diff, borrow_out = sub_32(v0, v1, u32(1))
		// C.printf("x:%08x [%llu,%llu] %08x\n", u32(i) << x, diff, borrow_out, v0 - v1)
		assert diff == (v1 - 1)
		assert borrow_out == u32(0)
		diff, borrow_out = sub_32(v1, v0, u32(1))
		// C.printf("x:%08x [%llu,%llu] %08x\n", u32(i) << x, diff, borrow_out, v1 - v0)
		assert borrow_out == u32(1)
	}
	// 64 bit
	i = 1
	for x in 1 .. 64 {
		v0 := u64(i) << x
		v1 := v0 >> 1
		mut diff, mut borrow_out := sub_64(v0, v1, u64(0))
		// C.printf("x:%08x [%llu,%llu] %08x\n", u64(i) << x, diff, borrow_out, v0 - v1)
		assert diff == v1
		diff, borrow_out = sub_64(v0, v1, u64(1))
		// C.printf("x:%08x [%llu,%llu] %08x\n", u64(i) << x, diff, borrow_out, v0 - v1)
		assert diff == (v1 - 1)
		assert borrow_out == u64(0)
		diff, borrow_out = sub_64(v1, v0, u64(1))
		// C.printf("x:%08x [%llu,%llu] %08x\n",u64(i) << x, diff, borrow_out, v1 - v0)
		assert borrow_out == u64(1)
	}
	//
	// --- mul ---
	//
	// 32 bit
	i = 1
	for x in 0 .. 32 {
		v0 := u32(i) << x
		v1 := v0 - 1
		hi, lo := mul_32(v0, v1)
		assert (u64(hi) << 32) | (u64(lo)) == u64(v0) * u64(v1)
	}
	// 64 bit
	i = 1
	for x in 0 .. 64 {
		v0 := u64(i) << x
		v1 := v0 - 1
		hi, lo := mul_64(v0, v1)
		// C.printf("v0: %llu v1: %llu [%llu,%llu] tt: %llu\n", v0, v1, hi, lo, (v0 >> 32) * (v1 >> 32))
		assert (hi & 0xFFFFFFFF00000000) == (((v0 >> 32) * (v1 >> 32)) & 0xFFFFFFFF00000000)
		assert (lo & 0x00000000FFFFFFFF) ==
			(((v0 & 0x00000000FFFFFFFF) * (v1 & 0x00000000FFFFFFFF)) & 0x00000000FFFFFFFF)
	}
	//
	// --- div ---
	//
	// 32 bit
	i = 1
	for x in 0 .. 31 {
		hi := u32(i) << x
		lo := hi - 1
		y := u32(3) << x
		quo, rem := div_32(hi, lo, y)
		// C.printf("[%08x_%08x] %08x (%08x,%08x)\n", hi, lo, y, quo, rem)
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
		y := u64(0x4000000000000000)
		quo, rem := div_64(hi, lo, y)
		// C.printf("[%016llx_%016llx] %016llx (%016llx,%016llx)\n", hi, lo, y, quo, rem)
		assert quo == u64(2) << (x + 1)
		_, rem1 := div_64(hi % y, lo, y)
		assert rem == rem1
		assert rem == rem_64(hi, lo, y)
	}
}
