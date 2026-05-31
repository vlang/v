//
// test suite for bits and bits math functions
//
module main

import math
import math.bits
import os

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
		assert bits.leading_zeros_8(u8(u8(i) << x)) == 7 - x
	}
	assert bits.leading_zeros_8(0) == 8

	// 16 bit
	i = 1
	for x in 0 .. 16 {
		// C.printf("x:%04x lz: %d cmp: %d\n", u16(i) << x, leading_zeros_16(u16(i) << x), 15-x)
		assert bits.leading_zeros_16(u16(i) << x) == 15 - x
	}
	assert bits.leading_zeros_16(0) == 16

	// 32 bit
	i = 1
	for x in 0 .. 32 {
		// C.printf("x:%08x lz: %d cmp: %d\n", u32(i) << x, leading_zeros_32(u32(i) << x), 31-x)
		assert bits.leading_zeros_32(u32(i) << x) == 31 - x
	}
	assert bits.leading_zeros_32(0) == 32

	// 64 bit
	i = 1
	for x in 0 .. 64 {
		// C.printf("x:%016llx lz: %llu cmp: %d\n", u64(i) << x, leading_zeros_64(u64(i) << x), 63-x)
		assert bits.leading_zeros_64(u64(i) << x) == 63 - x
	}
	assert bits.leading_zeros_64(0) == 64

	//
	// --- TrailingZeros ---
	//

	// 8 bit
	i = 1
	for x in 0 .. 8 {
		assert bits.trailing_zeros_8(u8(u8(i) << x)) == x
	}
	assert bits.trailing_zeros_8(0) == 8

	// 16 bit
	i = 1
	for x in 0 .. 16 {
		assert bits.trailing_zeros_16(u16(i) << x) == x
	}
	assert bits.trailing_zeros_16(0) == 16

	// 32 bit
	i = 1
	for x in 0 .. 32 {
		assert bits.trailing_zeros_32(u32(i) << x) == x
	}
	assert bits.trailing_zeros_32(0) == 32

	// 64 bit
	i = 1
	for x in 0 .. 64 {
		assert bits.trailing_zeros_64(u64(i) << x) == x
	}
	assert bits.trailing_zeros_64(0) == 64

	//
	// --- ones_count ---
	//

	// 8 bit
	i = 0
	for x in 0 .. 9 {
		// C.printf("x:%02x lz: %llu cmp: %d\n", u8(i), ones_count_8(u8(i)), x)
		assert bits.ones_count_8(u8(i)) == x
		i = int(u32(i) << 1) + 1
	}
	assert bits.ones_count_8(0) == 0
	assert bits.ones_count_8(0xFF) == 8

	// 16 bit
	i = 0
	for x in 0 .. 17 {
		// C.printf("x:%04x lz: %llu cmp: %d\n", u16(i), ones_count_16(u16(i)), x)
		assert bits.ones_count_16(u16(i)) == x
		i = int(u32(i) << 1) + 1
	}
	assert bits.ones_count_16(0) == 0
	assert bits.ones_count_16(0xFFFF) == 16

	// 32 bit
	i = 0
	for x in 0 .. 33 {
		// C.printf("x:%08x lz: %llu cmp: %d\n", u32(i), ones_count_32(u32(i)), x)
		assert bits.ones_count_32(u32(i)) == x
		i = int(u32(i) << 1) + 1
	}
	assert bits.ones_count_32(0) == 0
	assert bits.ones_count_32(0xFFFF_FFFF) == 32

	// 64 bit
	i1 = 0
	for x in 0 .. 65 {
		// C.printf("x:%016llx lz: %llu cmp: %d\n", u64(i1), ones_count_64(u64(i1)), x)
		assert bits.ones_count_64(i1) == x
		i1 = (i1 << 1) + 1
	}
	assert bits.ones_count_64(0) == 0
	assert bits.ones_count_64(0xFFFF_FFFF_FFFF_FFFF) == 64

	//
	// --- rotate_left/right ---
	//
	assert bits.rotate_left_8(0x12, 4) == 0x21
	assert bits.rotate_left_16(0x1234, 8) == 0x3412
	assert bits.rotate_left_32(0x12345678, 16) == 0x56781234
	assert bits.rotate_left_64(0x1234567887654321, 32) == 0x8765432112345678

	//
	// --- reverse ---
	//

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
		assert bits.reverse_8(u8(i)) == rv
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
		assert bits.reverse_16(u16(i)) == rv
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
		assert bits.reverse_32(u32(i)) == rv
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
		assert bits.reverse_64(i1) == rv
		i1 = (i1 << 1) + 1
	}

	//
	// --- reverse_bytes ---
	//

	assert bits.reverse_bytes_16(0x1234) == 0x3412
	assert bits.reverse_bytes_32(0x12345678) == 0x78563412
	assert bits.reverse_bytes_64(0x1234567887654321) == 0x2143658778563412

	//
	// --- add ---
	//

	// 32 bit
	i = 1
	for x in 0 .. 32 {
		v := u32(i) << x
		sum, carry := bits.add_32(v, v, u32(0))
		assert ((u64(carry) << 32) | u64(sum)) == u64(v) + u64(v)
	}
	mut sum_32t, mut carry_32t := bits.add_32(0x8000_0000, 0x8000_0000, u32(0))
	assert sum_32t == u32(0)
	assert carry_32t == u32(1)

	sum_32t, carry_32t = bits.add_32(0xFFFF_FFFF, 0xFFFF_FFFF, u32(1))
	assert sum_32t == 0xFFFF_FFFF
	assert carry_32t == u32(1)

	// 64 bit
	i = 1
	for x in 0 .. 63 {
		v := u64(i) << x
		sum, carry := bits.add_64(v, v, u64(0))
		assert ((carry << 32) | sum) == v + v
	}
	mut sum_64t, mut carry_64t := bits.add_64(0x8000_0000_0000_0000, 0x8000_0000_0000_0000, u64(0))
	assert sum_64t == u64(0)
	assert carry_64t == u64(1)

	sum_64t, carry_64t = bits.add_64(0xFFFF_FFFF_FFFF_FFFF, 0xFFFF_FFFF_FFFF_FFFF, u64(1))
	assert sum_64t == 0xFFFF_FFFF_FFFF_FFFF
	assert carry_64t == u64(1)

	//
	// --- sub ---
	//

	// 32 bit
	i = 1
	for x in 1 .. 32 {
		v0 := u32(i) << x
		v1 := v0 >> 1
		mut diff, mut borrow_out := bits.sub_32(v0, v1, u32(0))
		assert diff == v1

		diff, borrow_out = bits.sub_32(v0, v1, u32(1))
		assert diff == (v1 - 1)
		assert borrow_out == u32(0)

		diff, borrow_out = bits.sub_32(v1, v0, u32(1))
		assert borrow_out == u32(1)
	}

	// 64 bit
	i = 1
	for x in 1 .. 64 {
		v0 := u64(i) << x
		v1 := v0 >> 1
		mut diff, mut borrow_out := bits.sub_64(v0, v1, u64(0))
		assert diff == v1

		diff, borrow_out = bits.sub_64(v0, v1, u64(1))
		assert diff == (v1 - 1)
		assert borrow_out == u64(0)

		diff, borrow_out = bits.sub_64(v1, v0, u64(1))
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
		hi, lo := bits.mul_32(v0, v1)
		assert (u64(hi) << 32) | (u64(lo)) == u64(v0) * u64(v1)
		v2 := u32(x)
		h, l := bits.mul_add_32(v0, v1, v2)
		assert (u64(h) << 32) | (u64(l)) == u64(v0) * u64(v1) + u64(v2)
	}

	// 64 bit
	i = 1
	for x in 0 .. 64 {
		v0 := u64(i) << x
		v1 := v0 - 1
		hi, lo := bits.mul_64(v0, v1)
		assert (hi & 0xFFFF_FFFF_0000_0000) == (((v0 >> 32) * (v1 >> 32)) & 0xFFFF_FFFF_0000_0000)
		assert (lo & 0x0000_0000_FFFF_FFFF) == (((v0 & 0x0000_0000_FFFF_FFFF) * (v1 & 0x0000_0000_FFFF_FFFF)) & 0x0000_0000_FFFF_FFFF)
		v2 := u64(x)
		h, l := bits.mul_add_64(v0, v1, v2)
		assert (h & 0xFFFF_FFFF_0000_0000) == (((v0 >> 32) * (v1 >> 32)) & 0xFFFF_FFFF_0000_0000)
		assert (l & 0x0000_0000_FFFF_FFFF) == ((
			(v0 & 0x0000_0000_FFFF_FFFF) * (v1 & 0x0000_0000_FFFF_FFFF) + v2) & 0x0000_0000_FFFF_FFFF)
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
		quo, rem := bits.div_32(hi, lo, y)
		tst := ((u64(hi) << 32) | u64(lo))
		assert quo == (tst / u64(y))
		assert rem == (tst % u64(y))
		assert rem == bits.rem_32(hi, lo, y)
	}

	// 64 bit
	i = 1
	for x in 0 .. 62 {
		hi := u64(i) << x
		lo := u64(2) // hi - 1
		y := u64(0x4000_0000_0000_0000)
		quo, rem := bits.div_64(hi, lo, y)
		assert quo == u64(2) << (x + 1)
		_, rem1 := bits.div_64(hi % y, lo, y)
		assert rem == rem1
		assert rem == bits.rem_64(hi, lo, y)
	}
}

fn test_div_64_edge_cases() {
	mut q, mut r := bits.div_64(10, 12, 11)
	assert q == 16769767339735956015
	assert r == 7
	q, r = bits.div_64(0, 23, 10000000000000000000)
	assert q == 0
	assert r == 23
}

fn test_float_bits_roundtrip() {
	for u32_bits in [u32(0), 0x8000_0000, 0x3f80_0000, 0x7f80_0000, 0x7fc0_0001] {
		assert math.f32_bits(math.f32_from_bits(u32_bits)) == u32_bits
	}
	for u64_bits in [u64(0), 0x8000_0000_0000_0000, 0x3ff0_0000_0000_0000, 0x7ff0_0000_0000_0000,
		0x7ff8_0000_0000_0001] {
		assert math.f64_bits(math.f64_from_bits(u64_bits)) == u64_bits
	}
}

fn test_div_panics() {
	assert_program_panics('module main\nimport math.bits\nfn main() {\n\t_, _ := bits.div_32(0, 0, 0)\n}\n')
	assert_program_panics('module main\nimport math.bits\nfn main() {\n\t_, _ := bits.div_32(1, 0, 1)\n}\n')
	assert_program_panics('module main\nimport math.bits\nfn main() {\n\t_, _ := bits.div_64(0, 0, 0)\n}\n')
	assert_program_panics('module main\nimport math.bits\nfn main() {\n\t_, _ := bits.div_64(1, 0, 1)\n}\n')
}

fn assert_program_panics(src string) {
	path := os.join_path_single(os.temp_dir(), 'bits_panic_test.v')
	f := C.fopen(path.str, c'w')
	C.fputs(src.str, f)
	C.fclose(f)
	cmd := '${@VEXE} run ${path}'
	res := unsafe { C.system(cmd.str) }
	C.remove(path.str)
	assert res != 0
}
