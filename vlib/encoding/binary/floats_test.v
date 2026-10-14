module binary

import math.bits

fn test_float32_endian_bit_patterns() {
	for pattern in [u32(0), 0x80000000, 1, 0x80000001, 0x007fffff, 0x00800000, 0x01090405, 0x3f800000,
		0xbf800000, 0x7f7fffff, 0x7f800000, 0xff800000, 0x7fc01234] {
		mut little := []u8{len: 6, init: 0x5a}
		mut big := little.clone()
		value := bits.f32_from_bits(pattern)
		little_endian_put_f32_at(mut little, value, 1)
		big_endian_put_f32_at(mut big, value, 1)
		assert little[0] == 0x5a && little[5] == 0x5a
		assert big[0] == 0x5a && big[5] == 0x5a
		for i in 0 .. 4 {
			assert little[i + 1] == u8(pattern >> (8 * i))
			assert big[i + 1] == u8(pattern >> (8 * (3 - i)))
		}
		assert bits.f32_bits(little_endian_f32_at(little, 1)) == pattern
		assert bits.f32_bits(big_endian_f32_at(big, 1)) == pattern
	}
}

fn test_float64_endian_bit_patterns() {
	for pattern in [u64(0), 0x8000000000000000, 1, 0x8000000000000001, 0x000fffffffffffff,
		0x0010000000000000, 0x3ff0000000000000, 0xbff0000000000000, 0x7fefffffffffffff,
		0x7ff0000000000000, 0xfff0000000000000, 0x7ff8000000001234] {
		mut little := []u8{len: 10, init: 0x5a}
		mut big := little.clone()
		value := bits.f64_from_bits(pattern)
		little_endian_put_f64_at(mut little, value, 1)
		big_endian_put_f64_at(mut big, value, 1)
		assert little[0] == 0x5a && little[9] == 0x5a
		assert big[0] == 0x5a && big[9] == 0x5a
		for i in 0 .. 8 {
			assert little[i + 1] == u8(pattern >> (8 * i))
			assert big[i + 1] == u8(pattern >> (8 * (7 - i)))
		}
		assert bits.f64_bits(little_endian_f64_at(little, 1)) == pattern
		assert bits.f64_bits(big_endian_f64_at(big, 1)) == pattern
	}
}
