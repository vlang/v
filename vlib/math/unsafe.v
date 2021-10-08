// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module math

// f32_bits returns the IEEE 754 binary representation of f,
// with the sign bit of f and the result in the same bit position.
// f32_bits(f32_from_bits(x)) == x.
pub fn f32_bits(f f32) u32 {
	p := *unsafe { &u32(&f) }
	return p
}

// f32_from_bits returns the floating-point number corresponding
// to the IEEE 754 binary representation b, with the sign bit of b
// and the result in the same bit position.
// f32_from_bits(f32_bits(x)) == x.
pub fn f32_from_bits(b u32) f32 {
	p := *unsafe { &f32(&b) }
	return p
}

// f64_bits returns the IEEE 754 binary representation of f,
// with the sign bit of f and the result in the same bit position,
// and f64_bits(f64_from_bits(x)) == x.
pub fn f64_bits(f f64) u64 {
	p := *unsafe { &u64(&f) }
	return p
}

// f64_from_bits returns the floating-point number corresponding
// to the IEEE 754 binary representation b, with the sign bit of b
// and the result in the same bit position.
// f64_from_bits(f64_bits(x)) == x.
pub fn f64_from_bits(b u64) f64 {
	p := *unsafe { &f64(&b) }
	return p
}

// with_set_low_word sets low word of `f` to `lo`
pub fn with_set_low_word(f f64, lo u32) f64 {
	mut tmp := f64_bits(f)
	tmp &= 0xffffffff_00000000
	tmp |= u64(lo)
	return f64_from_bits(tmp)
}

// with_set_high_word sets high word of `f` to `lo`
pub fn with_set_high_word(f f64, hi u32) f64 {
	mut tmp := f64_bits(f)
	tmp &= 0x00000000_ffffffff
	tmp |= u64(hi) << 32
	return f64_from_bits(tmp)
}

// get_high_word returns high part of the word of `f`.
pub fn get_high_word(f f64) u32 {
	return u32(f64_bits(f) >> 32)
}
