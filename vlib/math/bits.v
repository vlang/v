// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module math

const (
	uvnan = u64(0x7FF8000000000001)
	uvinf = u64(0x7FF0000000000000)
	uvneginf = u64(0xFFF0000000000000)
	uvone = u64(0x3FF0000000000000)
	mask = 0x7FF
	shift = 64 - 11 - 1
	bias = 1023
	sign_mask = (u64(1)<<63)
	frac_mask = ((u64(1)<<u64(shift)) - u64(1))
)
// inf returns positive infinity if sign >= 0, negative infinity if sign < 0.
pub fn inf(sign int) f64 {
	v := if sign >= 0 { uvinf } else { uvneginf }
	return f64_from_bits(v)
}

// nan returns an IEEE 754 ``not-a-number'' value.
pub fn nan() f64 {
	return f64_from_bits(uvnan)
}

// is_nan reports whether f is an IEEE 754 ``not-a-number'' value.
pub fn is_nan(f f64) bool {
	// IEEE 754 says that only NaNs satisfy f != f.
	// To avoid the floating-point hardware, could use:
	// x := f64_bits(f);
	// return u32(x>>shift)&mask == mask && x != uvinf && x != uvneginf
	return f != f
}

// is_inf reports whether f is an infinity, according to sign.
// If sign > 0, is_inf reports whether f is positive infinity.
// If sign < 0, is_inf reports whether f is negative infinity.
// If sign == 0, is_inf reports whether f is either infinity.
pub fn is_inf(f f64, sign int) bool {
	// Test for infinity by comparing against maximum float.
	// To avoid the floating-point hardware, could use:
	// x := f64_bits(f);
	// return sign >= 0 && x == uvinf || sign <= 0 && x == uvneginf;
	return (sign >= 0 && f > max_f64) || (sign <= 0 && f < -max_f64)
}

// NOTE: (joe-c) exponent notation is borked
// normalize returns a normal number y and exponent exp
// satisfying x == y × 2**exp. It assumes x is finite and non-zero.
// pub fn normalize(x f64) (f64, int) {
// smallest_normal := 2.2250738585072014e-308 // 2**-1022
// if abs(x) < smallest_normal {
// return x * (1 << 52), -52
// }
// return x, 0
// }
