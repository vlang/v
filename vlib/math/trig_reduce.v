module math

import math.bits

// reduce_threshold, m_pi4 and the multi-word product in trig_reduce are a V
// version of trig_reduce.go from the Go standard library, which came with
// this notice:
//
// Copyright 2018 The Go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.
//
// trig_reduce ends differently: the fraction is mapped to the origin and
// multiplied by pi/4 as an integer, before it is rounded to a f64, which
// keeps the result accurate when x is close to a multiple of pi/2.

// reduce_threshold is the maximum value of x where the reduction using pi/4
// in 3 f64 parts still gives accurate results. This threshold
// is set by y*c being representable as a f64 without error
// where y is given by y = floor(x * (4 / pi)) and c is the leading partial
// terms of pi/4. Since the leading terms (p1 and p2 in sin.v) have 30
// and 32 trailing zero bits, y should have less than 30 significant bits.
//
//	y < 1<<30  -> floor(x*4/pi) < 1<<30 -> x < (1<<30 - 1) * pi/4
//
// So, conservatively we can take x < 1<<29.
// Above this threshold Payne-Hanek range reduction must be used.
// For the parts used in tan.v the threshold is not conservative: y * tan_dp1
// is exact for every x < 1<<29, and stops being so right above it.
const reduce_threshold = 5.36870912e+8 // 1 << 29

// m_pi4 is the binary digits of 4/pi as a u64 array,
// that is, 4/pi = Sum m_pi4[i]*2^(-64*i)
// 19 64-bit digits and the leading one bit give 1217 bits
// of precision to handle the largest possible f64 exponent.
const m_pi4 = [
	u64(0x0000000000000001),
	0x45f306dc9c882a53,
	0xf84eafa3ea69bb81,
	0xb6c52b3278872083,
	0xfca2c757bd778ac3,
	0x6e48dc74849ba5c0,
	0x0c925dd413a32439,
	0xfc3bd63962534e7d,
	0xd1046bea5d768909,
	0xd338e04d68befc82,
	0x7323ac7306a673e9,
	0x3908bf177bf25076,
	0x3ff12fffbc0b301f,
	0xde5e2316b414da3e,
	0xda6cfd9e4f96136e,
	0x9e8c7ecd3cbfd45a,
	0xea4f758fd7cbe2f6,
	0x7a0e73ef14a525d4,
	0xd7f6bf623f1aba10,
	0xac06608df8f6d757,
]!

// pi4_frac is pi/4 * 2^64, rounded to the nearest integer.
const pi4_frac = u64(0xc90fdaa22168c235)

// trig_reduce implements Payne-Hanek range reduction by pi/4 for x > 0.
// It returns j, the integer part of x / (pi/4) mod 8, rounded up to an even
// number, and z, the rest of x in radians, so that x = j * pi/4 + z (mod 2*pi)
// and |z| <= pi/4. For an infinite x, z is nan.
// The implementation is based on:
// "ARGUMENT REDUCTION FOR HUGE ARGUMENTS: Good to the Last Bit"
// K. C. Ng et al, March 24, 1992
// The simulated multi-precision calculation of x*b uses 64-bit integer arithmetic.
@[direct_array_access; ignore_overflow]
fn trig_reduce(x f64) (int, f64) {
	if x < pi_4 {
		return 0, x
	}
	if x > max_f64 {
		return 0, nan()
	}
	// Extract out the integer and exponent such that,
	// x = ix * 2 ** exponent.
	mut ix := f64_bits(x)
	exponent := int((ix >> shift) & mask) - bias - shift
	ix &= frac_mask
	ix |= u64(1) << shift
	// Use the exponent to extract the 3 appropriate u64 digits from m_pi4,
	// b ~ (z0, z1, z2), such that the product leading digit has the exponent -61.
	// Note, exponent >= -53 since x >= pi_4 and exponent <= 971 for the maximum f64.
	digit := u32(exponent + 61) / 64
	bitshift := u32(exponent + 61) % 64
	mut z0 := m_pi4[digit]
	mut z1 := m_pi4[digit + 1]
	mut z2 := m_pi4[digit + 2]
	if bitshift != 0 {
		z0 = (z0 << bitshift) | (z1 >> (64 - bitshift))
		z1 = (z1 << bitshift) | (z2 >> (64 - bitshift))
		z2 = (z2 << bitshift) | (m_pi4[digit + 3] >> (64 - bitshift))
	}
	// Multiply mantissa by the digits and extract the upper two digits (hi, lo).
	z2hi, _ := bits.mul_64(z2, ix)
	z1hi, z1lo := bits.mul_64(z1, ix)
	z0lo := z0 * ix
	mut lo, mut carry := bits.add_64(z1lo, z2hi, 0)
	mut hi, _ := bits.add_64(z0lo, z1hi, carry)
	// The top 3 bits are j, the 125 bits below them are the fraction.
	mut j := int(hi >> 61)
	hi = (hi << 3) | (lo >> 61)
	lo <<= 3
	// Map zeros to origin: an odd j becomes the next even one, which turns the
	// fraction f into f - 1. Its magnitude 1 - f is taken here, on all the bits
	// of f, so that an x just below a multiple of pi/2 loses none of them.
	is_odd := j & 1 == 1
	if is_odd {
		j++
		j &= 7
		// (hi, lo) = 0 - (hi, lo)
		lo, carry = bits.sub_64(0, lo, 0)
		hi, _ = bits.sub_64(0, hi, carry)
	}
	// Shift the leading one bit of the fraction to the top of hi. No f64 is
	// close enough to a multiple of pi/2 to leave more than 60 zero bits above
	// it, so hi still holds 64 bits of the fraction.
	lz := bits.leading_zeros_64(hi)
	if lz > 0 {
		hi = (hi << lz) | (lo >> (64 - lz))
	}
	// Multiply the fractional part by pi/4 and round the product to a float once.
	prod, _ := bits.mul_64(hi, pi4_frac)
	z := f64(prod) * f64_from_bits(u64(bias - 64 - lz) << shift)
	if is_odd {
		return j, -z
	}
	return j, z
}
