// Copyright 2019 The Go Authors. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// * Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright
//   notice, this list of conditions and the following disclaimer in the
//   documentation and/or other materials provided with the distribution.
// * Neither the name of Google LLC nor the names of its contributors may be
//   used to endorse or promote products derived from this software without
//   specific prior written permission.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
// LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
// CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
// SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
// CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
// ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
// POSSIBILITY OF SUCH DAMAGE.
//
// Adapted from https://go.dev/src/math/fma.go. Shift boundaries are explicit
// because Go defines shifts beyond a word's width, while V targets also include C.
module math

import math.bits

@[inline]
fn fma_nonzero(value u64) u64 {
	return if value == 0 { u64(0) } else { u64(1) }
}

fn fma_shl(high u64, low u64, amount u32) (u64, u64) {
	if amount == 0 { return high, low }
	if amount < 64 { return (high << amount) | (low >> (64 - amount)), low << amount }
	if amount < 128 { return low << (amount - 64), 0 }
	return 0, 0
}

// fma_shr_sticky retains a bit for every discarded nonzero bit. This lets the
// final rounding distinguish exact halfway cases from values just above them.
fn fma_shr_sticky(high u64, low u64, amount u32) (u64, u64) {
	if amount == 0 { return high, low }
	if amount < 64 {
		return high >> amount, (low >> amount) | (high << (64 - amount)) |
			fma_nonzero(low & ((u64(1) << amount) - 1))
	}
	if amount == 64 { return 0, high | fma_nonzero(low) }
	if amount < 128 {
		n := amount - 64
		return 0, (high >> n) | fma_nonzero((high & ((u64(1) << n) - 1)) | low)
	}
	return 0, fma_nonzero(high | low)
}

fn fma_leading_zeros(high u64, low u64) i32 {
	leading := bits.leading_zeros_64(high)
	return i32(if leading == 64 { 64 + bits.leading_zeros_64(low) } else { leading })
}

fn fma_split(value u64) (u32, i32, u64) {
	sign_word := u32(value >> 63)
	mut exponent := i32((value >> 52) & 0x7ff)
	mut mantissa := value & u64(0x000fffffffffffff)
	if exponent == 0 {
		amount := u32(bits.leading_zeros_64(mantissa) - 11)
		mantissa <<= amount
		exponent = 1 - i32(amount)
	} else {
		mantissa |= u64(1) << 52
	}
	return sign_word, exponent, mantissa
}

// fma_software keeps the exact product and an aligned addend in two words,
// then rounds once to nearest, ties to even. No intermediate floating-point
// product is formed for finite, nonzero inputs.
fn fma_software(x f64, y f64, z f64) f64 {
	inf_bits := u64(0x7ff0000000000000)
	bx, by, bz := f64_bits(x), f64_bits(y), f64_bits(z)
	if x == 0 || y == 0 || bx & inf_bits == inf_bits || by & inf_bits == inf_bits {
		return x * y + z
	}
	// A negative product that underflows keeps its sign even when z is +0.
	if z == 0 { return x * y }
	if bz & inf_bits == inf_bits { return z }
	xs, xe, xm := fma_split(bx)
	ys, ye, ym := fma_split(by)
	mut zs, mut ze, zm := fma_split(bz)
	mut pe := xe + ye - 1023 + 1
	mut pm1, mut pm2 := bits.mul_64(xm << 10, ym << 11)
	mut zm1, mut zm2 := zm << 10, u64(0)
	mut ps := xs ^ ys
	is_62_zero := u32((~pm1 >> 62) & 1)
	pm1, pm2 = fma_shl(pm1, pm2, is_62_zero)
	pe -= i32(is_62_zero)
	if pe < ze || (pe == ze && pm1 < zm1) {
		ps, pe, pm1, pm2, zs, ze, zm1, zm2 = zs, ze, zm1, zm2, ps, pe, pm1, pm2
	}
	if ps != zs && pe == ze && pm1 == zm1 && pm2 == zm2 { return 0.0 }
	zm1, zm2 = fma_shr_sticky(zm1, zm2, u32(pe - ze))
	mut mantissa := u64(0)
	mut carry := u64(0)
	if ps == zs {
		pm2, carry = bits.add_64(pm2, zm2, 0)
		pm1, _ = bits.add_64(pm1, zm1, carry)
		pe -= i32(~pm1 >> 63)
		pm1, mantissa = fma_shr_sticky(pm1, pm2, u32(64 + (pm1 >> 63)))
	} else {
		pm2, carry = bits.sub_64(pm2, zm2, 0)
		pm1, _ = bits.sub_64(pm1, zm1, carry)
		leading := fma_leading_zeros(pm1, pm2)
		pe -= leading
		mantissa, pm2 = fma_shl(pm1, pm2, u32(leading - 1))
		mantissa |= fma_nonzero(pm2)
	}
	if pe > 2045 || (pe == 2045 && (mantissa + (u64(1) << 9)) >> 63 == 1) {
		return f64_from_bits((u64(ps) << 63) | inf_bits)
	}
	if pe < 0 {
		amount := u32(-pe)
		mantissa = if amount >= 64 {
			fma_nonzero(mantissa)
		} else {
			(mantissa >> amount) | fma_nonzero(mantissa & ((u64(1) << amount) - 1))
		}
		pe = 0
	}
	// A halfway mantissa rounds to the even neighboring significand.
	tie := (mantissa & 0x3ff) == 0x200
	mantissa = (mantissa + 0x200) >> 10
	if tie { mantissa &= ~u64(1) }
	if mantissa == 0 { pe = 0 }
	return f64_from_bits((u64(ps) << 63) + (u64(pe) << 52) + mantissa)
}
