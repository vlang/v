module math

// The bit-by-bit square root is adapted from https://go.dev/src/math/sqrt.go.
// Copyright 2009 The Go Authors. All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// * Redistributions of source code must retain the above copyright notice,
//   this list of conditions and the following disclaimer.
// * Redistributions in binary form must reproduce the above copyright notice,
//   this list of conditions and the following disclaimer in the documentation
//   and/or other materials provided with the distribution.
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
// The original algorithm came with this notice:
// Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
// Developed at SunPro, a Sun Microsystems, Inc. business.
// Permission to use, copy, modify, and distribute this software is freely
// granted, provided that this notice is preserved.

// sqrt returns the correctly rounded square root of a.
//
// special cases are:
// sqrt(+inf) = +inf
// sqrt(±0) = ±0
// sqrt(x < 0) = nan
// sqrt(nan) = nan
@[inline]
pub fn sqrt(a f64) f64 {
	if a == 0.0 || is_nan(a) || is_inf(a, 1) {
		return a
	}
	if a < 0.0 {
		return nan()
	}
	mut ix := f64_bits(a)
	mut exponent := int((ix >> shift) & u64(mask))
	if exponent == 0 {
		for ix & (u64(1) << shift) == 0 {
			ix <<= 1
			exponent--
		}
		exponent++
	}
	exponent -= bias
	ix = (ix & frac_mask) | (u64(1) << shift)
	if exponent & 1 != 0 {
		ix <<= 1
	}
	exponent >>= 1
	// Generate the significand and an extra rounding bit using integer arithmetic.
	ix <<= 1
	mut q := u64(0)
	mut s := u64(0)
	mut r := u64(1) << (shift + 1)
	for r != 0 {
		t := s + r
		if t <= ix {
			s = t + r
			ix -= t
			q += r
		}
		ix <<= 1
		r >>= 1
	}
	if ix != 0 {
		q += q & 1
	}
	return f64_from_bits((q >> 1) + (u64(exponent - 1 + bias) << shift))
}

// sqrtf calculates square-root of the provided value. (float32)
@[inline]
pub fn sqrtf(a f32) f32 {
	return f32(sqrt(a))
}

// sqrti calculates the integer square-root of the provided value. (i64)
pub fn sqrti(a i64) i64 {
	mut x := a
	mut q, mut r := i64(1), i64(0)
	for ; q <= x; {
		q <<= 2
	}
	for ; q > 1; {
		q >>= 2
		t := x - r - q
		r >>= 1
		if t >= 0 {
			x = t
			r += q
		}
	}
	return r
}
