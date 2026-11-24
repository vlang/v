// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2007, 2011 David Schultz <das@FreeBSD.ORG>
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice, this list of conditions and the following disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
// ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
// ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
// OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
// HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
// LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
// OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
// SUCH DAMAGE.
//
// Adapted from FreeBSD msun s_csqrt.c, s_cexp.c, k_exp.c, s_csinh.c,
// s_ccosh.c and s_ctanh.c:
// https://github.com/freebsd/freebsd-src/tree/main/lib/msun/src

module complex

import math

// sqrt returns the principal square root, with a nonnegative real part.
// The imaginary part has the sign of c.im, including on the negative real axis.
pub fn (c Complex) sqrt() Complex {
	mut a := c.re
	mut b := c.im
	if a == 0 && b == 0 {
		return Complex{0, b}
	}
	if math.is_inf(b, 0) {
		return Complex{math.inf(1), b}
	}
	if math.is_nan(a) {
		return Complex{a, a}
	}
	if math.is_inf(a, 0) {
		if math.signbit(a) {
			return Complex{math.abs(b - b), math.copysign(a, b)}
		}
		return Complex{a, math.copysign(b - b, b)}
	}
	if math.is_nan(b) {
		return Complex{b, b}
	}
	mut scale := 1.0
	// DBL_MAX / (1 + sqrt(2)); keep the addition below finite.
	if math.abs(a) >= 7.446288774449766e307 || math.abs(b) >= 7.446288774449766e307 {
		// Leave extremely small components unscaled: rounding their quarter
		// would lose a result which may still be representable after division.
		if math.abs(a) >= 8.900295434028806e-308 {
			a *= 0.25
		}
		if math.abs(b) >= 8.900295434028806e-308 {
			b *= 0.25
		}
		scale = 2
	}
	if math.abs(a) < 2.2250738585072014e-308 && math.abs(b) < 2.2250738585072014e-308 {
		a *= 18014398509481984.0 // 2^54
		b *= 18014398509481984.0
		scale = 7.450580596923828e-9 // 2^-27
	}
	if a >= 0 {
		t := math.sqrt((a + math.hypot(a, b)) * 0.5)
		return Complex{scale * t, scale * b / (2 * t)}
	}
	t := math.sqrt((-a + math.hypot(a, b)) * 0.5)
	return Complex{scale * math.abs(b) / (2 * t), math.copysign(scale * t, b)}
}

// exp_scaled computes exp(c) * 2^adjustment for large positive real parts.
// Splitting the scale keeps tiny sine/cosine components representable even
// when exp(c.re) alone would overflow. Call only for 709.7 <= re <= 1454.9,
// with adjustment 0 or -1.
fn exp_scaled(c Complex, adjustment int) Complex {
	e := math.exp(c.re - 1246.97177782734161156) // 1799 * ln(2)
	bits := math.f64_bits(e)
	exponent := int((bits >> 52) & 0x7ff) - 2046 + 1799 + adjustment
	normalized := math.f64_from_bits((bits & 0x000fffffffffffff) | (u64(2046) << 52))
	half := exponent / 2
	scale1 := math.f64_from_bits(u64(1023 + half) << 52)
	scale2 := math.f64_from_bits(u64(1023 + exponent - half) << 52)
	return Complex{math.cos(c.im) * normalized * scale1 * scale2, math.sin(c.im) * normalized * scale1 * scale2}
}

fn exponential(c Complex) Complex {
	if c.im == 0 {
		return Complex{math.exp(c.re), c.im}
	}
	if c.re == 0 {
		return Complex{math.cos(c.im), math.sin(c.im)}
	}
	if math.is_inf(c.im, 0) || math.is_nan(c.im) {
		if math.is_inf(c.re, -1) {
			return Complex{0, 0}
		}
		if math.is_inf(c.re, 1) {
			return Complex{c.re, math.nan()}
		}
		return Complex{math.nan(), math.nan()}
	}
	high := u32(math.f64_bits(c.re) >> 32)
	if high >= 0x40862e42 && high <= 0x4096b8e4 {
		return exp_scaled(c, 0)
	}
	e := math.exp(c.re)
	return Complex{e * math.cos(c.im), e * math.sin(c.im)}
}

fn hyperbolic_sine(c Complex) Complex {
	x := c.re
	y := c.im
	ax := math.abs(x)
	if !math.is_inf(x, 0) && !math.is_nan(x) && !math.is_inf(y, 0) && !math.is_nan(y) {
		if y == 0 {
			return Complex{math.sinh(x), y}
		}
		co := math.cos(y)
		si := math.sin(y)
		if ax < 22 {
			return Complex{math.sinh(x) * co, math.cosh(x) * si}
		}
		high := u32(math.f64_bits(ax) >> 32)
		if high < 0x40862e42 {
			h := math.exp(ax) / 2
			return Complex{math.copysign(h, x) * co, h * si}
		}
		if high < 0x4096bbaa {
			w := exp_scaled(Complex{ax, y}, -1)
			return Complex{w.re * math.copysign(1, x), w.im}
		}
		return Complex{math.copysign(math.inf(1), x) * co, math.inf(1) * si}
	}
	if x == 0 {
		return Complex{x, math.nan()}
	}
	if y == 0 {
		return Complex{x, y}
	}
	if math.is_inf(x, 0) {
		if math.is_inf(y, 0) || math.is_nan(y) {
			return Complex{x, math.nan()}
		}
		return Complex{x * math.cos(y), math.inf(1) * math.sin(y)}
	}
	return Complex{math.nan(), math.nan()}
}

fn hyperbolic_cosine(c Complex) Complex {
	x := c.re
	y := c.im
	ax := math.abs(x)
	if !math.is_inf(x, 0) && !math.is_nan(x) && !math.is_inf(y, 0) && !math.is_nan(y) {
		if y == 0 {
			return Complex{math.cosh(x), x * y}
		}
		co := math.cos(y)
		si := math.sin(y)
		if ax < 22 {
			return Complex{math.cosh(x) * co, math.sinh(x) * si}
		}
		high := u32(math.f64_bits(ax) >> 32)
		if high < 0x40862e42 {
			h := math.exp(ax) / 2
			return Complex{h * co, math.copysign(h, x) * si}
		}
		if high < 0x4096bbaa {
			w := exp_scaled(Complex{ax, y}, -1)
			return Complex{w.re, w.im * math.copysign(1, x)}
		}
		return Complex{math.inf(1) * co, math.copysign(math.inf(1), x) * si}
	}
	if x == 0 {
		return Complex{math.nan(), x * math.copysign(0, y)}
	}
	if y == 0 {
		return Complex{x * x, math.copysign(0, x) * y}
	}
	if math.is_inf(x, 0) {
		if math.is_inf(y, 0) || math.is_nan(y) {
			return Complex{math.inf(1), math.nan()}
		}
		return Complex{math.inf(1) * math.cos(y), x * math.sin(y)}
	}
	return Complex{math.nan(), math.nan()}
}

fn hyperbolic_tangent(c Complex) Complex {
	x := c.re
	y := c.im
	if math.is_nan(x) {
		return Complex{math.nan(), if y == 0 { y } else { math.nan() }}
	}
	if math.is_inf(x, 0) {
		return Complex{math.copysign(1, x), math.copysign(0, if math.is_inf(y, 0) {
			y
		} else {
			math.sin(y) * math.cos(y)
		})}
	}
	if math.is_inf(y, 0) || math.is_nan(y) {
		return Complex{if x == 0 { x } else { math.nan() }, math.nan()}
	}
	if math.abs(x) >= 22 {
		e := math.exp(-math.abs(x))
		return Complex{math.copysign(1, x), 4 * math.sin(y) * math.cos(y) * e * e}
	}
	// Kahan's algorithm avoids overflowing sinh/cosh and cancellation in
	// cosh(2*x) + cos(2*y) near the imaginary-axis poles.
	t := math.tan(y)
	beta := 1 + t * t
	s := math.sinh(x)
	rho := math.sqrt(1 + s * s)
	denominator := 1 + beta * s * s
	return Complex{beta * rho * s / denominator, t / denominator}
}
