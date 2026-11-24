// SPDX-License-Identifier: BSD-2-Clause
// Copyright (c) 2012 Stephen Montgomery-Smith <stephen@FreeBSD.ORG>
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
// Adapted from FreeBSD msun catrig.c:
// https://github.com/freebsd/freebsd-src/blob/main/lib/msun/src/catrig.c
// Based on Hull, Fairgrieve and Tang, "Implementing the complex arcsine
// and arccosine functions using exception handling", ACM TOMS 23(3), 1997.
// This translation preserves result and branch-cut conventions; it does not
// explicitly raise floating-point status flags.

module complex

import math

const inverse_epsilon = 2.220446049250313e-16
const inverse_recip_epsilon = 4503599627370496.0
const inverse_four_sqrt_min = 5.966672584960166e-154 // 2^-509
const inverse_sqrt_min = 1.4916681462400413e-154 // 2^-511
const inverse_pio2_lo = 6.1232339957367659e-17

struct InverseWork {
mut:
	real_part    f64
	ratio_usable bool
	ratio        f64
	root         f64
	new_y        f64
}

// inverse_difference computes (hypot(a,b)-b)/2 without cancellation.
fn inverse_difference(a f64, b f64, hyp f64) f64 {
	if b < 0 {
		return (hyp - b) / 2
	}
	if b == 0 {
		return a / 2
	}
	return a * a / (hyp + b) / 2
}

// inverse_work assumes nonnegative x,y <= 1/epsilon. Computing A-1
// and A-y separately avoids subtracting nearly equal rounded hypot values.
fn inverse_work(x f64, y f64) InverseWork {
	r := math.hypot(x, y + 1)
	s := math.hypot(x, y - 1)
	a := math.max(1.0, (r + s) / 2)
	mut w := InverseWork{ new_y: y }
	if a < 10 {
		if y == 1 && x < inverse_epsilon * inverse_epsilon / 128 {
			w.real_part = math.sqrt(x)
		} else if x >= inverse_epsilon * math.abs(y - 1) {
			am1 := inverse_difference(x, 1 + y, r) + inverse_difference(x, 1 - y, s)
			w.real_part = math.log1p(am1 + math.sqrt(am1 * (a + 1)))
		} else if y < 1 {
			w.real_part = x / math.sqrt((1 - y) * (1 + y))
		} else {
			w.real_part = math.log1p((y - 1) + math.sqrt((y - 1) * (y + 1)))
		}
	} else {
		w.real_part = math.log(a + math.sqrt(a * a - 1))
	}
	if y < inverse_four_sqrt_min {
		w.root = a * (2 / inverse_epsilon)
		w.new_y = y * (2 / inverse_epsilon)
		return w
	}
	w.ratio = y / a
	w.ratio_usable = true
	if w.ratio > 0.6417 {
		w.ratio_usable = false
		if y == 1 && x < inverse_epsilon / 128 {
			w.root = math.sqrt(x) * math.sqrt((a + y) / 2)
		} else if x >= inverse_epsilon * math.abs(y - 1) {
			amy := inverse_difference(x, y + 1, r) + inverse_difference(x, y - 1, s)
			w.root = math.sqrt(amy * (a + y))
		} else if y > 1 {
			w.root = x * (4 / inverse_epsilon / inverse_epsilon) * y / math.sqrt((y + 1) * (y - 1))
			w.new_y = y * (4 / inverse_epsilon / inverse_epsilon)
		} else {
			w.root = math.sqrt((1 - y) * (1 + y))
		}
	}
	return w
}

fn inverse_hyperbolic_sine(c Complex) Complex {
	x := c.re
	y := c.im
	ax := math.abs(x)
	ay := math.abs(y)
	if math.is_nan(x) || math.is_nan(y) {
		if math.is_inf(x, 0) {
			return Complex{x, y}
		}
		if math.is_inf(y, 0) {
			return Complex{y, x}
		}
		if y == 0 {
			return Complex{x, y}
		}
		return Complex{math.nan(), math.nan()}
	}
	if ax > inverse_recip_epsilon || ay > inverse_recip_epsilon {
		w := logarithm(Complex{ax, ay})
		return Complex{math.copysign(w.re + math.ln2, x), math.copysign(w.im, y)}
	}
	if ax < 9.125060374972143e-9 && ay < 9.125060374972143e-9 {
		return c
	}
	w := inverse_work(ax, ay)
	im := if w.ratio_usable { math.asin(w.ratio) } else { math.atan2(w.new_y, w.root) }
	return Complex{math.copysign(w.real_part, x), math.copysign(im, y)}
}

fn inverse_cosine(c Complex) Complex {
	x := c.re
	y := c.im
	ax := math.abs(x)
	ay := math.abs(y)
	if math.is_nan(x) || math.is_nan(y) {
		if math.is_inf(x, 0) {
			return Complex{math.nan(), math.inf(-1)}
		}
		if math.is_inf(y, 0) {
			return Complex{math.nan(), -y}
		}
		if x == 0 {
			return Complex{math.pi / 2, math.nan()}
		}
		return Complex{math.nan(), math.nan()}
	}
	if ax > inverse_recip_epsilon || ay > inverse_recip_epsilon {
		w := logarithm(c)
		return Complex{math.abs(w.im), -math.copysign(w.re + math.ln2, y)}
	}
	if x == 1 && y == 0 {
		return Complex{0, -y}
	}
	if ax < 9.125060374972143e-9 && ay < 9.125060374972143e-9 {
		return Complex{math.pi / 2 - (x - inverse_pio2_lo), -y}
	}
	w := inverse_work(ay, ax)
	mut re := 0.0
	if w.ratio_usable {
		re = math.acos(math.copysign(w.ratio, x))
	} else {
		re = math.atan2(w.root, math.copysign(w.new_y, x))
	}
	return Complex{re, -math.copysign(w.real_part, y)}
}

// real_reciprocal avoids squaring huge operands or losing a small numerator.
fn real_reciprocal(x f64, y f64) f64 {
	ix := int((math.f64_bits(x) >> 52) & 0x7ff)
	iy := int((math.f64_bits(y) >> 52) & 0x7ff)
	if ix - iy >= 27 || math.is_inf(x, 0) {
		return 1 / x
	}
	if iy - ix >= 27 {
		return x / y / y
	}
	if ix <= 1508 {
		return x / (x * x + y * y)
	}
	scale := math.f64_from_bits(u64(2047 - ix) << 52)
	a := x * scale
	b := y * scale
	return a / (a * a + b * b) * scale
}

fn inverse_hyperbolic_tangent(c Complex) Complex {
	x := c.re
	y := c.im
	ax := math.abs(x)
	ay := math.abs(y)
	if y == 0 && ax <= 1 {
		return Complex{math.atanh(x), y}
	}
	if x == 0 {
		return Complex{x, math.atan(y)}
	}
	if math.is_nan(x) || math.is_nan(y) {
		if math.is_inf(x, 0) {
			return Complex{math.copysign(0, x), math.nan()}
		}
		if math.is_inf(y, 0) {
			return Complex{math.copysign(0, x), math.copysign(math.pi / 2, y)}
		}
		return Complex{math.nan(), math.nan()}
	}
	if ax > inverse_recip_epsilon || ay > inverse_recip_epsilon {
		return Complex{real_reciprocal(x, y), math.copysign(math.pi / 2, y)}
	}
	if ax < 1.2904784139758924e-8 && ay < 1.2904784139758924e-8 {
		return c
	}
	mut re := 0.0
	if ax == 1 && ay < inverse_epsilon {
		re = (math.ln2 - math.log(ay)) / 2
	} else {
		square := (ax - 1) * (ax - 1)
		denominator := if ay < inverse_sqrt_min { square } else { square + ay * ay }
		re = math.log1p(4 * ax / denominator) / 4
	}
	mut im := 0.0
	if ax == 1 {
		im = math.atan2(2, -ay) / 2
	} else if ay < inverse_epsilon {
		im = math.atan2(2 * ay, (1 - ax) * (1 + ax)) / 2
	} else {
		im = math.atan2(2 * ay, (1 - ax) * (1 + ax) - ay * ay) / 2
	}
	return Complex{math.copysign(re, x), math.copysign(im, y)}
}
