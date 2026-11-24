// Copyright (c) 2013 Bruce D. Evans
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions
// are met:
// 1. Redistributions of source code must retain the above copyright
//    notice unmodified, this list of conditions, and the following
//    disclaimer.
// 2. Redistributions in binary form must reproduce the above copyright
//    notice, this list of conditions and the following disclaimer in the
//    documentation and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
// IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
// IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
// INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
// NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
// DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
// THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
// (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
// THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
//
// Adapted from FreeBSD msun s_clog.c, using FMA for product residuals.
// https://github.com/freebsd/freebsd-src/blob/main/lib/msun/src/s_clog.c

module complex

import math

// two_sum retains the roundoff from adding two finite values.
fn two_sum(a f64, b f64) (f64, f64) {
	s := a + b
	v := s - a
	return s, (a - (s - v)) + (b - v)
}

// fast_two_sum requires abs(a) >= abs(b).
fn fast_two_sum(a f64, b f64) (f64, f64) {
	s := a + b
	return s, (a - s) + b
}

// Power's exponent can nearly cancel even when each product is large.
// Recover both multiplication roundoffs before the outer exponential.
fn power_product_sum(a f64, b f64, c f64, d f64) f64 {
	p := a * b
	q := c * d
	if (a == 0 || b == 0) && (c == 0 || d == 0) {
		return p + q
	}
	if math.is_nan(p) || math.is_nan(q) {
		return p + q
	}
	if math.is_inf(p, 0) || math.is_inf(q, 0) || math.abs(p) < 4.008336720017946e-292 || math.abs(q) < 4.008336720017946e-292 {
		if math.is_inf(a, 0) || math.is_inf(b, 0) || math.is_inf(c, 0) || math.is_inf(d, 0) {
			return p + q
		}
		// Normalize each product independently. Scaling the operands by a
		// shared maximum would lose crossed huge*tiny products entirely.
		fa, ea := math.frexp(a)
		fb, eb := math.frexp(b)
		fc, ec := math.frexp(c)
		fd, ed := math.frexp(d)
		e := if a == 0 || b == 0 {
			ec + ed
		} else if c == 0 || d == 0 {
			ea + eb
		} else {
			math.max(ea + eb, ec + ed)
		}
		ph := fa * fb
		qh := fc * fd
		pl := math.ldexp(math.fma(fa, fb, -ph), ea + eb - e)
		ql := math.ldexp(math.fma(fc, fd, -qh), ec + ed - e)
		s, residual := two_sum(math.ldexp(ph, ea + eb - e), math.ldexp(qh, ec + ed - e))
		return math.ldexp(s + (residual + pl + ql), e)
	}
	s, e := two_sum(p, q)
	return s + (e + math.fma(a, b, -p) + math.fma(c, d, -q))
}

fn logarithm(c Complex) Complex {
	angle := math.atan2(c.im, c.re)
	mut ax := math.abs(c.re)
	mut ay := math.abs(c.im)
	if ax < ay {
		ax, ay = ay, ax
	}
	kx := int((math.f64_bits(ax) >> 52) & 0x7ff) - 1023
	ky := int((math.f64_bits(ay) >> 52) & 0x7ff) - 1023
	if kx == 1024 || ky == 1024 {
		return Complex{math.log(math.hypot(c.re, c.im)), angle}
	}
	if ax == 1 {
		if ky < -511 {
			return Complex{(ay / 2) * ay, angle}
		}
		return Complex{math.log1p(ay * ay) / 2, angle}
	}
	if kx - ky > 53 || ay == 0 {
		return Complex{math.log(ax), angle}
	}
	ln2_hi := 6.9314718055829871e-1
	ln2_lo := 1.6465949582897082e-12
	if kx >= 1023 {
		return Complex{math.log(math.hypot(c.re * 2.2250738585072014e-308, c.im * 2.2250738585072014e-308)) + 1022 * ln2_lo + 1022 * ln2_hi, angle}
	}
	if kx >= 511 {
		return Complex{math.log(math.hypot(c.re, c.im)), angle}
	}
	if kx <= -1023 {
		return Complex{math.log(math.hypot(c.re * 8.98846567431158e307, c.im * 8.98846567431158e307)) - 1023 * ln2_lo - 1023 * ln2_hi, angle}
	}
	if ky < -458 {
		return Complex{math.log(math.hypot(c.re, c.im)), angle}
	}
	ax2h := ax * ax
	mut ax2l := math.fma(ax, ax, -ax2h)
	ay2h := ay * ay
	mut ay2l := math.fma(ay, ay, -ay2h)
	mut sh, mut sl := fast_two_sum(ax2h, ay2h)
	if sh < 0.5 || sh >= 3 {
		return Complex{math.log(ay2l + ax2l + sl + sh) / 2, angle}
	}
	sh -= 1
	sh, sl = two_sum(sh, sl)
	ax2l, ay2l = two_sum(ax2l, ay2l)
	sh, ax2l = two_sum(sh, ax2l)
	sl, ay2l = two_sum(sl, ay2l)
	mut t := ax2l + sl
	sh, t = fast_two_sum(sh, t)
	return Complex{math.log1p(ay2l + t + sh) / 2, angle}
}
