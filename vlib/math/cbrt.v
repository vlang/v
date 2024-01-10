module math

// The vlang code is a modified version of the original C code from
// http://www.netlib.org/fdlibm/s_cbrt.c and came with this notice.
//
// ====================================================
// Copyright (C) 1993 by Sun Microsystems, Inc. All rights reserved.
//
// Developed at SunSoft, a Sun Microsystems, Inc. business.
// Permission to use, copy, modify, and distribute this
// software is freely granted, provided that this notice
// is preserved.
// ====================================================

// cbrt returns the cube root of a.
//
// special cases are:
// cbrt(±0) = ±0
// cbrt(±inf) = ±inf
// cbrt(nan) = nan
pub fn cbrt(a f64) f64 {
	mut x := a
	b1 := 715094163 // (682-0.03306235651)*2**20
	b2 := 696219795 // (664-0.03306235651)*2**20
	c := 5.42857142857142815906e-01 // 19/35     = 0x3FE15F15F15F15F1
	d := -7.05306122448979611050e-01 // -864/1225 = 0xBFE691DE2532C834
	e_ := 1.41428571428571436819e+00 // 99/70     = 0x3FF6A0EA0EA0EA0F
	f := 1.60714285714285720630e+00 // 45/28     = 0x3FF9B6DB6DB6DB6E
	g := 3.57142857142857150787e-01 // 5/14      = 0x3FD6DB6DB6DB6DB7
	smallest_normal := 2.22507385850720138309e-308 // 2**-1022  = 0x0010000000000000
	if x == 0.0 || is_nan(x) || is_inf(x, 0) {
		return x
	}
	mut sign := false
	if x < 0 {
		x = -x
		sign = true
	}
	// rough cbrt to 5 bits
	mut t := f64_from_bits(f64_bits(x) / u64(3) + (u64(b1) << 32))
	if x < smallest_normal {
		// subnormal number
		t = f64(u64(1) << 54) // set t= 2**54
		t *= x
		t = f64_from_bits(f64_bits(t) / u64(3) + (u64(b2) << 32))
	}
	// new cbrt to 23 bits
	mut r := t * t / x
	mut s := c + r * t
	t *= g + f / (s + e_ + d / s)
	// chop to 22 bits, make larger than cbrt(x)
	t = f64_from_bits(f64_bits(t) & (u64(0xffffffffc) << 28) + (u64(1) << 30))
	// one step newton iteration to 53 bits with error less than 0.667ulps
	s = t * t // t*t is exact
	r = x / s
	w := t + t
	r = (r - t) / (w + r) // r-s is exact
	t = t + t * r
	// restore the sign bit
	if sign {
		t = -t
	}
	return t
}
