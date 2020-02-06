// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module specfunc

import vsl.poly
import vsl.math

// bessel_i0 returns the modified Bessel function i0(x) for any real x.
fn bessel_i0(x f64) f64 {
	ax := math.abs(x)
	if ax < 15.0 { // Rational approximation.
		y := x * x
		return mbpoly(i0p, 13, y) / mbpoly(i0q, 4, 225.0-y)
	}
	// rational approximation with exp(x)/sqrt(x) factored out.
	z := 1.0 - 15.0/ax
	return math.exp(ax) * mbpoly(i0pp, 4, z) / (mbpoly(i0qq, 5, z) * math.sqrt(ax))
}

// bessel_i1 returns the modified Bessel function i1(x) for any real x.
fn bessel_i1(x f64) f64 {
	ax := math.abs(x)
	if ax < 15.0 { // Rational approximation.
		y := x * x
		return x * mbpoly(i1p, 13, y) / mbpoly(i1q, 4, 225.0-y)
	}
	// rational approximation with exp(x)/sqrt(x) factored out.
	z := 1.0 - 15.0/ax
	ans := math.exp(ax) * mbpoly(i1pp, 4, z) / (mbpoly(i1qq, 5, z) * math.sqrt(ax))
	if x > 0.0 {
		return ans
	}
	return -ans
}

// bessel_in returns the modified Bessel function in(x) for any real x and n ≥ 0
fn bessel_in(n int, x f64) f64 {
	if n == 0 {
		return bessel_i0(x)
	}
	if n == 1 {
		return bessel_i1(x)
	}
	if x*x <= 8.0*math.smallest_non_zero_f64 {
		return 0.0
	}
	acc := 200.0     // acc determines accuracy.
	iexp := 1024 / 2 // numeric_limits<double>::max_exponent/2;
	tox := 2.0 / math.abs(x)
	mut bip := 0.0
	mut bi := 1.0
	mut k := 0
	mut bim := f64(0)
        mut ans := f64(0)
	for j := 2 * (n + int(math.sqrt(acc*f64(n)))); j > 0; j-- { // Downward recurrence.
		bim = bip + f64(j)*tox*bi
		bip = bi
		bi = bim
		_, k = math.frexp(bi)
		if k > iexp { // Renormalize to prevent overflows.
			ans = math.ldexp(ans, -iexp)
			bi = math.ldexp(bi, -iexp)
			bip = math.ldexp(bip, -iexp)
		}
		if j == n {
			ans = bip
		}
	}
	ans *= bessel_i0(x) / bi // Normalize with I0.
	if x < 0.0 && (n&1) != 0 { // n&1 != 0   ⇒  is odd
		return -ans
	}
	return ans
}

// bessel_k0 returns the modified Bessel function k0(x) for positive real x.
//   special cases:
//     K0(x=0) = +inf
//     K0(x<0) = nan
fn bessel_k0(x f64) f64 {
	if x < 0 {
		return math.nan()
	}
	if x == 0 {
		return math.inf(1)
	}
	if x <= 1.0 { // Use two rational approximations.
		z := x * x
		term := mbpoly(k0pi, 4, z) * math.log(x) / mbpoly(k0qi, 2, 1.-z)
		return mbpoly(k0p, 4, z)/mbpoly(k0q, 2, 1.-z) - term
	}
	// rational approximation with exp(-x) / sqrt(x) factored out.
	z := 1.0 / x
	return math.exp(-x) * mbpoly(k0pp, 7, z) / (mbpoly(k0qq, 7, z) * math.sqrt(x))
}

// bessel_k1 returns the modified Bessel function k1(x) for positive real x.
//   special cases:
//     K0(x=0) = +inf
//     K0(x<0) = nan
fn bessel_k1(x f64) f64 {
	if x < 0 {
		return math.nan()
	}
	if x == 0 {
		return math.inf(1)
	}
	if x <= 1.0 { // Use two rational approximations.
		z := x * x
		term := mbpoly(k1pi, 4, z) * math.log(x) / mbpoly(k1qi, 2, 1.-z)
		return x*(mbpoly(k1p, 4, z)/mbpoly(k1q, 2, 1.-z)+term) + 1./x
	}
	// rational approximation with exp(-x)/sqrt(x) factored out.
	z := 1.0 / x
	return math.exp(-x) * mbpoly(k1pp, 7, z) / (mbpoly(k1qq, 7, z) * math.sqrt(x))
}

// bessel_kn returns the modified Bessel function kn(x) for positive x and n ≥ 0
fn bessel_kn(n int, x f64) f64 {
	if n == 0 {
		return bessel_k0(x)
	}
	if n == 1 {
		return bessel_k1(x)
	}
	if x < 0 {
		return math.nan()
	}
	if x == 0 {
		return math.inf(1)
	}
	tox := 2.0 / x
	mut bkm := bessel_k0(x) // Upward recurrence for all x...
	mut bk := bessel_k1(x)
	mut bkp := f64(0)
	for j := 1; j < n; j++ {
		bkp = bkm + f64(j)*tox*bk
		bkm = bk
		bk = bkp
	}
	return bk
}

// mbpoly evaluate a polynomial for the modified Bessel functions
[inline]
fn mbpoly(cof []f64, n int, x f64) f64 {
	return poly.eval(cof[..n-1], x)
}
