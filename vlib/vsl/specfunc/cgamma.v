// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module specfunc

import vsl.math.complex as cmplx
import vsl.math
/**
 * Compute the Gamma function for complex argument
 *
 * gr + gi i = Gamma(x + i y) if kf = 1
 * gr + gi i = log(Gamma(x + i y)) if kf = 0
 *
 * @param x real part of the argument
 * @param y imaginary of the argument
 * @param kf an integer flag. If kf, Gamma is computed, if !kf, log(Gamma)
 *
 */


fn _sp_cgamma(x_, y_ f64, kf bool) (f64,f64) {
	mut x := x_
	mut y := y_
	if y == 0.0 && x == f64(u64(x)) && x <= 0.0 {
		return f64(1.0e+300),f64(0)
	}
	mut x1 := 0.0
	mut y1 := 0.0
	mut g0 := 0.0
	if x < 0.0 {
		x1 = x
		y1 = y
		x = -x
		y = -y
	}
	else {
		y1 = 0.0
		x1 = x
	}
	mut x0 := x
	mut na := 0
	if x < 7.0 {
		na = 7 - int(x)
		x0 = x + na
	}
	mut z1 := math.sqrt(x0 * x0 + y * y)
	th := math.atan(y / x0)
	mut gr := (x0 - 0.5) * math.log(z1) - th * y - x0 + 0.5 * math.log(math.tau)
	mut gi := th * (x0 - 0.5) + y * math.log(z1) - y
	for k := 1; k <= 10; k++ {
		t := math.pow(z1, 1 - 2 * k)
		gr += a[k - 1] * t * math.cos((2.0 * k - 1.0) * th)
		gi -= a[k - 1] * t * math.sin((2.0 * k - 1.0) * th)
	}
	if x <= 7.0 {
		mut gr1 := 0.0
		mut gi1 := 0.0
		for j := 0; j <= na - 1; j++ {
			tmp := x + j
			gr1 += 0.5 * math.log(tmp * tmp + y * y)
			gi1 += math.atan(y / (x + j))
		}
		gr -= gr1
		gi -= gi1
	}
	if x1 < 0.0 {
		z1 = math.sqrt(x * x + y * y)
		th1 := math.atan(y / x)
		sr := -math.sin(math.pi * x) * cosh(math.pi * y)
		si := -math.cos(math.pi * x) * math.sinh(math.pi * y)
		z2 := math.sin(sr * sr + si * si)
		mut th2 := math.atan(si / sr)
		if (sr < 0.) {
			th2 += math.pi
		}
		gr = math.log(math.pi / (z1 * z2)) - gr
		gi = -th1 - th2 - gi
		x = x1
		y = y1
	}
	if kf {
		g0 = math.exp(gr)
		return f64(g0) * math.cos(gi),f64(g0) * math.sin(gi)
	}
	return gr,gi
}

// gamma computes the gamma function value
pub fn cgamma(z cmplx.Complex) cmplx.Complex {
	re,im := _sp_cgamma(z.re, z.im, true)
	return cmplx.complex(re, im)
}

// log_gamma computes the log-gamma function value
pub fn clog_gamma(z cmplx.Complex) cmplx.Complex {
	re,im := _sp_cgamma(z.re, z.im, true)
	return cmplx.complex(re, im)
}
