// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module fit

import vsl.math
// linear computes linear fitting parameters. Errors on y-direction only
//
// y(x) = a + b⋅x
//
// See page 780 of [1]
// Reference:
// [1] Press WH, Teukolsky SA, Vetterling WT, Fnannery BP (2007) Numerical Recipes: The Art of
// Scientific Computing. Third Edition. Cambridge University Press. 1235p.
fn linear(x, y []f64) (f64,f64) {
	mut sx := f64(0)
	mut sy := f64(0)
	ndata := x.len
	// accumulate sums
	for i := 0; i < ndata; i++ {
		sx += x[i]
		sy += y[i]
	}
	// compute b
	mut b := f64(0)
	mut st2 := f64(0)
	ss := f64(ndata)
	sxoss := sx / ss
	for i := 0; i < ndata; i++ {
		t := x[i] - sxoss
		st2 += t * t
		b += t * y[i]
	}
	b /= st2
	// compute a
	a := (sy - sx * b) / ss
	return a,b
}

// linear_sigma computes linear fitting parameters and variances (sigma_a,sigma_b) in the estimates of a and b
// Errors on y-direction only
//
// y(x) = a + b⋅x
//
// See page 780 of [1]
// Reference:
// [1] Press WH, Teukolsky SA, Vetterling WT, Fnannery BP (2007) Numerical Recipes: The Art of
// Scientific Computing. Third Edition. Cambridge University Press. 1235p.
fn linear_sigma(x, y []f64) (f64,f64,f64,f64,f64) {
	mut sx := f64(0)
	mut sy := f64(0)
	ndata := x.len
	// accumulate sums
	for i := 0; i < ndata; i++ {
		sx += x[i]
		sy += y[i]
	}
	// compute b
	mut b := f64(0)
	mut st2 := f64(0)
	ss := f64(ndata)
	sxoss := sx / ss
	for i := 0; i < ndata; i++ {
		t := x[i] - sxoss
		st2 += t * t
		b += t * y[i]
	}
	b /= st2
	// compute a
	a := (sy - sx * b) / ss
	// solve for sigma_a and sigma_b
	mut sigma_a := math.sqrt((1.0 + sx * sx / (ss * st2)) / ss)
	mut sigma_b := math.sqrt(1.0 / st2)
	// calculate χ².
	mut chi_2 := f64(0)
	for i := 0; i < ndata; i++ {
		d := y[i] - a - b * x[i]
		chi_2 += d * d
	}
	// for unweighted data evaluate typical sig using chi_2,
	// and adjust the standard deviations.
	mut sigma_dat := 0.0
	if ndata > 2 {
		sigma_dat = math.sqrt(chi_2 / f64(ndata - 2))
	}
	sigma_a *= sigma_dat
	sigma_b *= sigma_dat
	return a,b,sigma_a,sigma_b,chi_2
}
