// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module diff

import vsl.math
import vsl
import vsl.internal

pub fn backward(f vsl.Function, x f64) (f64,f64) {
	/* Construct a divided difference table with a fairly large step
         * size to get a very rough estimate of f''. Use this to estimate
         * the step size which will minimize the error in calculating f'.
         */
	mut h := internal.sqrt_f64_epsilon
	mut a := []f64
	mut d := []f64
	mut k := 0
	mut i := 0
	/* Algorithm based on description on pg. 204 of Conte and de Boor
         * (CdB) - coefficients of Newton form of polynomial of degree 2.
         */

	for i = 0; i < 3; i++ {
		a << x + (f64(i) - 2.0) * h
		d << f.eval(a[i])
	}
	for k = 1; k < 4; k++ {
		for i = 0; i < 3 - k; i++ {
			d[i] = (d[i + 1] - d[i]) / (a[i + k] - a[i])
		}
	}
	/* Adapt procedure described on pg. 282 of CdB to find best value of
         * step size.
         */

	mut a2 := math.abs(d[0] + d[1] + d[2])
	if a2 < 100.0 * internal.sqrt_f64_epsilon {
		a2 = 100.0 * internal.sqrt_f64_epsilon
	}
	h = math.sqrt(internal.sqrt_f64_epsilon / (2.0 * a2))
	if h > 100.0 * internal.sqrt_f64_epsilon {
		h = 100.0 * internal.sqrt_f64_epsilon
	}
	return (f.eval(x) - f.eval(x - h)) / h,math.abs(10.0 * a2 * h)
}

pub fn forward(f vsl.Function, x f64) (f64,f64) {
	/* Construct a divided difference table with a fairly large step
         * size to get a very rough estimate of f''. Use this to estimate
         * the step size which will minimize the error in calculating f'.
         */
	mut h := internal.sqrt_f64_epsilon
	mut a := []f64
	mut d := []f64
	mut k := 0
	mut i := 0
	/* Algorithm based on description on pg. 204 of Conte and de Boor
         * (CdB) - coefficients of Newton form of polynomial of degree 2.
         */

	for i = 0; i < 3; i++ {
		a << x + f64(i) * h
		d << f.eval(a[i])
	}
	for k = 1; k < 4; k++ {
		for i = 0; i < 3 - k; i++ {
			d[i] = (d[i + 1] - d[i]) / (a[i + k] - a[i])
		}
	}
	/* Adapt procedure described on pg. 282 of CdB to find best value of
         * step size.
         */

	mut a2 := math.abs(d[0] + d[1] + d[2])
	if a2 < 100.0 * internal.sqrt_f64_epsilon {
		a2 = 100.0 * internal.sqrt_f64_epsilon
	}
	h = math.sqrt(internal.sqrt_f64_epsilon / (2.0 * a2))
	if h > 100.0 * internal.sqrt_f64_epsilon {
		h = 100.0 * internal.sqrt_f64_epsilon
	}
	return (f.eval(x + h) - f.eval(x)) / h,math.abs(10.0 * a2 * h)
}

pub fn central(f vsl.Function, x f64) (f64,f64) {
	/* Construct a divided difference table with a fairly large step
         * size to get a very rough estimate of f'''. Use this to estimate
         * the step size which will minimize the error in calculating f'.
         */
	mut h := internal.sqrt_f64_epsilon
	mut a := []f64
	mut d := []f64
	mut k := 0
	mut i := 0
	/* Algorithm based on description on pg. 204 of Conte and de Boor
         * (CdB) - coefficients of Newton form of polynomial of degree 3.
         */

	for i = 0; i < 4; i++ {
		a << x + (f64(i) - 2.0) * h
		d << f.eval(a[i])
	}
	for k = 1; k < 5; k++ {
		for i = 0; i < 3 - k; i++ {
			d[i] = (d[i + 1] - d[i]) / (a[i + k] - a[i])
		}
	}
	/* Adapt procedure described on pg. 282 of CdB to find best value of
         * step size.
         */

	mut a3 := math.abs(d[0] + d[1] + d[2] + d[3])
	if a3 < 100.0 * internal.sqrt_f64_epsilon {
		a3 = 100.0 * internal.sqrt_f64_epsilon
	}
	h = math.pow(internal.sqrt_f64_epsilon / (2.0 * a3), 1.0 / 3.0)
	if h > 100.0 * internal.sqrt_f64_epsilon {
		h = 100.0 * internal.sqrt_f64_epsilon
	}
	return (f.eval(x + h) - f.eval(x - h)) / (2.0 * h),math.abs(100.0 * a3 * h * h)
}
