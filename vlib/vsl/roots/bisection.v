// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module roots

import vsl.math
import vsl.errno
import vsl
/**
 * Find the root of a function using a bisection method
 *
 * @param func a function pointer
 * @param xmin lower bound
 * @param xmax upper bound
 * @param epsrel if the relative improvement over the root is less than this value,
 * then break
 * @param epsabs if the absolute improvement over the root is less than this value,
 * then break
 * @param n_max maximum number of iterations
 *
 */
pub fn bisection(func vsl.Function, xmin, xmax, epsrel, epsabs f64, n_max int) ?f64 {
	fxmin := func.safe_eval(xmin) or {
		return error(err)
	}
	fxmax := func.safe_eval(xmax) or {
		return error(err)
	}
	if (fxmin < 0.0 && fxmax < 0.0) || (fxmin > 0.0 && fxmax > 0.0) {
		return errno.vsl_error('endpoints do not straddle y=0', .einval)
	}
	mut a := xmin
	mut b := xmax
	if fxmin > 0.0 {
		a = xmax
		b = xmin
	}
	mut fa := func.safe_eval(a) or {
		return error(err)
	}
	mut fb := func.safe_eval(b) or {
		return error(err)
	}
	mut i := 0
	for i < n_max {
		c := (a + b) / 2.0
		fc := func.safe_eval(c) or {
			return error(err)
		}
		if fc < 0.0 {
			a = c
			fa = fc
		}
		else {
			b = c
			fb = fc
		}
		if math.abs(b - a) < epsabs + epsrel * math.abs(a) {
			break
		}
		i++
	}
	/* maximum number of iterations reached */

	if i == n_max {
		return error('maximum number of iterations reached')
	}
	return (a + b) / 2.0
}
