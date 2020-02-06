// Copyright (c) 2019-2020 Ulises Jeremias Cornejo Fandos. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//
//
// PLEASE DO NOT MODIFY THE CONTENT OF THIS FILE. If you find potencial errors 
// or want to add new features, create an issue or make a pull request
// in the official VSL repository: https://github.com/ulises-jeremias/vsl
module deriv

import vsl.math
import vsl
import vsl.internal

fn central_deriv(f vsl.Function, x, h f64) (f64,f64,f64) {
	/* Compute the derivative using the 5-point rule (x-h, x-h/2, x,
         * x+h/2, x+h). Note that the central point is not used.
         * Compute the error using the difference between the 5-point and
         * the 3-point rule (x-h,x,x+h). Again the central point is not
         * used.
         */
	fm1 := f.eval(x - h)
	fp1 := f.eval(x + h)
	fmh := f.eval(x - h / 2)
	fph := f.eval(x + h / 2)
	r3 := 0.50 * (fp1 - fm1)
	r5 := (4.00 / 3.00) * (fph - fmh) - (1.00 / 3.00) * r3
	e3 := (math.abs(fp1) + math.abs(fm1)) * internal.f64_epsilon
	e5 := 2.00 * (math.abs(fph) + math.abs(fmh)) * internal.f64_epsilon + e3
	/* The next term is due to finite precision in x+h = O(eps * x) */

	dy := math.max(math.abs(r3 / h), math.abs(r5 / h)) * (math.abs(x) / h) * internal.f64_epsilon
	/* The truncation error in the r5 approximation itself is O(h^4).
         * However, for safety, we estimate the error from r5-r3, which is
         * O(h^2).  By scaling h we will minimise this estimated error, not
         * the actual truncation error in r5.
         */

	result := f64(r5) / h
	abserr_trunc := math.abs((r5 - r3) / h)/* Estimated truncation error O(h^2) */

	abserr_round := math.abs(e5 / h) + dy/* Rounding error (cancellations) */

	return result,abserr_trunc,abserr_round
}

pub fn central(f vsl.Function, x, h f64) (f64,f64) {
	r_0,round,trunc := central_deriv(f, x, h)
	mut error := round + trunc
	mut result := r_0
	if round < trunc && (round > 0.00 && trunc > 0.00) {
		/* Compute an optimised stepsize to minimize the total error,
                 * using the scaling of the truncation error (O(h^2)) and
                 * rounding error (O(1/h)).
                 */
		h_opt := h * math.pow(round / (2.00 * trunc), 1.00 / 3.00)
		r_opt,round_opt,trunc_opt := central_deriv(f, x, h_opt)
		error_opt := round_opt + trunc_opt
		/* Check that the new error is smaller, and that the new derivative
                 * is consistent with the error bounds of the original estimate.
                 */

		if error_opt < error && math.abs(r_opt - r_0) < 4.00 * error {
			result = r_opt
			error = error_opt
		}
	}
	return result,error
}

fn forward_deriv(f vsl.Function, x, h f64) (f64,f64,f64) {
	/* Compute the derivative using the 4-point rule (x+h/4, x+h/2,
         * x+3h/4, x+h).
         * Compute the error using the difference between the 4-point and
         * the 2-point rule (x+h/2,x+h).
         */
	f1 := f.eval(x + h / 4.0)
	f2 := f.eval(x + h / 2.0)
	f3 := f.eval(x + (3.0 / 4.0) * h)
	f4 := f.eval(x + h)
	r2 := 2.00 * (f4 - f2)
	r4 := (22.00 / 3.00) * (f4 - f3) - (62.00 / 3.00) * (f3 - f2) + (52.00 / 3.00) * (f2 - f1)
	/* Estimate the rounding error for r4 */

	e4 := 2.00 * 20.670 * (math.abs(f4) + math.abs(f3) + math.abs(f2) + math.abs(f1)) * internal.f64_epsilon
	/* The next term is due to finite precision in x+h = O(eps * x) */

	dy := math.max(math.abs(r2 / h), math.abs(r4 / h)) * math.abs(x / h) * internal.f64_epsilon
	/* The truncation error in the r4 approximation itself is O(h^3).
         * However, for safety, we estimate the error from r4-r2, which is
         * O(h).  By scaling h we will minimise this estimated error, not
         * the actual truncation error in r4.
         */

	result := f64(r4) / h
	abserr_trunc := math.abs((r4 - r2) / h)/* Estimated truncation error O(h) */

	abserr_round := math.abs(e4 / h) + dy
	return result,abserr_trunc,abserr_round
}

pub fn forward(f vsl.Function, x, h f64) (f64,f64) {
	r_0,round,trunc := forward_deriv(f, x, h)
	mut error := round + trunc
	mut result := r_0
	if round < trunc && (round > 0.00 && trunc > 0.00) {
		/* Compute an optimised stepsize to minimize the total error,
                 * using the scaling of the estimated truncation error (O(h)) and
                 * rounding error (O(1/h)).
                 */
		h_opt := h * math.pow(round / (trunc), 1.00 / 2.00)
		r_opt,round_opt,trunc_opt := forward_deriv(f, x, h_opt)
		error_opt := round_opt + trunc_opt
		/* Check that the new error is smaller, and that the new derivative
                 * is consistent with the error bounds of the original estimate.
                 */

		if error_opt < error && math.abs(r_opt - r_0) < 4.00 * error {
			result = r_opt
			error = error_opt
		}
	}
	return result,error
}

pub fn backward(f vsl.Function, x, h f64) (f64,f64) {
	return forward(f, x, -h)
}
