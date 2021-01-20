// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fractions

import math

const (
	default_eps    = 1.0e-4
	max_iterations = 50
	zero           = fraction(0, 1)
)

// ------------------------------------------------------------------------
// Unwrapped evaluation methods for fast evaluation of continued fractions.
// ------------------------------------------------------------------------
// We need these functions because the evaluation of continued fractions
// always has to be done from the end. Also, the numerator-denominator pairs
// are generated from front to end. This means building a result from a
// previous one isn't possible. So we need unrolled versions to ensure that
// we don't take too much of a performance penalty by calling eval_cf
// several times.
// ------------------------------------------------------------------------
// eval_1 returns the result of evaluating a continued fraction series of length 1
fn eval_1(whole i64, d []i64) Fraction {
	return fraction(whole * d[0] + 1, d[0])
}

// eval_2 returns the result of evaluating a continued fraction series of length 2
fn eval_2(whole i64, d []i64) Fraction {
	den := d[0] * d[1] + 1
	return fraction(whole * den + d[1], den)
}

// eval_3 returns the result of evaluating a continued fraction series of length 3
fn eval_3(whole i64, d []i64) Fraction {
	d1d2_plus_n2 := d[1] * d[2] + 1
	den := d[0] * d1d2_plus_n2 + d[2]
	return fraction(whole * den + d1d2_plus_n2, den)
}

// eval_cf evaluates a continued fraction series and returns a Fraction.
fn eval_cf(whole i64, den []i64) Fraction {
	count := den.len
	// Offload some small-scale calculations
	// to dedicated functions
	match count {
		1 {
			return eval_1(whole, den)
		}
		2 {
			return eval_2(whole, den)
		}
		3 {
			return eval_3(whole, den)
		}
		else {
			last := count - 1
			mut n := i64(1)
			mut d := den[last]
			// The calculations are done from back to front
			for index := count - 2; index >= 0; index-- {
				t := d
				d = den[index] * d + n
				n = t
			}
			return fraction(d * whole + n, d)
		}
	}
}

// approximate returns a Fraction that approcimates the given value to
// within the default epsilon value (1.0e-4). This means the result will
// be accurate to 3 places after the decimal.
pub fn approximate(val f64) Fraction {
	return approximate_with_eps(val, default_eps)
}

// approximate_with_eps returns a Fraction
pub fn approximate_with_eps(val f64, eps f64) Fraction {
	if val == 0.0 {
		return zero
	}
	if eps < 0.0 {
		panic('Epsilon value cannot be negative.')
	}
	if math.fabs(val) > math.max_i64 {
		panic('Value out of range.')
	}
	// The integer part is separated first. Then we process the fractional
	// part to generate numerators and denominators in tandem.
	whole := i64(val)
	mut frac := val - f64(whole)
	// Quick exit for integers
	if frac == 0.0 {
		return fraction(whole, 1)
	}
	mut d := []i64{}
	mut partial := zero
	// We must complete the approximation within the maximum number of
	// itertations allowed. If we can't panic.
	// Empirically tested: the hardest constant to approximate is the
	// golden ratio (math.phi) and for f64s, it only needs 38 iterations.
	for _ in 0 .. max_iterations {
		// We calculate the reciprocal. That's why the numerator is
		// always 1.
		frac = 1.0 / frac
		den := i64(frac)
		d << den
		// eval_cf is called often so it needs to be performant
		partial = eval_cf(whole, d)
		// Check if we're done
		if math.fabs(val - partial.f64()) < eps {
			return partial
		}
		frac -= f64(den)
	}
	panic("Couldn\'t converge. Please create an issue on https://github.com/vlang/v")
}
