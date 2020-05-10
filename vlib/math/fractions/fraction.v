// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fractions

import math
import math.bits

// Fraction Struct
// A Fraction has a numerator (n) and a denominator (d). If the user uses
// the helper functions in this module, then the following are guaranteed:
// 1.
struct Fraction {
	n          i64
	d          i64
pub:
	is_reduced bool
}

// A factory function for creating a Fraction, adds a boundary condition
// to ensure that the denominator is non-zero. It automatically converts
// the negative denominator to positive and adjusts the numerator.
// NOTE: Fractions created are not reduced by default.
pub fn fraction(n, d i64) Fraction {
	if d != 0 {
		// The denominator is always guaranteed to be positive (and non-zero).
		if d < 0 {
			return fraction(-n, -d)
		} else {
			return Fraction{
				n: n
				d: d
				is_reduced: math.gcd(n, d) == 1
			}
		}
	} else {
		panic('Denominator cannot be zero')
	}
}

// To String method
pub fn (f Fraction) str() string {
	return '$f.n/$f.d'
}

//
// + ---------------------+
// | Arithmetic functions.|
// + ---------------------+
//
// These are implemented from Knuth, TAOCP Vol 2. Section 4.5
//
// Returns a correctly reduced result for both addition and subtraction
fn general_addition_result(f1, f2 Fraction, addition bool) Fraction {
	d1 := math.gcd(f1.d, f2.d)
	// d1 happends to be 1 around 600/(pi)^2 or 61 percent of the time (Theorem 4.5.2D)
	if d1 == 1 {
		mut n := i64(0)
		num1n2d := f1.n * f2.d
		num1d2n := f1.d * f2.n
		if addition {
			n = num1n2d + num1d2n
		} else {
			n = num1n2d - num1d2n
		}
		return Fraction{
			n: n
			d: f1.d * f2.d
			is_reduced: true
		}
	}
	// Here d1 > 1.
	// Without the i64(...), t is declared as an int
	// and it does not have enough precision
	mut t := i64(0)
	term1 := f1.n * (f2.d / d1)
	term2 := f2.n * (f1.d / d1)
	if addition {
		t = term1 + term2
	} else {
		t = term1 - term2
	}
	d2 := math.gcd(t, d1)
	return Fraction{
		n: t / d2
		d: (f1.d / d1) * (f2.d / d2)
		is_reduced: true
	}
}

// Fraction add using operator overloading
pub fn (f1 Fraction) +(f2 Fraction) Fraction {
	return general_addition_result(f1.reduce(), f2.reduce(), true)
}

// Fraction subtract using operator overloading
pub fn (f1 Fraction) -(f2 Fraction) Fraction {
	return general_addition_result(f1.reduce(), f2.reduce(), false)
}

// Returns a correctly reduced result for both multiplication and division
fn general_multiplication_result(f1, f2 Fraction, multiplication bool) Fraction {
	// Theorem: If f1 and f2 are reduced i.e. gcd(f1.n, f1.d) ==  1 and gcd(f2.n, f2.d) == 1,
	// then gcd(f1.n * f2.n, f1.d * f2.d) == gcd(f1.n, f2.d) * gcd(f1.d, f2.n)
	// Knuth poses this an exercise for 4.5.1. - Exercise 2
	mut d1 := i64(0)
	mut d2 := i64(0)
	mut n := i64(0)
	mut d := i64(0)
	// The terms are flipped for multiplication and division, so the gcds must be calculated carefully
	// We do multiple divisions in order to prevent any possible overflows. Also, note that:
	// if d = gcd(a, b) for example, then d divides both a and b
	if multiplication {
		d1 = math.gcd(f1.n, f2.d)
		d2 = math.gcd(f1.d, f2.n)
		n = (f1.n / d1) * (f2.n / d2)
		d = (f2.d / d1) * (f1.d / d2)
	} else {
		d1 = math.gcd(f1.n, f2.n)
		d2 = math.gcd(f1.d, f2.d)
		n = (f1.n / d1) * (f2.d / d2)
		d = (f2.n / d1) * (f1.d / d2)
	}
	return Fraction{
		n: n
		d: d
		is_reduced: true
	}
}

// Fraction multiply using operator overloading
pub fn (f1 Fraction) *(f2 Fraction) Fraction {
	return general_multiplication_result(f1.reduce(), f2.reduce(), true)
}

// Fraction divide using operator overloading
pub fn (f1 Fraction) /(f2 Fraction) Fraction {
	if f2.n == 0 {
		panic('Cannot divive by zero')
	}
	// If the second fraction is negative, it will
	// mess up the sign. We need positive denominator
	if f2.n < 0 {
		return f1.negate() / f2.negate()
	}
	return general_multiplication_result(f1.reduce(), f2.reduce(), false)
}

// Fraction add method. Deprecated. Use the operator instead.
[deprecated]
pub fn (f1 Fraction) add(f2 Fraction) Fraction {
	return f1 + f2
}

// Fraction subtract method. Deprecated. Use the operator instead.
[deprecated]
pub fn (f1 Fraction) subtract(f2 Fraction) Fraction {
	return f1 - f2
}

// Fraction multiply method. Deprecated. Use the operator instead.
[deprecated]
pub fn (f1 Fraction) multiply(f2 Fraction) Fraction {
	return f1 * f2
}

// Fraction divide method. Deprecated. Use the operator instead.
[deprecated]
pub fn (f1 Fraction) divide(f2 Fraction) Fraction {
	return f1 / f2
}

// Fraction negate method
pub fn (f1 Fraction) negate() Fraction {
	return Fraction{
		n: -f1.n
		d: f1.d
		is_reduced: f1.is_reduced
	}
}

// Fraction reciprocal method
pub fn (f1 Fraction) reciprocal() Fraction {
	if f1.n == 0 {
		panic('Denominator cannot be zero')
	}
	return Fraction{
		n: f1.d
		d: f1.n
		is_reduced: f1.is_reduced
	}
}

// Fraction method which reduces the fraction
pub fn (f1 Fraction) reduce() Fraction {
	if f1.is_reduced {
		return f1
	}
	cf := math.gcd(f1.n, f1.d)
	return Fraction{
		n: f1.n / cf
		d: f1.d / cf
		is_reduced: true
	}
}

// f64 converts the Fraction to 64-bit floating point
pub fn (f1 Fraction) f64() f64 {
	return f64(f1.n) / f64(f1.d)
}

//
// + ------------------+
// | Utility functions.|
// + ------------------+
//
// Returns the absolute value of an i64
fn abs(num i64) i64 {
	if num < 0 {
		return -num
	} else {
		return num
	}
}

fn cmp_i64s(a, b i64) int {
	if a == b {
		return 0
	} else if a > b {
		return 1
	} else {
		return -1
	}
}

fn cmp_f64s(a, b f64) int {
	// V uses epsilon comparison internally
	if a == b {
		return 0
	} else if a > b {
		return 1
	} else {
		return -1
	}
}

// Two integers are safe to multiply when their bit lengths
// sum up to less than 64 (conservative estimate).
fn safe_to_multiply(a, b i64) bool {
	return (bits.len_64(abs(a)) + bits.len_64(abs(b))) < 64
}

fn cmp(f1, f2 Fraction) int {
	if safe_to_multiply(f1.n, f2.d) && safe_to_multiply(f2.n, f1.d) {
		return cmp_i64s(f1.n * f2.d, f2.n * f1.d)
	} else {
		return cmp_f64s(f1.f64(), f2.f64())
	}
}

// +-----------------------------+
// | Public comparison functions |
// +-----------------------------+
// equals returns true if both the Fractions are equal
pub fn (f1 Fraction) equals(f2 Fraction) bool {
	return cmp(f1, f2) == 0
}

// ge returns true if f1 >= f2
pub fn (f1 Fraction) ge(f2 Fraction) bool {
	return cmp(f1, f2) >= 0
}

// gt returns true if f1 > f2
pub fn (f1 Fraction) gt(f2 Fraction) bool {
	return cmp(f1, f2) > 0
}

// le returns true if f1 <= f2
pub fn (f1 Fraction) le(f2 Fraction) bool {
	return cmp(f1, f2) <= 0
}

// lt returns true if f1 < f2
pub fn (f1 Fraction) lt(f2 Fraction) bool {
	return cmp(f1, f2) < 0
}
