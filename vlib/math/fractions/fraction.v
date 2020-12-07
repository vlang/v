// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fractions

import math
import math.bits

// Fraction Struct
// ---------------
// A Fraction has a numerator (n) and a denominator (d). If the user uses
// the helper functions in this module, then the following are guaranteed:
// 1. If the user provides n and d with gcd(n, d) > 1, the fraction will
// not be reduced automatically.
// 2. d cannot be set to zero. The factory function will panic.
// 3. If provided d is negative, it will be made positive. n will change as well.
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
pub fn fraction(n i64, d i64) Fraction {
	if d == 0 {
		panic('Denominator cannot be zero')
	}
	// The denominator is always guaranteed to be positive (and non-zero).
	if d < 0 {
		return fraction(-n, -d)
	}
	return Fraction{
		n: n
		d: d
		is_reduced: math.gcd(n, d) == 1
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
// NOTE: requires reduced inputs
fn general_addition_result(f1 Fraction, f2 Fraction, addition bool) Fraction {
	d1 := math.gcd(f1.d, f2.d)
	// d1 happends to be 1 around 600/(pi)^2 or 61 percent of the time (Theorem 4.5.2D)
	if d1 == 1 {
		num1n2d := f1.n * f2.d
		num1d2n := f1.d * f2.n
		n := if addition { num1n2d + num1d2n } else { num1n2d - num1d2n }
		return Fraction{
			n: n
			d: f1.d * f2.d
			is_reduced: true
		}
	}
	// Here d1 > 1.
	f1den := f1.d / d1
	f2den := f2.d / d1
	term1 := f1.n * f2den
	term2 := f2.n * f1den
	t := if addition { term1 + term2 } else { term1 - term2 }
	d2 := math.gcd(t, d1)
	return Fraction{
		n: t / d2
		d: f1den * (f2.d / d2)
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
// NOTE: requires reduced inputs
fn general_multiplication_result(f1 Fraction, f2 Fraction, multiplication bool) Fraction {
	// * Theorem: If f1 and f2 are reduced i.e. gcd(f1.n, f1.d) ==  1 and gcd(f2.n, f2.d) == 1,
	// then gcd(f1.n * f2.n, f1.d * f2.d) == gcd(f1.n, f2.d) * gcd(f1.d, f2.n)
	// * Knuth poses this an exercise for 4.5.1. - Exercise 2
	// * Also, note that:
	// The terms are flipped for multiplication and division, so the gcds must be calculated carefully
	// We do multiple divisions in order to prevent any possible overflows.
	// * One more thing:
	// if d = gcd(a, b) for example, then d divides both a and b
	if multiplication {
		d1 := math.gcd(f1.n, f2.d)
		d2 := math.gcd(f1.d, f2.n)
		return Fraction{
			n: (f1.n / d1) * (f2.n / d2)
			d: (f2.d / d1) * (f1.d / d2)
			is_reduced: true
		}
	} else {
		d1 := math.gcd(f1.n, f2.n)
		d2 := math.gcd(f1.d, f2.d)
		return Fraction{
			n: (f1.n / d1) * (f2.d / d2)
			d: (f2.n / d1) * (f1.d / d2)
			is_reduced: true
		}
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
pub fn (f Fraction) negate() Fraction {
	return Fraction{
		n: -f.n
		d: f.d
		is_reduced: f.is_reduced
	}
}

// Fraction reciprocal method
pub fn (f Fraction) reciprocal() Fraction {
	if f.n == 0 {
		panic('Denominator cannot be zero')
	}
	return Fraction{
		n: f.d
		d: f.n
		is_reduced: f.is_reduced
	}
}

// Fraction method which reduces the fraction
pub fn (f Fraction) reduce() Fraction {
	if f.is_reduced {
		return f
	}
	cf := math.gcd(f.n, f.d)
	return Fraction{
		n: f.n / cf
		d: f.d / cf
		is_reduced: true
	}
}

// f64 converts the Fraction to 64-bit floating point
pub fn (f Fraction) f64() f64 {
	return f64(f.n) / f64(f.d)
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

// cmp_i64s compares the two arguments, returns 0 when equal, 1 when the first is bigger, -1 otherwise
fn cmp_i64s(a i64, b i64) int {
	if a == b {
		return 0
	} else if a > b {
		return 1
	} else {
		return -1
	}
}

// cmp_f64s compares the two arguments, returns 0 when equal, 1 when the first is bigger, -1 otherwise
fn cmp_f64s(a f64, b f64) int {
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
fn safe_to_multiply(a i64, b i64) bool {
	return (bits.len_64(u64(abs(a))) + bits.len_64(u64(abs(b)))) < 64
}

// cmp compares the two arguments, returns 0 when equal, 1 when the first is bigger, -1 otherwise
fn cmp(f1 Fraction, f2 Fraction) int {
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
