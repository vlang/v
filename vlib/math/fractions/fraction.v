// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fractions

import math.big

// Fraction stores a numerator and denominator using the existing `i64` backend.
pub struct Fraction {
pub:
	n          i64
	d          i64
	is_reduced bool
}

// Rational stores a numerator and denominator using the backend type `T`.
//
// Supported backend types are signed builtin integers and `math.big.Integer`.
pub struct Rational[T] {
pub:
	n          T
	d          T
	is_reduced bool
}

// fraction creates an `i64`-backed Fraction.
//
// The denominator must be non-zero. Negative denominators are normalized so
// that the stored denominator is always positive.
pub fn fraction(n i64, d i64) Fraction {
	return rational_to_fraction(rational[i64](n, d))
}

// big_fraction creates a `math.big.Integer`-backed Rational.
//
// The denominator must be non-zero. Negative denominators are normalized so
// that the stored denominator is always positive.
pub fn big_fraction(n big.Integer, d big.Integer) Rational[big.Integer] {
	return rational[big.Integer](n, d)
}

// rational creates a Rational backed by the concrete numeric type `T`.
//
// Fractions created this way are not reduced automatically, but `is_reduced`
// reflects whether the input was already in lowest terms.
pub fn rational[T](n T, d T) Rational[T] {
	ensure_supported_backend[T]()
	if is_zero_value[T](d) {
		panic('Denominator cannot be zero')
	}
	if is_negative_value[T](d) {
		return rational[T](negate_value[T](n), negate_value[T](d))
	}
	return Rational[T]{
		n:          n
		d:          d
		is_reduced: gcd_values[T](n, d) == one_value[T]()
	}
}

// str returns the fraction in `n/d` form.
pub fn (f Fraction) str() string {
	return f.to_rational().str()
}

// Fraction add using operator overloading.
pub fn (f1 Fraction) + (f2 Fraction) Fraction {
	return rational_to_fraction(f1.to_rational() + f2.to_rational())
}

// Fraction subtract using operator overloading.
pub fn (f1 Fraction) - (f2 Fraction) Fraction {
	return rational_to_fraction(f1.to_rational() - f2.to_rational())
}

// Fraction multiply using operator overloading.
pub fn (f1 Fraction) * (f2 Fraction) Fraction {
	return rational_to_fraction(f1.to_rational() * f2.to_rational())
}

// Fraction divide using operator overloading.
pub fn (f1 Fraction) / (f2 Fraction) Fraction {
	return rational_to_fraction(f1.to_rational() / f2.to_rational())
}

// negate returns the additive inverse of the Fraction.
pub fn (f Fraction) negate() Fraction {
	return rational_to_fraction(f.to_rational().negate())
}

// reciprocal returns the reciprocal of the Fraction.
pub fn (f Fraction) reciprocal() Fraction {
	return rational_to_fraction(f.to_rational().reciprocal())
}

// reduce returns the Fraction reduced to lowest terms.
pub fn (f Fraction) reduce() Fraction {
	return rational_to_fraction(f.to_rational().reduce())
}

// f64 converts the Fraction to 64-bit floating point.
pub fn (f Fraction) f64() f64 {
	return f.to_rational().f64()
}

// return true if f1 == f2.
pub fn (f1 Fraction) == (f2 Fraction) bool {
	return cmp_fraction(f1, f2) == 0
}

// return true if f1 < f2.
pub fn (f1 Fraction) < (f2 Fraction) bool {
	return cmp_fraction(f1, f2) < 0
}

// str returns the fraction in `n/d` form.
pub fn (f Rational[T]) str() string {
	return '${f.n}/${f.d}'
}

//
// + ---------------------+
// | Arithmetic functions.|
// + ---------------------+
//
// These are implemented from Knuth, TAOCP Vol 2. Section 4.5
//
// Returns a correctly reduced result for both addition and subtraction.
// NOTE: requires reduced inputs.
fn general_addition_result[T](f1 Rational[T], f2 Rational[T], addition bool) Rational[T] {
	d1 := gcd_values[T](f1.d, f2.d)
	// d1 happens to be 1 around 600/(pi)^2 or 61 percent of the time (Theorem 4.5.2D)
	if d1 == one_value[T]() {
		num1n2d := f1.n * f2.d
		num1d2n := f1.d * f2.n
		if addition {
			return Rational[T]{
				n:          add_values[T](num1n2d, num1d2n)
				d:          f1.d * f2.d
				is_reduced: true
			}
		}
		return Rational[T]{
			n:          sub_values[T](num1n2d, num1d2n)
			d:          f1.d * f2.d
			is_reduced: true
		}
	}
	// Here d1 > 1.
	f1den := f1.d / d1
	f2den := f2.d / d1
	term1 := f1.n * f2den
	term2 := f2.n * f1den
	if addition {
		t := add_values[T](term1, term2)
		d2 := gcd_values[T](t, d1)
		return Rational[T]{
			n:          t / d2
			d:          f1den * (f2.d / d2)
			is_reduced: true
		}
	}
	t := sub_values[T](term1, term2)
	d2 := gcd_values[T](t, d1)
	return Rational[T]{
		n:          t / d2
		d:          f1den * (f2.d / d2)
		is_reduced: true
	}
}

// Fraction add using operator overloading.
pub fn (f1 Rational[T]) + (f2 Rational[T]) Rational[T] {
	return general_addition_result[T](f1.reduce(), f2.reduce(), true)
}

// Fraction subtract using operator overloading.
pub fn (f1 Rational[T]) - (f2 Rational[T]) Rational[T] {
	return general_addition_result[T](f1.reduce(), f2.reduce(), false)
}

// Returns a correctly reduced result for both multiplication and division.
// NOTE: requires reduced inputs.
fn general_multiplication_result[T](f1 Rational[T], f2 Rational[T], multiplication bool) Rational[T] {
	// * Theorem: If f1 and f2 are reduced i.e. gcd(f1.n, f1.d) ==  1 and gcd(f2.n, f2.d) == 1,
	// then gcd(f1.n * f2.n, f1.d * f2.d) == gcd(f1.n, f2.d) * gcd(f1.d, f2.n)
	// * Knuth poses this an exercise for 4.5.1. - Exercise 2
	// * Also, note that:
	// The terms are flipped for multiplication and division, so the gcds must be calculated carefully
	// We do multiple divisions in order to prevent any possible overflows.
	// * One more thing:
	// if d = gcd(a, b) for example, then d divides both a and b
	if multiplication {
		d1 := gcd_values[T](f1.n, f2.d)
		d2 := gcd_values[T](f1.d, f2.n)
		return Rational[T]{
			n:          (f1.n / d1) * (f2.n / d2)
			d:          (f2.d / d1) * (f1.d / d2)
			is_reduced: true
		}
	} else {
		d1 := gcd_values[T](f1.n, f2.n)
		d2 := gcd_values[T](f1.d, f2.d)
		return Rational[T]{
			n:          (f1.n / d1) * (f2.d / d2)
			d:          (f2.n / d1) * (f1.d / d2)
			is_reduced: true
		}
	}
}

// Fraction multiply using operator overloading.
pub fn (f1 Rational[T]) * (f2 Rational[T]) Rational[T] {
	return general_multiplication_result[T](f1.reduce(), f2.reduce(), true)
}

// Fraction divide using operator overloading.
pub fn (f1 Rational[T]) / (f2 Rational[T]) Rational[T] {
	if is_zero_value[T](f2.n) {
		panic('Cannot divide by zero')
	}
	// If the second fraction is negative, it will mess up the sign.
	// We need a positive denominator.
	if is_negative_value[T](f2.n) {
		return f1.negate() / f2.negate()
	}
	return general_multiplication_result[T](f1.reduce(), f2.reduce(), false)
}

// negate returns the additive inverse of the Rational.
pub fn (f Rational[T]) negate() Rational[T] {
	return Rational[T]{
		n:          negate_value[T](f.n)
		d:          f.d
		is_reduced: f.is_reduced
	}
}

// reciprocal returns the reciprocal of the Rational.
pub fn (f Rational[T]) reciprocal() Rational[T] {
	if is_zero_value[T](f.n) {
		panic('Denominator cannot be zero')
	}
	return rational[T](f.d, f.n)
}

// reduce returns the Rational reduced to lowest terms.
pub fn (f Rational[T]) reduce() Rational[T] {
	if f.is_reduced {
		return f
	}
	cf := gcd_values[T](f.n, f.d)
	return Rational[T]{
		n:          f.n / cf
		d:          f.d / cf
		is_reduced: true
	}
}

// f64 converts the Rational to 64-bit floating point.
pub fn (f Rational[T]) f64() f64 {
	return to_f64_value[T](f.n) / to_f64_value[T](f.d)
}

//
// + ------------------+
// | Utility functions.|
// + ------------------+
//

fn ensure_supported_backend[T]() {
	$if T is big.Integer || T is i8 || T is i16 || T is i32 || T is i64 || T is int {
	} $else {
		$compile_error('fractions: Rational only supports signed builtin integers and math.big.Integer backends')
	}
}

@[inline]
fn (f Fraction) to_rational() Rational[i64] {
	return Rational[i64]{
		n:          f.n
		d:          f.d
		is_reduced: f.is_reduced
	}
}

@[inline]
fn rational_to_fraction(r Rational[i64]) Fraction {
	return Fraction{
		n:          r.n
		d:          r.d
		is_reduced: r.is_reduced
	}
}

fn cmp_fraction(f1 Fraction, f2 Fraction) int {
	return cmp_values[big.Integer](to_big_integer[i64](f1.n) * to_big_integer[i64](f2.d),
		to_big_integer[i64](f2.n) * to_big_integer[i64](f1.d))
}

@[inline]
fn zero_value[T]() T {
	$if T is big.Integer {
		return big.zero_int
	} $else {
		return T(0)
	}
}

@[inline]
fn one_value[T]() T {
	$if T is big.Integer {
		return big.one_int
	} $else {
		return T(1)
	}
}

@[inline]
fn is_zero_value[T](value T) bool {
	return value == zero_value[T]()
}

@[inline]
fn is_negative_value[T](value T) bool {
	$if T is big.Integer {
		return value < big.zero_int
	} $else {
		return value < 0
	}
}

@[inline]
fn negate_value[T](value T) T {
	$if T is big.Integer {
		return value.neg()
	} $else {
		return -value
	}
}

@[inline]
fn abs_value[T](value T) T {
	$if T is big.Integer {
		return value.abs()
	} $else {
		return if value < 0 { -value } else { value }
	}
}

fn gcd_values[T](a T, b T) T {
	$if T is big.Integer {
		return a.gcd(b)
	} $else {
		mut x := abs_value[T](a)
		mut y := abs_value[T](b)
		for y != zero_value[T]() {
			x %= y
			if x == zero_value[T]() {
				return y
			}
			y %= x
		}
		return x
	}
}

@[inline]
fn to_f64_value[T](value T) f64 {
	$if T is big.Integer {
		return value.str().f64()
	} $else {
		return f64(value)
	}
}

@[inline]
fn to_big_integer[T](value T) big.Integer {
	$if T is big.Integer {
		return value
	} $else {
		return big.integer_from_i64(i64(value))
	}
}

@[inline]
fn add_values[T](a T, b T) T {
	return a + b
}

@[inline]
fn sub_values[T](a T, b T) T {
	return a - b
}

// cmp compares the two arguments, returns 0 when equal, 1 when the first is bigger, -1 otherwise.
fn cmp[T](f1 Rational[T], f2 Rational[T]) int {
	$if T is big.Integer {
		return cmp_values[T](f1.n * f2.d, f2.n * f1.d)
	} $else {
		return cmp_values[big.Integer](to_big_integer[T](f1.n) * to_big_integer[T](f2.d),
			to_big_integer[T](f2.n) * to_big_integer[T](f1.d))
	}
}

fn cmp_values[T](a T, b T) int {
	if a == b {
		return 0
	}
	return if a > b { 1 } else { -1 }
}

// return true if f1 == f2.
pub fn (f1 Rational[T]) == (f2 Rational[T]) bool {
	return cmp[T](f1, f2) == 0
}

// return true if f1 < f2.
pub fn (f1 Rational[T]) < (f2 Rational[T]) bool {
	return cmp[T](f1, f2) < 0
}
