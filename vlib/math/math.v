// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module math

// aprox_sin returns an approximation of sin(a) made using lolremez
pub fn aprox_sin(a f64) f64 {
	a0 := 1.91059300966915117e-31
	a1 := 1.00086760103908896
	a2 := -1.21276126894734565e-2
	a3 := -1.38078780785773762e-1
	a4 := -2.67353392911981221e-2
	a5 := 2.08026600266304389e-2
	a6 := -3.03996055049204407e-3
	a7 := 1.38235642404333740e-4
	return a0 + a * (a1 + a * (a2 + a * (a3 + a * (a4 + a * (a5 + a * (a6 + a * a7))))))
}

// aprox_cos returns an approximation of sin(a) made using lolremez
pub fn aprox_cos(a f64) f64 {
	a0 := 9.9995999154986614e-1
	a1 := 1.2548995793001028e-3
	a2 := -5.0648546280678015e-1
	a3 := 1.2942246466519995e-2
	a4 := 2.8668384702547972e-2
	a5 := 7.3726485210586547e-3
	a6 := -3.8510875386947414e-3
	a7 := 4.7196604604366623e-4
	a8 := -1.8776444013090451e-5
	return a0 + a * (a1 + a * (a2 + a * (a3 + a * (a4 + a * (a5 + a * (a6 + a * (a7 + a * a8)))))))
}

// copysign returns a value with the magnitude of x and the sign of y
[inline]
pub fn copysign(x f64, y f64) f64 {
	return f64_from_bits((f64_bits(x) & ~sign_mask) | (f64_bits(y) & sign_mask))
}

// degrees converts from radians to degrees.
[inline]
pub fn degrees(radians f64) f64 {
	return radians * (180.0 / pi)
}

[params]
pub struct DigitParams {
	base    int = 10
	reverse bool
}

// digits returns an array of the digits of n in the given base b.
// Number argument accepts any integer type (i8|i16|int|isize|i64) and will be cast to i64
// The base argument is optional, if no value is given it will default to base 10.
// digits returns an array with the digits in reverse order i.e. digits(12345, 10) == [5,4,3,2,1]
pub fn digits(num i64, params DigitParams) []int {
	// set base to 10 initially and change only if base is explicitly set.
	mut b := params.base
	if b < 2 {
		panic('digits: Cannot find digits of n with base $b')
	}
	mut n := num
	mut sign := 1
	if n < 0 {
		sign = -1
		n = -n
	}
	mut res := []int{}
	// if number passed to function is 0 then short-circuit and return 0
	if n == 0 {
		res << 0
		return res
	}
	for n != 0 {
		res << int((n % b) * sign)
		n /= b
	}

	if params.reverse {
		res = res.reverse()
	}

	return res
}

// count_digits return the number of digits in the number passed.
// Number argument accepts any integer type (i8|i16|int|isize|i64) and will be cast to i64
pub fn count_digits(n i64) int {
	return int(ceil(log(abs(f64(n) + 1)) / log(10.0)))
}

// minmax returns the minimum and maximum value of the two provided.
pub fn minmax(a f64, b f64) (f64, f64) {
	if a < b {
		return a, b
	}
	return b, a
}

// clamp returns x constrained between a and b
[inline]
pub fn clamp(x f64, a f64, b f64) f64 {
	if x < a {
		return a
	}
	if x > b {
		return b
	}
	return x
}

// sign returns the corresponding sign -1.0, 1.0 of the provided number.
// if n is not a number, its sign is nan too.
[inline]
pub fn sign(n f64) f64 {
	if is_nan(n) {
		return nan()
	}
	return copysign(1.0, n)
}

// signi returns the corresponding sign -1.0, 1.0 of the provided number.
[inline]
pub fn signi(n f64) int {
	return int(copysign(1.0, n))
}

// radians converts from degrees to radians.
[inline]
pub fn radians(degrees f64) f64 {
	return degrees * (pi / 180.0)
}

// signbit returns a value with the boolean representation of the sign for x
[inline]
pub fn signbit(x f64) bool {
	return f64_bits(x) & sign_mask != 0
}

pub fn tolerance(a f64, b f64, tol f64) bool {
	mut ee := tol
	// Multiplying by ee here can underflow denormal values to zero.
	// Check a==b so that at least if a and b are small and identical
	// we say they match.
	if a == b {
		return true
	}
	mut d := a - b
	if d < 0 {
		d = -d
	}
	// note: b is correct (expected) value, a is actual value.
	// make error tolerance a fraction of b, not a.
	if b != 0 {
		ee = ee * b
		if ee < 0 {
			ee = -ee
		}
	}
	return d < ee
}

pub fn close(a f64, b f64) bool {
	return tolerance(a, b, 1e-14)
}

pub fn veryclose(a f64, b f64) bool {
	return tolerance(a, b, 4e-16)
}

pub fn alike(a f64, b f64) bool {
	if is_nan(a) && is_nan(b) {
		return true
	} else if a == b {
		return signbit(a) == signbit(b)
	}
	return false
}

fn is_odd_int(x f64) bool {
	xi, xf := modf(x)
	return xf == 0 && (i64(xi) & 1) == 1
}

fn is_neg_int(x f64) bool {
	if x < 0 {
		_, xf := modf(x)
		return xf == 0
	}
	return false
}
