module math

import math.bits

// Floating-point mod function.
// mod returns the floating-point remainder of x/y.
// The magnitude of the result is less than y and its
// sign agrees with that of x.
//
// special cases are:
// mod(±inf, y) = nan
// mod(nan, y) = nan
// mod(x, 0) = nan
// mod(x, ±inf) = x
// mod(x, nan) = nan
pub fn mod(x f64, y f64) f64 {
	return fmod(x, y)
}

// fmod returns the floating-point remainder of number / denom (rounded towards zero)
pub fn fmod(x f64, y f64) f64 {
	if y == 0 || is_inf(x, 0) || is_nan(x) || is_nan(y) {
		return nan()
	}
	abs_y := abs(y)
	abs_y_fr, abs_y_exp := frexp(abs_y)
	mut r := x
	if x < 0 {
		r = -x
	}
	for r >= abs_y {
		rfr, mut rexp := frexp(r)
		if rfr < abs_y_fr {
			rexp = rexp - 1
		}
		r = r - ldexp(abs_y, rexp - abs_y_exp)
	}
	if x < 0 {
		r = -r
	}
	return r
}

// gcd calculates greatest common (positive) divisor (or zero if a and b are both zero).
// It works on magnitudes in the u64 domain, so unlike plain negation it is
// safe for minimum values; unrepresentable results saturate to the type
// maximum. Supports int and signed/unsigned integer widths up to 64 bits.
pub fn gcd[T](a T, b T) T {
	if a == T(0) && b == T(0) {
		return T(0)
	}
	x := gcd_u64(u64mag(a), u64mag(b))
	if x > u64(maxof[T]()) {
		return maxof[T]()
	}
	return T(x)
}

// egcd returns (gcd(a, b), x, y) such that |a*x + b*y| = gcd(a, b)
pub fn egcd(a i64, b i64) (i64, i64, i64) {
	mut old_r, mut r := a, b
	mut old_s, mut s := i64(1), i64(0)
	mut old_t, mut t := i64(0), i64(1)

	for r != 0 {
		quot := old_r / r
		old_r, r = r, old_r % r
		old_s, s = s, old_s - quot * s
		old_t, t = t, old_t - quot * t
	}
	return if old_r < 0 { -old_r } else { old_r }, old_s, old_t
}

// lcm calculates least common (non-negative) multiple.
// Results too large to represent saturate to the type maximum instead of
// overflowing. Supports int and signed/unsigned integer widths up to 64 bits.
pub fn lcm[T](a T, b T) T {
	if a == T(0) || b == T(0) {
		return T(0)
	}
	x := u64mag(a)
	y := u64mag(b)
	qa := x / gcd_u64(x, y)
	hi, lo := bits.mul_64(qa, y)
	if hi != 0 || lo > u64(maxof[T]()) {
		return maxof[T]()
	}
	return T(lo)
}
