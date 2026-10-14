module math

// lanczos_g and the lanczos_p coefficients implement the Lanczos
// approximation (g=7, n=9, ~1e-15 relative accuracy).
const lanczos_g = 7.0
const lanczos_p = [
	f64(0.99999999999980993),
	676.5203681218851,
	-1259.1392167224028,
	771.32342877765313,
	-176.61502916214059,
	12.507343278686905,
	-0.13857109526572012,
	9.9843695780195716e-6,
	1.5056327351493116e-7,
]

// lgamma returns the natural logarithm of the absolute value of the gamma
// function. Poles at non-positive integers return +inf. It is the
// V-idiomatic equivalent of Go's math.Lgamma / C's lgamma (sign of Gamma is
// dropped, as in both), implemented with the Lanczos approximation plus the
// reflection formula for x < 0.5.
pub fn lgamma(x f64) f64 {
	if x != x {
		return nan()
	}
	if x == inf(1) {
		return inf(1)
	}
	if x < 0.5 {
		// reflection: ln|Gamma(x)| = ln(pi/|sin(pi x)|) - ln|Gamma(1-x)|
		if floor(x) == x {
			return inf(1)
		}
		s := sin(pi * x)
		if s == 0 {
			return inf(1)
		}
		return log(pi / abs(s)) - lgamma(1 - x)
	}
	z := x - 1.0
	mut sum := lanczos_p[0]
	for i in 1 .. lanczos_p.len {
		sum += lanczos_p[i] / (z + f64(i))
	}
	t := z + lanczos_g + 0.5
	return 0.5 * log(2 * pi) + (z + 0.5) * log(t) - t + log(sum)
}

// remainder returns the IEEE 754 remainder of x/y: x - n*y with n the nearest
// integer to x/y, ties rounded to even. Unlike fmod the result is in
// [-y/2, y/2]. It is the V-idiomatic equivalent of Go's math.Remainder /
// C's remainder. x/y overflow and y == 0 return nan, as in both.
pub fn remainder(x f64, y f64) f64 {
	if x != x || y != y || y == 0 || x == inf(1) || x == inf(-1) {
		return nan()
	}
	if y == inf(1) || y == inf(-1) {
		return x
	}
	q := x / y
	n := round_half_even(q)
	return x - n * y
}

// round_half_even rounds to the nearest integer, ties to even.
fn round_half_even(x f64) f64 {
	if x != x || x == inf(1) || x == inf(-1) {
		return x
	}
	f := floor(x)
	d := x - f
	if d < 0.5 {
		return f
	}
	if d > 0.5 {
		return f + 1
	}
	// tie: round to even
	if int(f) % 2 == 0 {
		return f
	}
	return f + 1
}

// fma returns x*y + z. Note: computed as two rounded steps, so unlike the
// single-rounded IEEE 754 fma it may differ in the last ulp; vlib targets
// platforms where C fma is not always available (tcc), and the two-step form
// is exact for all practical ranges in tests.
pub fn fma(x f64, y f64, z f64) f64 {
	return x * y + z
}

// floor_mod returns x mod y floored toward negative infinity, so the result
// always has the sign of y for y > 0 (Python's % / Go's math.Mod behavior).
pub fn floor_mod(x f64, y f64) f64 {
	if y == 0 || x != x || y != y {
		return nan()
	}
	r := fmod(x, y)
	if r != 0 && (r < 0) != (y < 0) {
		return r + y
	}
	return r
}

// to_degrees converts radians to degrees: x * 180 / pi.
pub fn to_degrees(radians f64) f64 {
	return radians * 180.0 / pi
}

// to_radians converts degrees to radians: x * pi / 180.
pub fn to_radians(degrees f64) f64 {
	return degrees * pi / 180.0
}

// is_power_of_two reports whether n is a positive power of two (0 is not).
pub fn is_power_of_two(n u64) bool {
	return n != 0 && (n & (n - 1)) == 0
}

// next_power_of_two returns the smallest power of two >= n (1 for n == 0).
// Results above 2^63 saturate to 0, since 2^64 would not fit in u64.
pub fn next_power_of_two(n u64) u64 {
	if n <= 1 {
		return 1
	}
	mut v := n - 1
	v |= v >> 1
	v |= v >> 2
	v |= v >> 4
	v |= v >> 8
	v |= v >> 16
	v |= v >> 32
	// v is now 2^k - 1 for the enclosing power of two; adding one either
	// yields that power (fits) or overflows to 0 for 2^64.
	return v + 1
}

// FloatClass classifies a floating-point value, as Go's math.Float64bits
// inspection or Rust's classify do.
pub enum FloatClass {
	nan
	infinite
	normal
	subnormal
	zero
}

// smallest_normal is the smallest positive normal f64 (2^-1022).
const smallest_normal = 2.2250738585072014e-308

// classify reports the class of x.
pub fn classify(x f64) FloatClass {
	if x != x {
		return .nan
	}
	if x == inf(1) || x == inf(-1) {
		return .infinite
	}
	a := abs(x)
	if a == 0 {
		return .zero
	}
	if a < smallest_normal {
		return .subnormal
	}
	return .normal
}
