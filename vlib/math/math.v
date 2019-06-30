// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module math

const (
	E   = 2.71828182845904523536028747135266249775724709369995957496696763
	Pi  = 3.14159265358979323846264338327950288419716939937510582097494459
	Phi = 1.61803398874989484820458683436563811772030917980576286213544862
	Tau = 6.28318530717958647692528676655900576839433879875021164194988918

	Sqrt2   = 1.41421356237309504880168872420969807856967187537694807317667974
	SqrtE   = 1.64872127070012814684865078781416357165377610071014801157507931
	SqrtPi  = 1.77245385090551602729816748334114518279754945612238712821380779
	SqrtPhi = 1.27201964951406896425242246173749149171560804184009624861664038

	Ln2    = 0.693147180559945309417232121458176568075500134360255254120680009
	Log2E  = 1.0 / Ln2
	Ln10   = 2.30258509299404568401799145468436420760110148862877297603332790
	Log10E = 1.0 / Ln10
)

// Returns the absolute value.
pub fn abs(a f64) f64 {
	if a < 0 {
		return -a
	}
	return a
}

// Inverse cosine.
pub fn acos(a f64) f64 {
	return C.acos(a)
}

// Inverse sine.
pub fn asin(a f64) f64 {
	return C.asin(a)
}

// Inverse tangent
pub fn atan(a f64) f64 {
	return C.atan(a)
}

// Inverse tangent with two arguments, returns angle between the X axis and the point.
pub fn atan2(a, b f64) f64 {
	return C.atan2(a, b)
}

// Cubic root.
pub fn cbrt(a f64) f64 {
	return C.cbrt(a)	
}

// Returns the nearest integer equal or higher to the provided value.
pub fn ceil(a f64) f64 {
	return C.ceil(a)
}

// Cosine.
pub fn cos(a f64) f64 {
	return C.cos(a)
}

// Hyperbolic cosine.
pub fn cosh(a f64) f64 {
	return C.cosh(a)
}

// Returns euler number (e) raised to the provided power.
pub fn exp(a f64) f64 {
	return C.exp(a)
}

// Returns the base-2 exponential function of x.
pub fn exp2(a f64) f64 {
	return C.exp2(a)	
}

// Returns the nearest integer equal or lower of the provided value.
pub fn floor(a f64) f64 {
	return C.floor(a)
}

// Returns the floating-point remainder of number / denom (rounded towards zero):
pub fn fmod(a, b f64) f64 {
	return C.fmod(a, b)
}

// gcd calculates greatest common (positive) divisor (or zero if x and y are both zero).
pub fn gcd(a, b int) int {
	if a < 0 {
		a = -a
	}
	if b < 0 {
		b = -b
	}
	for b != 0 {
		a %= b
		if a == 0 {
			return b
		}
		b %= a
	}
	return a
}

// lcm calculates least common (non-negative) multiple.
pub fn lcm(a, b int) int {
	if a == 0 {
		return a
	}
	res := a * (b / gcd(b, a))
	if res < 0 {
		return -res
	}
	return res
}

// Returns natural (base e) logarithm of the provided value.
pub fn log(a f64) f64 {
	return C.log(a)
}

// Returns base 2 logarithm of the provided value.
pub fn log2(a f64) f64 {
	return C.log(a) / C.log(2)	
}

// Returns the common (base-10) logarithm of x.
pub fn log10(a f64) f64 {
	return C.log10(a)
}

// Returns base N logarithm of the provided value.
pub fn log_n(a, b f64) f64 {
	return C.log(a) / C.log(b)
}

// Returns the maximum value of the two provided.
pub fn max(a, b f64) f64 {
	if a > b {
		return a
	}
	return b
}

// Returns the minimum value of all the values provided.
pub fn min(a, b f64) f64 {
	if a < b {
		return a
	}
	return b
}

// Returns base raised to the provided power.
pub fn pow(a, b f64) f64 {
	return C.pow(a, b)
}

// Radians conversion.
pub fn radians(degrees f64) f64 {
	return degrees * (Pi / 180.0)
}

// Degrees conversion.
pub fn degrees(radians f64) f64 {
	return radians * (180.0 / Pi)
}

// Returns the integer nearest to the provided value.
pub fn round(f f64) f64 {
	return C.round(f)
}

// Sine.
pub fn sin(a f64) f64 {
	return C.sin(a)
}

// Hyperbolic sine.
pub fn sinh(a f64) f64 {
	return C.sinh(a)
}

// Returns square of the provided value.
pub fn sqrt(a f64) f64 {
	return C.sqrt(a)
}
// Tangent.
pub fn tan(a f64) f64 {
	return C.tan(a)
}

// Hyperbolic tangent.
pub fn tanh(a f64) f64 {
	return C.tanh(a)
}

// Rounds a toward zero, returning the nearest integral value that is not
// larger in magnitude than a.
pub fn trunc(a f64) f64 {
	return C.trunc(a)
}

// Return the factorial of the value provided.
pub fn factorial(a int) i64 {
	mut prod := 1	
	for i:= 0; i < a; i++ {
		prod = prod * (i+1)
	}
	return prod
}

