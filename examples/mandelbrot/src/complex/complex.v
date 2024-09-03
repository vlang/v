module complex

import math

/**
 * Minimalist and incomplete implementation of a Complex type, for learning purposes.
 * This code contains the bare minimum to allow Mandelbrot's set computation.
 */
pub struct Complex {
pub:
	re f64
	im f64
}

/**
 * (s Complex) here is called a receiver. It's basically similar to C++ this.
 * in x.f(a, b, c), x is the receiver, f the method and a,b and c the parameters.
 *
 * This method compute the modulus of s complex number.
 */
pub fn (s Complex) mod() f64 {
	return math.sqrt(s.re * s.re + s.im * s.im)
}

/**
 * Overload of the * (mul) operator for Complex struct.
 * Compute product between s and b.
 */
pub fn (s Complex) * (b Complex) Complex {
	return Complex{(s.re * b.re) - (s.im * b.im), (s.im * b.re) + (s.re * b.im)}
}

/**
 * Overload of the + (add) operator for Complex struct.
 * Compute addition between s and b.
 */
pub fn (s Complex) + (b Complex) Complex {
	return Complex{(s.re + b.re), (s.im + b.im)}
}
