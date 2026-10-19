module math

fn C.remainder(x f64, y f64) f64
fn C.fma(x f64, y f64, z f64) f64

// remainder returns the IEEE 754 remainder x - n*y, with n the nearest
// integer to x/y, ties to even. Finite x and infinite y return x; zero y,
// infinite x, or NaN operands return NaN. Zero results keep the sign of x.
pub fn remainder(x f64, y f64) f64 {
	return C.remainder(x, y)
}

// fma returns x*y + z with a single rounding, using the C math library.
pub fn fma(x f64, y f64, z f64) f64 {
	return C.fma(x, y, z)
}
