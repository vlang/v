module math

fn C.tanh(x f64) f64

// tanh calculates hyperbolic tangent.
[inline]
pub fn tanh(a f64) f64 {
	return C.tanh(a)
}
