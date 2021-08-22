module math

fn JS.Math.exp(x f64) f64

// exp calculates exponent of the number (math.pow(math.E, x)).
[inline]
pub fn exp(x f64) f64 {
	mut res := 0.0
	#res.val = Math.exp(x)

	return res
}
