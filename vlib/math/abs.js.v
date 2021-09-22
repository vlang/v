module math

fn JS.Math.abs(x f64) f64

// Returns the absolute value.
[inline]
pub fn abs(a f64) f64 {
	return JS.Math.abs(a)
}
