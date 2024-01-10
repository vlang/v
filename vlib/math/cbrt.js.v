module math

fn JS.Math.cbrt(x f64) f64

// cbrt calculates cubic root.
@[inline]
pub fn cbrt(a f64) f64 {
	return JS.Math.cbrt(a)
}
