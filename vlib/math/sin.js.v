module math

fn JS.Math.cos(x f64) f64

fn JS.Math.sin(x f64) f64

// cos calculates cosine.
[inline]
pub fn cos(a f64) f64 {
	return JS.Math.cos(a)
}

// sin calculates sine.
[inline]
pub fn sin(a f64) f64 {
	return JS.Math.sin(a)
}
