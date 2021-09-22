module math

fn JS.Math.acos(x f64) f64

fn JS.Math.asin(x f64) f64

fn JS.Math.atan(x f64) f64

fn JS.Math.atan2(y f64, x f64) f64

// acos calculates inverse cosine (arccosine).
[inline]
pub fn acos(a f64) f64 {
	return JS.Math.acos(a)
}

// asin calculates inverse sine (arcsine).
[inline]
pub fn asin(a f64) f64 {
	return JS.Math.asin(a)
}

// atan calculates inverse tangent (arctangent).
[inline]
pub fn atan(a f64) f64 {
	return JS.Math.atan(a)
}

// atan2 calculates inverse tangent with two arguments, returns the angle between the X axis and the point.
[inline]
pub fn atan2(a f64, b f64) f64 {
	return JS.Math.atan2(a, b)
}
