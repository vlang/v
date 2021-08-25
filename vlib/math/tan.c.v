module math

fn C.tan(x f64) f64

fn C.tanf(x f32) f32

// tan calculates tangent.
[inline]
pub fn tan(a f64) f64 {
	return C.tan(a)
}

// tanf calculates tangent. (float32)
[inline]
pub fn tanf(a f32) f32 {
	return C.tanf(a)
}
