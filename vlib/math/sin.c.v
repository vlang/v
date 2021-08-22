module math

fn C.cos(x f64) f64

fn C.cosf(x f32) f32

fn C.sin(x f64) f64

fn C.sinf(x f32) f32

// cos calculates cosine.
[inline]
pub fn cos(a f64) f64 {
	return C.cos(a)
}

// cosf calculates cosine. (float32)
[inline]
pub fn cosf(a f32) f32 {
	return C.cosf(a)
}

// sin calculates sine.
[inline]
pub fn sin(a f64) f64 {
	return C.sin(a)
}

// sinf calculates sine. (float32)
[inline]
pub fn sinf(a f32) f32 {
	return C.sinf(a)
}
