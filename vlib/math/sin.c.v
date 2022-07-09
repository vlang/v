module math

fn C.cosf(x f32) f32

fn C.sinf(x f32) f32

// cosf calculates cosine in radians (float32)
[inline]
pub fn cosf(a f32) f32 {
	return C.cosf(a)
}

// sinf calculates sine in radians (float32)
[inline]
pub fn sinf(a f32) f32 {
	return C.sinf(a)
}
