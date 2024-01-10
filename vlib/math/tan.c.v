module math

fn C.tanf(x f32) f32

// tanf calculates tangent. (float32)
@[inline]
pub fn tanf(a f32) f32 {
	return C.tanf(a)
}
