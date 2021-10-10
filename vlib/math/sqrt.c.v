module math

fn C.sqrtf(x f32) f32

// sqrtf calculates square-root of the provided value. (float32)
[inline]
pub fn sqrtf(a f32) f32 {
	return C.sqrtf(a)
}
