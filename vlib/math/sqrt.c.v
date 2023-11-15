module math

fn C.sqrt(x f64) f64
fn C.sqrtf(x f32) f32

// sqrt calculates square-root of the provided value. (float64)
@[inline]
pub fn sqrt(a f64) f64 {
	return C.sqrt(a)
}

// sqrtf calculates square-root of the provided value. (float32)
@[inline]
pub fn sqrtf(a f32) f32 {
	return C.sqrtf(a)
}
