module math

fn C.pow(x f64, y f64) f64

fn C.powf(x f32, y f32) f32

// pow returns base raised to the provided power.
[inline]
pub fn pow(a f64, b f64) f64 {
	return C.pow(a, b)
}

// powf returns base raised to the provided power. (float32)
[inline]
pub fn powf(a f32, b f32) f32 {
	return C.powf(a, b)
}
