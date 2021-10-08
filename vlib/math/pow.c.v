module math

fn C.powf(x f32, y f32) f32

// powf returns base raised to the provided power. (float32)
[inline]
pub fn powf(a f32, b f32) f32 {
	return C.powf(a, b)
}
