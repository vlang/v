module math

fn C.pow(x f64, y f64) f64

fn C.powf(x f32, y f32) f32

// pow returns the base x, raised to the provided power y. (float64)
@[inline]
pub fn pow(x f64, y f64) f64 {
	return C.pow(x, y)
}

// powf returns the base a, raised to the provided power b. (float32)
@[inline]
pub fn powf(a f32, b f32) f32 {
	return C.powf(a, b)
}
