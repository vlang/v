module builtin

// eq_epsilon returns true if the `f32` is equal to input `b`.
// using an epsilon of typically 1E-5 or higher (backend/compiler dependent).
// Example: assert f32(2.0).eq_epsilon(2.0)
[inline]
pub fn (a f32) eq_epsilon(b f32) bool {
	hi := f32_max(f32_abs(a), f32_abs(b))
	delta := f32_abs(a - b)
	if hi > f32(1.0) {
		return delta <= hi * (4 * f32(C.FLT_EPSILON))
	} else {
		return (1 / (4 * f32(C.FLT_EPSILON))) * delta <= hi
	}
}

// eq_epsilon returns true if the `f64` is equal to input `b`.
// using an epsilon of typically 1E-9 or higher (backend/compiler dependent).
// Example: assert f64(2.0).eq_epsilon(2.0)
[inline]
pub fn (a f64) eq_epsilon(b f64) bool {
	hi := f64_max(f64_abs(a), f64_abs(b))
	delta := f64_abs(a - b)
	if hi > 1.0 {
		return delta <= hi * (4 * f64(C.DBL_EPSILON))
	} else {
		return (1 / (4 * f64(C.DBL_EPSILON))) * delta <= hi
	}
}
