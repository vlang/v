// BEAM float implementation
// V floats are represented as Erlang floats on BEAM
module builtin

// f32_abs returns the absolute value of the `f32`.
@[inline]
pub fn f32_abs(a f32) f32 {
	return if a < 0 { -a } else { a }
}

// f64_abs returns the absolute value of the `f64`.
@[inline]
pub fn f64_abs(a f64) f64 {
	return if a < 0 { -a } else { a }
}

// f32_min returns the smaller `f32` of input `a` and `b`.
@[inline]
pub fn f32_min(a f32, b f32) f32 {
	return if a < b { a } else { b }
}

// f32_max returns the larger `f32` of input `a` and `b`.
@[inline]
pub fn f32_max(a f32, b f32) f32 {
	return if a > b { a } else { b }
}

// f64_min returns the smaller `f64` of input `a` and `b`.
@[inline]
pub fn f64_min(a f64, b f64) f64 {
	return if a < b { a } else { b }
}

// f64_max returns the larger `f64` of input `a` and `b`.
@[inline]
pub fn f64_max(a f64, b f64) f64 {
	return if a > b { a } else { b }
}

// str returns the string representation of the `f32`.
pub fn (f f32) str() string {
	return ''
}

// str returns the string representation of the `f64`.
pub fn (f f64) str() string {
	return ''
}

// strsci returns the scientific notation string representation.
pub fn (f f64) strsci(decimals int) string {
	return ''
}

// strlong returns the long decimal notation string representation.
pub fn (f f64) strlong() string {
	return ''
}

// eq_epsilon returns true if the `f32` is approximately equal to input `b`.
@[inline]
pub fn (a f32) eq_epsilon(b f32) bool {
	hi := f32_max(f32_abs(a), f32_abs(b))
	delta := f32_abs(a - b)
	if hi > f32(1.0) {
		return delta <= hi * (4 * 1.19209290e-7)
	} else {
		return (1 / (4 * 1.19209290e-7)) * delta <= hi
	}
}

// eq_epsilon returns true if the `f64` is approximately equal to input `b`.
@[inline]
pub fn (a f64) eq_epsilon(b f64) bool {
	hi := f64_max(f64_abs(a), f64_abs(b))
	delta := f64_abs(a - b)
	if hi > 1.0 {
		return delta <= hi * (4 * 2.2204460492503131e-16)
	} else {
		return (1 / (4 * 2.2204460492503131e-16)) * delta <= hi
	}
}
