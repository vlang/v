module math

fn C.fmod(x f64, y f64) f64

// fmod returns the floating-point remainder of number / denom (rounded towards zero):
[inline]
pub fn fmod(x f64, y f64) f64 {
	return C.fmod(x, y)
}
