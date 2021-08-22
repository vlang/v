module math

fn C.cbrt(x f64) f64

// cbrt calculates cubic root.
[inline]
pub fn cbrt(a f64) f64 {
	return C.cbrt(a)
}
