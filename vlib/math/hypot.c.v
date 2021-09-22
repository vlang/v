module math

fn C.hypot(x f64, y f64) f64

// Returns hypotenuse of a right triangle.
[inline]
pub fn hypot(x f64, y f64) f64 {
	return C.hypot(x, y)
}
