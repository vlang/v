module math

fn C.fabs(x f64) f64

[inline]
pub fn abs(a f64) f64 {
	return C.fabs(a)
}
