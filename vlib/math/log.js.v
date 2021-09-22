module math

fn JS.Math.log(x f64) f64

// log calculates natural (base-e) logarithm of the provided value.
[inline]
pub fn log(x f64) f64 {
	return JS.Math.log(x)
}
