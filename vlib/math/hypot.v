module math

// hypot returns the hypotenuse of the triangle give two sides.
//
// special cases are:
// hypot(±inf, y) = +inf
// hypot(x, ±inf) = +inf
// hypot(nan, y) = nan
// hypot(x, nan) = nan
pub fn hypot(x f64, y f64) f64 {
	mut p := abs(x)
	mut q := abs(y)
	if is_inf(p, 1) || is_inf(q, 1) {
		return inf(1)
	}
	if is_nan(p) || is_nan(q) {
		return nan()
	}
	if p < q {
		p, q = q, p
	}
	if p == 0.0 {
		return 0.0
	}
	q = q / p
	return p * sqrt(1 + q * q)
}
