module math

// trapz integrates y(x) using the trapezoidal rule with uniform spacing dx.
pub fn trapz(y []f64, dx f64) f64 {
	if y.len < 2 {
		return 0
	}
	mut s := (y[0] + y[y.len - 1]) / 2.0
	for i in 1 .. y.len - 1 {
		s += y[i]
	}
	return s * dx
}
