module math

// log_cosh returns log(cosh(x)).
pub fn log_cosh(x f64) f64 {
	ax := math.abs(x)
	return ax + math.log1p(math.exp(-2.0 * ax)) - math.log(2.0)
}
