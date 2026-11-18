module math

// log_sigmoid returns log(sigmoid(x)).
pub fn log_sigmoid(x f64) f64 {
	if x >= 0 {
		return -math.log1p(math.exp(-x))
	}
	return x - math.log1p(math.exp(x))
}
