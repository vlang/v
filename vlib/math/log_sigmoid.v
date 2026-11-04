module math

// log_sigmoid returns log(sigmoid(x)).
pub fn log_sigmoid(x f64) f64 {
	return -math.log(1.0 + math.exp(-x))
}
