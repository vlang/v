module math

fn C.tgamma(x f64) f64

fn C.lgamma(x f64) f64

// gamma computes the gamma function value
[inline]
pub fn gamma(a f64) f64 {
	return C.tgamma(a)
}

// log_gamma computes the log-gamma function value
[inline]
pub fn log_gamma(x f64) f64 {
	return C.lgamma(x)
}
