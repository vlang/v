module math

// softsign returns x / (1 + |x|).
pub fn softsign(x f64) f64 {
	return x / (1.0 + math.abs(x))
}
