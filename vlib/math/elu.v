module math

// elu returns x if x>0 else alpha*(exp(x)-1).
pub fn elu(x f64, alpha f64) f64 {
	if x > 0 {
		return x
	}
	return alpha * (math.exp(x) - 1.0)
}
