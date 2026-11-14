module math

// leaky_relu returns x if x>0 else alpha*x.
pub fn leaky_relu(x f64, alpha f64) f64 {
	if x > 0 {
		return x
	}
	return alpha * x
}
