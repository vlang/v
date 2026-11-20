module math

// celu returns max(0,x) + min(0, alpha*(exp(x/alpha)-1)).
pub fn celu(x f64, alpha f64) f64 {
	if x >= 0 {
		return x
	}
	return alpha * (math.exp(x / alpha) - 1.0)
}
