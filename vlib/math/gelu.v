module math

// gelu approximates the Gaussian Error Linear Unit.
pub fn gelu(x f64) f64 {
	return 0.5 * x * (1.0 + math.tanh(math.sqrt(2.0 / math.pi) * (x + 0.044715 * x * x * x)))
}
