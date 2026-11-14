module math

// mish returns x * tanh(softplus(x)).
pub fn mish(x f64) f64 {
	return x * math.tanh(math.log(1.0 + math.exp(x)))
}
