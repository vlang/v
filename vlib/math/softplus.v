module math

// softplus returns log(1 + exp(x)).
pub fn softplus(x f64) f64 {
	return math.log(1.0 + math.exp(x))
}
