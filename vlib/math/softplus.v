module math

// softplus returns log(1 + exp(x)).
pub fn softplus(x f64) f64 {
	if x > 0 {
		return x + math.log1p(math.exp(-x))
	}
	return math.log1p(math.exp(x))
}
