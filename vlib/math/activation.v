module math

// sigmoid returns 1 / (1 + exp(-x)), a smooth step from 0 to 1.
pub fn sigmoid(x f64) f64 {
	return 1.0 / (1.0 + exp(-x))
}

// relu returns max(0, x).
pub fn relu(x f64) f64 {
	if x > 0 {
		return x
	}
	return 0
}

// softmax_slice returns the softmax probabilities of x, computed with a
// shifted maximum for numerical stability. An empty slice returns empty.
pub fn softmax_slice(x []f64) []f64 {
	if x.len == 0 {
		return []f64{}
	}
	mut m := x[0]
	for v in x {
		if v > m {
			m = v
		}
	}
	mut exps := []f64{len: x.len}
	mut sum := 0.0
	for i, v in x {
		exps[i] = exp(v - m)
		sum += exps[i]
	}
	mut out := []f64{len: x.len}
	for i in 0 .. x.len {
		out[i] = exps[i] / sum
	}
	return out
}
