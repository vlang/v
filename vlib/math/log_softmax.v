module math

// log_softmax returns log(softmax(x)) for each element.
pub fn log_softmax(x []f64) []f64 {
	mut m := x[0]
	for v in x {
		if v > m {
			m = v
		}
	}
	mut s := 0.0
	for v in x {
		s += math.exp(v - m)
	}
	mut out := []f64{len: x.len}
	for i, v in x {
		out[i] = v - m - math.log(s)
	}
	return out
}
