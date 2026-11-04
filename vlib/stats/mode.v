module stats

// mode returns the most frequent value in `values` (first on ties).
pub fn mode(values []f64) f64 {
	if values.len == 0 {
		return 0
	}
	mut counts := map[f64]int{}
	for v in values {
		counts[v]++
	}
	mut best := values[0]
	mut bestc := 0
	for k, c in counts {
		if c > bestc {
			bestc = c
			best = k
		}
	}
	return best
}
