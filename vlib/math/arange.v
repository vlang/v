module math

// arange returns values in [start, stop) spaced by step. A zero step returns
// an empty slice; a negative step walks downwards.
pub fn arange(start f64, stop f64, step f64) []f64 {
	if step == 0 {
		return []f64{}
	}
	mut out := []f64{}
	if step > 0 {
		mut v := start
		for v < stop {
			out << v
			v += step
		}
	} else {
		mut v := start
		for v > stop {
			out << v
			v += step
		}
	}
	return out
}
