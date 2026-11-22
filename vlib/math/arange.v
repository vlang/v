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
			next := v + step
			// With large v and small step, v + step may round back to v.
			// Stop instead of looping forever on the same value.
			if next == v {
				return out
			}
			v = next
		}
	} else {
		mut v := start
		for v > stop {
			out << v
			next := v + step
			if next == v {
				return out
			}
			v = next
		}
	}
	return out
}
