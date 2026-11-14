module math

// manhattan_distance computes L1 distance between two vectors.
pub fn manhattan_distance(a []f64, b []f64) f64 {
	if a.len != b.len {
		return 0
	}
	mut s := 0.0
	for i in 0 .. a.len {
		s += math.abs(a[i] - b[i])
	}
	return s
}
