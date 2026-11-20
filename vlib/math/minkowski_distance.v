module math

// minkowski_distance computes Lp distance between two vectors.
pub fn minkowski_distance(a []f64, b []f64, p f64) f64 {
	if a.len != b.len {
		return 0
	}
	mut s := 0.0
	for i in 0 .. a.len {
		s += math.pow(math.abs(a[i] - b[i]), p)
	}
	return math.pow(s, 1.0 / p)
}
