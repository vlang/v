module math

// euclidean_distance computes L2 distance between two vectors.
pub fn euclidean_distance(a []f64, b []f64) f64 {
	if a.len != b.len {
		return 0
	}
	mut s := 0.0
	for i in 0 .. a.len {
		d := a[i] - b[i]
		s += d * d
	}
	return math.sqrt(s)
}
