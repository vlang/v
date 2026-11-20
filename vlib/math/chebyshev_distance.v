module math

// chebyshev_distance computes L-inf distance between two vectors.
pub fn chebyshev_distance(a []f64, b []f64) f64 {
	if a.len != b.len {
		return 0
	}
	mut m := 0.0
	for i in 0 .. a.len {
		d := math.abs(a[i] - b[i])
		if d > m {
			m = d
		}
	}
	return m
}
