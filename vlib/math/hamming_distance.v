module math

// hamming_distance counts differing positions.
pub fn hamming_distance(a []f64, b []f64) int {
	if a.len != b.len {
		return 0
	}
	mut d := 0
	for i in 0 .. a.len {
		if a[i] != b[i] {
			d++
		}
	}
	return d
}
