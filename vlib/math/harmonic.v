module math

// harmonic returns the n-th harmonic number H_n.
pub fn harmonic(n int) f64 {
	if n <= 0 {
		return 0
	}
	mut s := 0.0
	for i in 1 .. n + 1 {
		s += 1.0 / f64(i)
	}
	return s
}
