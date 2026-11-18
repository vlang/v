module math

// linspace returns `n` evenly spaced samples over [start, stop].
pub fn linspace(start f64, stop f64, n int) []f64 {
	if n <= 0 {
		return []f64{}
	}
	if n == 1 {
		return [start]
	}
	mut out := []f64{len: n}
	out[0] = start
	out[n - 1] = stop
	// Convex combination keeps every sample within [start, stop] so a
	// finite span never overflows through (stop - start).
	for i in 1 .. n - 1 {
		t := f64(i) / f64(n - 1)
		out[i] = start * (1.0 - t) + stop * t
	}
	return out
}
