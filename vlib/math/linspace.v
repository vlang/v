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
	step := (stop - start) / f64(n - 1)
	for i in 0 .. n {
		out[i] = start + step * f64(i)
	}
	return out
}
