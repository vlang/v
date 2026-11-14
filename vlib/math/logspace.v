module math

// logspace returns `n` numbers spaced evenly on a log scale over [10^start, 10^stop].
pub fn logspace(start f64, stop f64, n int) []f64 {
	if n <= 0 {
		return []f64{}
	}
	if n == 1 {
		return [math.pow(10, start)]
	}
	mut out := []f64{len: n}
	step := (stop - start) / f64(n - 1)
	for i in 0 .. n {
		out[i] = math.pow(10, start + step * f64(i))
	}
	return out
}
