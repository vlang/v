module math

// geomspace returns `n` numbers spaced evenly on a log scale (geometric progression).
pub fn geomspace(start f64, stop f64, n int) []f64 {
	if n <= 0 {
		return []f64{}
	}
	if n == 1 {
		return [start]
	}
	mut out := []f64{len: n}
	ratio := math.pow(stop / start, 1.0 / f64(n - 1))
	mut v := start
	for i in 0 .. n {
		out[i] = v
		v *= ratio
	}
	return out
}
