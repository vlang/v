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
	out[0] = start
	out[n - 1] = stop
	if n == 2 {
		return out
	}
	if start > 0 && stop > 0 {
		// Log-domain interpolation: the ratio stop/start can overflow
		// (1e-300 to 1e300 spans 1e600) while every sample is finite.
		ls := math.log(start)
		le := math.log(stop)
		for i in 1 .. n - 1 {
			t := f64(i) / f64(n - 1)
			out[i] = math.exp(ls * (1.0 - t) + le * t)
		}
		return out
	}
	if start < 0 && stop < 0 {
		ls := math.log(-start)
		le := math.log(-stop)
		for i in 1 .. n - 1 {
			t := f64(i) / f64(n - 1)
			out[i] = -math.exp(ls * (1.0 - t) + le * t)
		}
		return out
	}
	ratio := math.pow(stop / start, 1.0 / f64(n - 1))
	mut v := start
	for i in 1 .. n - 1 {
		v *= ratio
		out[i] = v
	}
	return out
}
