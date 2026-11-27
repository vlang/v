module math

// trima returns triangular moving average.
pub fn trima(values []f64, period int) []f64 {
	if period <= 0 || values.len == 0 {
		return []f64{}
	}
	mut out := []f64{}
	for i in 0 .. values.len {
		start := if i + 1 >= period { i + 1 - period } else { 0 }
		mut s := 0.0
		mut c := 0
		for j in start .. i + 1 {
			// triangular weight
			w := if j <= (start + i) / 2 { j - start + 1 } else { i - j + 1 }
			s += values[j] * f64(w)
			c += w
		}
		out << s / f64(c)
	}
	return out
}
