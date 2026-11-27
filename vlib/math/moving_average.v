module math

// moving_average returns simple moving averages with window size.
pub fn moving_average(values []f64, window int) []f64 {
	if window <= 0 || values.len == 0 {
		return []f64{}
	}
	mut out := []f64{}
	for i in 0 .. values.len {
		start := if i + 1 >= window { i + 1 - window } else { 0 }
		mut s := 0.0
		mut c := 0
		for j in start .. i + 1 {
			s += values[j]
			c++
		}
		out << s / f64(c)
	}
	return out
}
