module math

// vidya returns variable index dynamic average (simplified).
pub fn vidya(values []f64, period int) []f64 {
	if period <= 0 || values.len == 0 {
		return []f64{}
	}
	mut out := []f64{len: values.len}
	out[0] = values[0]
	for i in 1 .. values.len {
		start := if i >= period { i - period } else { 0 }
		change := math.abs(values[i] - values[start])
		mut volatility := 0.0
		for j in start + 1 .. i + 1 {
			volatility += math.abs(values[j] - values[j - 1])
		}
		cmo := if volatility == 0 { 0 } else { change / volatility }
		sc := math.pow(cmo * (2.0 / 3.0 - 2.0 / 31.0) + 2.0 / 31.0, 2)
		out[i] = out[i - 1] + sc * (values[i] - out[i - 1])
	}
	return out
}
