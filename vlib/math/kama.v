module math

// kama returns Kaufman's adaptive moving average (simplified).
pub fn kama(values []f64, period int) []f64 {
	if period <= 0 || values.len == 0 {
		return []f64{}
	}
	mut out := []f64{len: values.len}
	out[0] = values[0]
	for i in 1 .. values.len {
		start := if i >= period { i - period } else { 0 }
		// efficiency ratio
		change := math.abs(values[i] - values[start])
		mut volatility := 0.0
		for j in start + 1 .. i + 1 {
			volatility += math.abs(values[j] - values[j - 1])
		}
		er := if volatility == 0 { 0 } else { change / volatility }
		sc := math.pow(er * (2.0 / 3.0 - 2.0 / 31.0) + 2.0 / 31.0, 2)
		out[i] = out[i - 1] + sc * (values[i] - out[i - 1])
	}
	return out
}
