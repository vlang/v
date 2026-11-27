module math

// zlema returns zero-lag exponential moving average.
pub fn zlema(values []f64, period int) []f64 {
	if period <= 0 || values.len == 0 {
		return []f64{}
	}
	lag := (period - 1) / 2
	mut data := []f64{len: values.len}
	for i in 0 .. values.len {
		j := if i >= lag { i - lag } else { 0 }
		data[i] = values[i] + (values[i] - values[j])
	}
	return ema(data, 2.0 / f64(period + 1))
}
