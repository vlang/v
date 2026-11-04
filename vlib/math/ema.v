module math

// ema returns exponential moving averages with smoothing factor alpha (0..1).
pub fn ema(values []f64, alpha f64) []f64 {
	if values.len == 0 {
		return []f64{}
	}
	mut out := []f64{len: values.len}
	out[0] = values[0]
	for i in 1 .. values.len {
		out[i] = alpha * values[i] + (1 - alpha) * out[i - 1]
	}
	return out
}
