module math

// ema returns the exponential moving average of values using the smoothing
// factor alpha (0..1). The first output equals the first input. An empty
// slice returns an empty slice.
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
