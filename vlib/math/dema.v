module math

// dema returns double exponential moving average.
pub fn dema(values []f64, alpha f64) []f64 {
	ema1 := ema(values, alpha)
	ema2 := ema(ema1, alpha)
	mut out := []f64{len: values.len}
	for i in 0 .. values.len {
		out[i] = 2 * ema1[i] - ema2[i]
	}
	return out
}
