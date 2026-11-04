module math

// tema returns triple exponential moving average.
pub fn tema(values []f64, alpha f64) []f64 {
	ema1 := ema(values, alpha)
	ema2 := ema(ema1, alpha)
	ema3 := ema(ema2, alpha)
	mut out := []f64{len: values.len}
	for i in 0 .. values.len {
		out[i] = 3 * ema1[i] - 3 * ema2[i] + ema3[i]
	}
	return out
}
