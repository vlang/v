module math

// t3 returns Tillson T3 moving average.
pub fn t3(values []f64, period int, vfactor f64) []f64 {
	e1 := ema(values, 2.0 / f64(period + 1))
	e2 := ema(e1, 2.0 / f64(period + 1))
	e3 := ema(e2, 2.0 / f64(period + 1))
	e4 := ema(e3, 2.0 / f64(period + 1))
	e5 := ema(e4, 2.0 / f64(period + 1))
	e6 := ema(e5, 2.0 / f64(period + 1))
	mut out := []f64{len: values.len}
	c1 := -vfactor * vfactor * vfactor
	c2 := 3 * vfactor * vfactor + 3 * vfactor * vfactor * vfactor
	c3 := -6 * vfactor * vfactor - 3 * vfactor - 3 * vfactor * vfactor * vfactor
	c4 := 1 + 3 * vfactor + vfactor * vfactor * vfactor + 3 * vfactor * vfactor
	for i in 0 .. values.len {
		out[i] = c1 * e6[i] + c2 * e5[i] + c3 * e4[i] + c4 * e3[i]
	}
	return out
}
