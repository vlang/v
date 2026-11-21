module math

// gradient returns the central differences of values with unit spacing.
// Endpoints use forward and backward differences. Fewer than two values
// returns an empty slice.
pub fn gradient(values []f64) []f64 {
	if values.len < 2 {
		return []f64{}
	}
	mut out := []f64{len: values.len}
	out[0] = values[1] - values[0]
	out[values.len - 1] = values[values.len - 1] - values[values.len - 2]
	for i in 1 .. values.len - 1 {
		out[i] = (values[i + 1] - values[i - 1]) / 2.0
	}
	return out
}
