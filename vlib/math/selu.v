module math

// selu returns scaled exponential linear unit.
pub fn selu(x f64) f64 {
	alpha := 1.6732632423543772
	scale := 1.0507009873554805
	if x > 0 {
		return scale * x
	}
	return scale * alpha * (math.exp(x) - 1.0)
}
