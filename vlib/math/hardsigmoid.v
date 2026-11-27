module math

// hardsigmoid returns clamp((x+3)/6, 0, 1).
pub fn hardsigmoid(x f64) f64 {
	mut v := (x + 3.0) / 6.0
	if v < 0 {
		v = 0
	}
	if v > 1 {
		v = 1
	}
	return v
}
