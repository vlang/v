module math

// hardswish returns x * relu6(x+3) / 6.
pub fn hardswish(x f64) f64 {
	mut r := x + 3.0
	if r < 0 {
		r = 0
	}
	if r > 6 {
		r = 6
	}
	return x * r / 6.0
}
