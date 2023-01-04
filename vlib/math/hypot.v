module math

// hypot returns the hypotenuse of the triangle give two sides
pub fn hypot(x f64, y f64) f64 {
	if is_inf(x, 0) || is_inf(y, 0) {
		return inf(1)
	}
	if is_nan(x) || is_nan(y) {
		return nan()
	}
	mut result := 0.0
	if x != 0.0 || y != 0.0 {
		abs_x := abs(x)
		abs_y := abs(y)
		min, max := minmax(abs_x, abs_y)
		rat := min / max
		root_term := sqrt(1.0 + rat * rat)
		if max < max_f64 / root_term {
			result = max * root_term
		} else {
			panic('overflow in hypot_e function')
		}
	}
	return result
}
