module math

// interp linearly interpolates y at x given (xp, yp).
pub fn interp(x f64, xp []f64, yp []f64) f64 {
	if xp.len == 0 || yp.len == 0 || xp.len != yp.len {
		return 0
	}
	if x <= xp[0] {
		return yp[0]
	}
	if x >= xp[xp.len - 1] {
		return yp[yp.len - 1]
	}
	for i in 0 .. xp.len - 1 {
		if x >= xp[i] && x <= xp[i + 1] {
			dx := xp[i + 1] - xp[i]
			if dx == 0 {
				return yp[i]
			}
			t := (x - xp[i]) / dx
			return yp[i] + t * (yp[i + 1] - yp[i])
		}
	}
	return yp[yp.len - 1]
}
