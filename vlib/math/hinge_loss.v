module math

// hinge_loss returns max(0, 1 - y*score).
pub fn hinge_loss(y f64, score f64) f64 {
	v := 1.0 - y * score
	if v > 0 {
		return v
	}
	return 0
}
