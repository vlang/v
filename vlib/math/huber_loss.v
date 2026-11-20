module math

// huber_loss with delta.
pub fn huber_loss(y f64, pred f64, delta f64) f64 {
	d := y - pred
	ad := math.abs(d)
	if ad <= delta {
		return 0.5 * d * d
	}
	return delta * (ad - 0.5 * delta)
}
