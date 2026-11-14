module math

// thresholded_relu returns x if x > theta else 0.
pub fn thresholded_relu(x f64, theta f64) f64 {
	if x > theta {
		return x
	}
	return 0
}
