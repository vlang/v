module math

// relu6 returns min(max(0, x), 6).
pub fn relu6(x f64) f64 {
	if x < 0 {
		return 0
	}
	if x > 6 {
		return 6
	}
	return x
}
