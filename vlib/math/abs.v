module math

[deprecated: 'use math.abs() instead']
pub fn fabs(x f64) f64 {
	if x > 0.0 {
		return x
	}
	return -x
}
