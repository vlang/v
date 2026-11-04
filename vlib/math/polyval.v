module math

// polyval evaluates polynomial with coefficients `p` (highest degree first) at x.
pub fn polyval(p []f64, x f64) f64 {
	mut res := 0.0
	for c in p {
		res = res * x + c
	}
	return res
}
