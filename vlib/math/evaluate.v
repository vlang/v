module math

// Provides functions that don't have a numerical solution and must
// be solved computationally (e.g. evaluation of a polynomial)

pub fn polynomial(z f64, coeff []f64) f64 {
	n := coeff.len
	if n == 0 {
		return 0.0
	}

	mut sum := coeff[n - 1]
	for i := n - 1; i >= 0; i-- {
		sum *= z
		sum += coeff[i]
	}
	return sum
}
