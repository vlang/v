module math

// multinomial returns n! / (k1! * k2! * ...).
pub fn multinomial(n int, ks []int) i64 {
	mut total := 0
	for k in ks {
		if k < 0 {
			return 0
		}
		total += k
	}
	if total != n {
		return 0
	}
	// n!/(k1! k2! ...) = C(n,k1)*C(n-k1,k2)*... . Each partial product
	// equals the running multinomial and is bounded by the final value,
	// so no intermediate factorial overflows when the result fits.
	mut acc := i64(1)
	mut rem := n
	for k in ks {
		acc *= comb(rem, k)
		rem -= k
	}
	return acc
}
