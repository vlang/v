module math

// comb returns the number of combinations n choose k.
pub fn comb(n int, k int) i64 {
	if k < 0 || k > n {
		return 0
	}
	if k == 0 || k == n {
		return 1
	}
	mut kk := k
	if kk > n - kk {
		kk = n - kk
	}
	mut res := i64(1)
	for i in 0 .. kk {
		num := i64(n - i)
		den := i64(i + 1)
		// Cancel before multiplying so the running product stays the
		// exact binomial coefficient and never overflows when the
		// final result fits.
		g1 := gcd(res, den)
		res_div := res / g1
		den_div := den / g1
		g2 := gcd(num, den_div)
		num_div := num / g2
		den_final := den_div / g2
		res = res_div / den_final * num_div
	}
	return res
}

// perm returns the number of permutations n P k.
pub fn perm(n int, k int) i64 {
	if k < 0 || k > n {
		return 0
	}
	mut res := i64(1)
	for i in 0 .. k {
		res *= i64(n - i)
	}
	return res
}
