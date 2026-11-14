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
		res = res * i64(n - i) / i64(i + 1)
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
