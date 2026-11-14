module math

// next_prime returns the smallest prime >= n.
pub fn next_prime(n i64) i64 {
	mut x := n
	if x < 2 {
		x = 2
	}
	for !is_prime(x) {
		x++
	}
	return x
}
