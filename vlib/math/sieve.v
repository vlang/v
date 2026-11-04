module math

// sieve returns all primes <= n using the Sieve of Eratosthenes.
pub fn sieve(n int) []int {
	if n < 2 {
		return []int{}
	}
	mut is_comp := []bool{len: n + 1}
	mut primes := []int{}
	for p in 2 .. n + 1 {
		if !is_comp[p] {
			primes << p
			for multiple := p * p; multiple <= n; multiple += p {
				is_comp[multiple] = true
			}
		}
	}
	return primes
}
