module math

fn test_prime_factors() {
	assert prime_factors(12) == [2, 2, 3]
	assert prime_factors(1) == []i64{}
}
