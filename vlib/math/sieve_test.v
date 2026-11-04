module math

fn test_sieve() {
	p := sieve(10)
	assert p == [2, 3, 5, 7]
}
