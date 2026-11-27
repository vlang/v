module math

fn test_is_prime() {
	assert is_prime(2)
	assert is_prime(3)
	assert is_prime(17)
	assert !is_prime(1)
	assert !is_prime(4)
	assert !is_prime(100)
}
