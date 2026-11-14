module math

fn test_mobius() {
	assert mobius(1) == 1
	assert mobius(2) == -1
	assert mobius(4) == 0
	assert mobius(6) == 1
}
