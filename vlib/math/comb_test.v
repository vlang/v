module math

fn test_comb_perm() {
	assert comb(5, 2) == 10
	assert perm(5, 2) == 20
}

fn test_comb_large_no_intermediate_overflow() {
	assert comb(66, 33) == 7219428434016265740
}
