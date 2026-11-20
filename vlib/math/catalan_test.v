module math

fn test_catalan() {
	assert catalan(0) == 1
	assert catalan(3) == 5
}

fn test_catalan_large_no_intermediate_overflow() {
	assert catalan(35) == 3116285494907301262
}
