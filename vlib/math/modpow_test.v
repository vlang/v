module math

fn test_modpow() {
	assert modpow(2, 10, 1000) == 24
}

fn test_modpow_no_i64_overflow() {
	assert modpow(4000000000, 2, 4000000007) == 49
}
