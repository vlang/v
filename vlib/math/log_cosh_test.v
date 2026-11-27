module math

fn test_log_cosh() {
	assert log_cosh(0) == 0
}

fn test_log_cosh_large_stays_finite() {
	got := log_cosh(1000)
	assert got > 999.306852 && got < 999.306854
}
