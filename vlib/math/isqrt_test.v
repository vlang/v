module math

fn test_isqrt() {
	assert isqrt(10) == 3
	assert isqrt(16) == 4
	assert isqrt(0) == 0
}

fn test_isqrt_max_i64_no_overflow() {
	assert isqrt(9223372036854775807) == 3037000499
	assert isqrt(9223372036854775806) == 3037000499
}
