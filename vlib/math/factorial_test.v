module math

fn test_factorial() {
	assert factorial(12) == 479001600
	assert factorial(5) == 120
	assert factorial(0) == 1
}

fn test_log_factorial() {
	assert log_factorial(12) == log(479001600)
	assert log_factorial(5) == log(120)
	assert log_factorial(0) == log(1)
}

fn test_factoriali() {
	assert factoriali(20) == 2432902008176640000
	assert factoriali(1) == 1
	assert factoriali(2) == 2
	assert factoriali(0) == 1
	assert factoriali(-2) == 1
	assert factoriali(1000) == -1
}

fn test_factorial_overflow() {
	for n in [171.0, 171.5, 172.0, 1e10, 1e300, max_f64, inf(1)] {
		assert is_inf(factorial(n), 1), 'factorial(${n}) should overflow to +inf'
	}
}

fn test_factorial_overflow_boundary() {
	assert factorial(170) == factorials_table[170]
	assert !is_inf(factorial(170), 0)
	assert factorial(170.5) == gamma(171.5)
	assert !is_inf(factorial(170.5), 0)
	assert is_inf(factorial(170.75), 1)
}

fn test_factorial_gamma_extension() {
	assert factorial(-0.5) == gamma(0.5)
	assert is_inf(factorial(-1), 1)
	assert is_nan(factorial(-2))
	assert is_nan(factorial(inf(-1)))
	assert is_nan(factorial(nan()))
}
