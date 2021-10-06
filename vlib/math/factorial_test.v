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

fn test_int_factorial() {
	assert int_factorial(20) == 2432902008176640000
	assert int_factorial(1) == 1
	assert int_factorial(2) == 2
	assert int_factorial(0) == 1
	assert int_factorial(-2) == 1
	assert int_factorial(1000) == -1
}
