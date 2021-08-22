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
