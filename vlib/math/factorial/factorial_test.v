import math
import math.factorial as fact

fn test_factorial() {
	assert fact.factorial(12) == 479001600
	assert fact.factorial(5) == 120
	assert fact.factorial(0) == 1
}

fn test_log_factorial() {
	assert fact.log_factorial(12) == math.log(479001600)
	assert fact.log_factorial(5) == math.log(120)
	assert fact.log_factorial(0) == math.log(1)
}
