module test_math

import math

[export: 'square']
fn calculate_square(i int) int {
	return i * i
}

[export: 'sqrt_of_sum_of_squares']
fn calculate_sqrt_of_sum_of_squares(x f64, y f64) f64 {
	return math.sqrt(x * x + y * y)
}
