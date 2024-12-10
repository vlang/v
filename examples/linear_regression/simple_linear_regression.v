import math

struct LinearResult {
	r2                         f64
	intercept                  f64
	slope                      f64
	dependent_variable_means   f64
	independent_variable_means f64
}

fn linearrelationship(independent_variable []int, dependent_variable []int) LinearResult {
	// Objective :
	// Find what is the linear relationship between two dataset X and Y?
	// x := independent variable
	// y := dependent variable
	mut sum_r2_x := 0
	mut sum_r2_y := 0
	mut sum_xy := 0
	mut sum_x := 0
	mut sum_y := 0
	for i in independent_variable {
		sum_x += i
		sum_r2_x += i * i
	}
	for yi in dependent_variable {
		sum_y += yi
		sum_r2_y += yi * yi
	}
	x_means := sum_x / independent_variable.len
	y_means := sum_y / dependent_variable.len
	for index, x_value in independent_variable {
		sum_xy += x_value * dependent_variable[index]
	}
	// /Slope = (∑y)(∑x²) - (∑x)(∑xy) / n(∑x²) - (∑x)²
	slope_value := f64((sum_y * sum_r2_x) - (sum_x * sum_xy)) / f64((sum_r2_x * independent_variable.len) - (sum_x * sum_x))
	// /Intercept = n(∑xy) - (∑x)(∑y) / n(∑x²) - (∑x)²
	intercept_value := f64((independent_variable.len * sum_xy) - (sum_x * sum_y)) / f64((independent_variable.len * sum_r2_x) - (sum_x * sum_x))
	// Regression equation = Intercept + Slope x
	// R2 = n(∑xy) - (∑x)(∑y) / sqrt([n(∑x²)-(∑x)²][n(∑y²)-(∑y)²]
	r2_value := f64((independent_variable.len * sum_xy) - (sum_x * sum_y)) / math.sqrt(f64((sum_r2_x * independent_variable.len) - (sum_x * sum_x)) * f64((sum_r2_y * dependent_variable.len) - (sum_y * sum_y)))
	return LinearResult{
		r2:                         r2_value
		intercept:                  intercept_value
		slope:                      slope_value
		independent_variable_means: x_means
		dependent_variable_means:   y_means
	}
}

fn main() {
	independent_variable := [4, 5, 6, 7, 10]
	dependent_variable := [3, 8, 20, 30, 12]
	result := linearrelationship(independent_variable, dependent_variable)
	println(result)
}
