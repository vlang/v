module math

fn test_multinomial() {
	assert multinomial(3, [1, 1, 1]) == 6
}

fn test_multinomial_no_factorial_overflow() {
	assert multinomial(21, [20, 1]) == 21
}
