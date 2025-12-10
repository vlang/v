import math

fn test_round_to_even() {
	assert math.round_to_even(0.123) == 0.0
	assert math.round_to_even(123.12345) == 123.00
}
