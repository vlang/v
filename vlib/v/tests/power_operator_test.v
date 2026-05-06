import math

struct Exponent {
	value int
}

fn (a Exponent) **(b Exponent) Exponent {
	mut result := 1
	for _ in 0 .. b.value {
		result *= a.value
	}
	return Exponent{
		value: result
	}
}

const const_power = 2 ** 5

fn test_power_operator_with_ints() {
	assert const_power == 32
	assert 2 ** 3 == 8
	assert 2 ** 3 ** 2 == 512
	assert -2 ** 2 == -4
	assert (-2) ** 2 == 4
}

fn test_power_assign_with_ints() {
	mut value := 3
	value **= 3
	assert value == 27
}

fn test_power_operator_with_floats() {
	assert math.abs(9.0 ** 0.5 - 3.0) < 1e-9
}

fn test_power_operator_overload() {
	mut value := Exponent{
		value: 2
	}
	assert (value ** Exponent{
		value: 3
	}).value == 8
	value **= Exponent{
		value: 3
	}
	assert value.value == 8
}
