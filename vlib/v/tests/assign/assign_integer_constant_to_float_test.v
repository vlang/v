const integer_constant_for_float_assignment = 64

fn test_assign_integer_constant_to_float() {
	mut value := f32(0)
	value = integer_constant_for_float_assignment
	assert value == f32(64)
	value = -integer_constant_for_float_assignment
	assert value == f32(-64)
}

const integer_constant_sum_for_float_assignment = integer_constant_for_float_assignment + 2

struct IntegerConstantFloatTarget {
mut:
	x f32
	y f64
}

// Arithmetic on integer literals and untyped integer constants is still an
// untyped integer, so it is assigned to a float like the literal it folds to.
fn test_assign_integer_constant_expression_to_float() {
	mut value := f32(0)
	value = 2 * 3
	assert value == f32(6)
	value = (2 + 3) * 4
	assert value == f32(20)
	value = 6 * integer_constant_for_float_assignment
	assert value == f32(384)
	value = integer_constant_for_float_assignment * integer_constant_for_float_assignment
	assert value == f32(4096)
	value = integer_constant_for_float_assignment + 1
	assert value == f32(65)
	value = integer_constant_for_float_assignment / 5
	assert value == f32(12)
	value = integer_constant_for_float_assignment % 5
	assert value == f32(4)
	value = integer_constant_sum_for_float_assignment * 2
	assert value == f32(132)
	value = -(integer_constant_for_float_assignment - 4) * 2
	assert value == f32(-120)
	mut target := IntegerConstantFloatTarget{}
	target.x = 6 * integer_constant_for_float_assignment
	target.y = 6 * integer_constant_for_float_assignment
	assert target.x == f32(384)
	assert target.y == f64(384)
}

const integer_deep_expression_for_float_assignment = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 +
	1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1

fn test_deep_integer_constant_arithmetic_can_be_assigned_to_float() {
	mut value := f32(0)
	value = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 +
		1 + 1 + 1 + 1
	assert value == f32(24)
	value = integer_deep_expression_for_float_assignment * 2
	assert value == f32(48)
}
