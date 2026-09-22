const integer_constant_for_float_assignment = 64

fn test_assign_integer_constant_to_float() {
	mut value := f32(0)
	value = integer_constant_for_float_assignment
	assert value == f32(64)
	value = -integer_constant_for_float_assignment
	assert value == f32(-64)
}
