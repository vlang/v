const v3_untyped_float_scale = 50.0

fn take_v3_f32(value f32) f32 {
	return value
}

fn test_untyped_float_const_adopts_other_infix_operand_type() {
	value := f32(2) * v3_untyped_float_scale
	assert take_v3_f32(value) == 100
	assert take_v3_f32(f32(3) * v3_untyped_float_scale) == 150
}
