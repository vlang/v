fn test_signed_integer_literal_assignment_to_f32() {
	mut a := f32(0)
	a = 1
	assert a == f32(1)
	a = -1
	assert a == f32(-1)
	a = +1
	assert a == f32(1)
	a = (-1)
	assert a == f32(-1)
	a = -(1)
	assert a == f32(-1)
	a = -(-1)
	assert a == f32(1)
	a = -0
	assert a == f32(0)
	a = -0x10
	assert a == f32(-16)
	a = -0b10
	assert a == f32(-2)
	a = -0o10
	assert a == f32(-8)
}

fn test_signed_integer_literal_assignment_to_f64() {
	mut a := f64(0)
	a = 1
	assert a == f64(1)
	a = -1
	assert a == f64(-1)
	a = +1
	assert a == f64(1)
	a = (-1)
	assert a == f64(-1)
	a = -(1)
	assert a == f64(-1)
	a = -(-1)
	assert a == f64(1)
	a = -0x10
	assert a == f64(-16)
}
