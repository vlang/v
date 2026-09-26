fn test_multiple_assign() {
	a, b, c := 1, 2, 3
	assert a == 1
	assert b == 2
	assert c == 3
}

fn test_multiple_assign_swap_simple() {
	mut a := 11
	mut b := 22
	a, b = b, a
	assert a == 22
	assert b == 11
}

fn test_multiple_assign_swap_complex() {
	mut a := 11
	mut b := 22
	mut c := 33
	mut d := 44
	a, b, c, d = b, a, d, c
	assert a == 22
	assert b == 11
	assert c == 44
	assert d == 33
}

fn test_multiple_assign_infix_expr() {
	mut a := 11
	mut b := 22
	mut c := 33
	a, b, c = b + 1, a * 2, c - a
	assert a == 23
	assert b == 22
	assert c == 22
}

fn test_multiple_assign_prefix_expr() {
	mut a := 11
	mut b := 22
	mut c := 33
	a, b, c = -b, -c, -a
	assert a == -22
	assert b == -33
	assert c == -11
}

fn test_multiple_assign_postfix_expr() {
	mut a := 11
	mut b := 22
	mut c := 33
	old_a, old_b, old_c := a, b, c
	a--
	b++
	c++
	a, b, c = old_b, old_c, old_a
	assert a == 22
	assert b == 33
	assert c == 11
}

fn test_multiple_assign_complex_expr() {
	mut a := 11
	mut b := 22
	mut c := 33
	old_a := a
	a++
	a, b, c = -b + 1, -c * 2, old_a
	assert a == -21
	assert b == -66
	assert c == 11
}

struct FloatFields {
mut:
	radius   f32
	rotation f32
}

fn test_multiple_assign_signed_integer_literals_to_float_fields() {
	mut fields := FloatFields{}
	fields.rotation = -90
	fields.radius, fields.rotation = 15, -90
	assert fields.radius == f32(15)
	assert fields.rotation == f32(-90)
}
