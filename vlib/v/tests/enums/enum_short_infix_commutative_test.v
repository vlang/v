enum EnumCommutative {
	a
	b
}

fn short_eq_left(x EnumCommutative) bool {
	return .a == x
}

fn short_eq_right(x EnumCommutative) bool {
	return x == .a
}

fn short_ne_left(x EnumCommutative) bool {
	return .a != x
}

fn short_ne_right(x EnumCommutative) bool {
	return x != .a
}

fn test_short_enum_infix_equality_is_commutative() {
	assert short_eq_left(.a)
	assert short_eq_right(.a)
	assert !short_eq_left(.b)
	assert !short_eq_right(.b)
}

fn test_short_enum_infix_inequality_is_commutative() {
	assert !short_ne_left(.a)
	assert !short_ne_right(.a)
	assert short_ne_left(.b)
	assert short_ne_right(.b)
}
