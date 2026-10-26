enum Value {
	int       = 0
	map       = 1
	interface = 2
	in        = 3
	is        = 4
	as        = 5
	or        = 6
}

fn test_enum_keyword_field_after_valued_field() {
	assert int(Value.interface) == 2
	assert int(Value.in) == 3
	assert int(Value.is) == 4
	assert int(Value.as) == 5
	assert int(Value.or) == 6
	assert Value.in.str() == 'in'
}
