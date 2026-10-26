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

enum InFieldAfterValue {
	int       = 0
	map       = 1
	interface = 2
	in        = 3
}

enum IsFieldAfterValue {
	foo = 2
	is  = 3
}

enum AsFieldAfterValue {
	foo = 2
	as  = 3
}

enum OrFieldAfterValue {
	foo = 2
	or  = 3
}

fn test_keyword_field_after_valued_field() {
	assert int(InFieldAfterValue.int) == 0
	assert int(InFieldAfterValue.map) == 1
	assert int(InFieldAfterValue.interface) == 2
	assert int(InFieldAfterValue.in) == 3
	assert int(IsFieldAfterValue.foo) == 2
	assert int(IsFieldAfterValue.is) == 3
	assert int(AsFieldAfterValue.foo) == 2
	assert int(AsFieldAfterValue.as) == 3
	assert int(OrFieldAfterValue.foo) == 2
	assert int(OrFieldAfterValue.or) == 3
}
