const a = 100
const b = 200

@[_allow_multiple_values]
enum Example {
	a = a
	b = b
	c = .a
	d = .b
}

fn test_enum_self_reference() {
	assert int(Example.a) == 100
	assert int(Example.b) == 200
	assert int(Example.c) == 100
	assert int(Example.d) == 200
}

@[_allow_multiple_values]
enum Offset {
	base = 100
	same = .base
	plus = .base + 2
	next
}

fn test_enum_value_expression_refers_to_an_earlier_field() {
	assert int(Offset.same) == 100
	assert int(Offset.plus) == 102
	assert int(Offset.next) == 103
}
