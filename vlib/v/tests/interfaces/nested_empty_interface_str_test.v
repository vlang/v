interface Value {}

interface Other {}

struct Point {
	x int
	y int
}

struct Holder {
	v     Value
	other Other
}

fn make(v Value) Holder {
	return Holder{
		v:     v
		other: Other('s')
	}
}

// An interface field is printed with the value boxed in it. The printer of a nested
// interface lists only the types boxed in that interface, which must cover them.
fn test_interface_fields_print_the_values_boxed_in_them() {
	assert '${make(Value(5))}' == "Holder{\n    v: Value(5)\n    other: Other('s')\n}"
	vals := [Value(Point{3, 4}), Value('str')]
	h := Holder{
		v:     vals[0]
		other: Other(Point{5, 6})
	}
	assert '${h}'.contains('v: Value(Point{')
	assert '${h}'.contains('other: Other(Point{')
	assert '${h}'.contains('x: 5')
}
