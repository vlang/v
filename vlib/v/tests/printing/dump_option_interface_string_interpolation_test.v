interface Nested {
	str() string
}

struct Leaf {
	name string
}

fn (l Leaf) str() string {
	return l.name
}

fn denest_none() ?Nested {
	return none
}

fn denest_value() ?Nested {
	return Leaf{'leaf'}
}

fn test_dump_string_interpolation_of_option_interface_none() {
	n := denest_none()
	s := dump('${n}')
	assert s == 'Option(none)'
}

fn test_dump_string_interpolation_of_option_interface_value() {
	n := denest_value()
	s := dump('${n}')
	assert s == 'Option(Nested(leaf))'
}
