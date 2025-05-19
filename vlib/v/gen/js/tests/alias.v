type Type0 = string
type Type1 = int | string
type Type2 = string | int

struct Foo {
	field_0 Type0
	field_1 Type1
	field_2 Type2
}

fn main() {
	foo := Foo{}

	// checks alias types
	assert foo.field_0 == ''

	// checks sum types
	if foo.field_1 is int {
		assert foo.field_1 == 0
	} else {
		assert false
	}

	// checks sum types
	if foo.field_2 is string {
		assert foo.field_2 == ''
	} else {
		assert false
	}
}
