struct Abc {
mut:
	val string
}
struct Xyz {
	name string
}
type Alphabet = Abc | Xyz

fn test_if_smartcast() {
	x := Alphabet(Abc{'test'})
	if x is Abc {
		assert x.val == 'test'
	}
}

fn test_mutable() {
	mut x := Alphabet(Abc{'test'})
	if x is Abc {
		y := Abc{}
		x = &y
		assert x == &y
	}
}

fn test_nested_if_smartcast() {
	x := Alphabet(Abc{'test'})
	y := Alphabet(Xyz{'foo'})
	if x is Abc {
		if y is Xyz {
			assert y.name == 'foo'
		}
	}
}

fn test_as_cast() {
	x := Alphabet(Abc{'test'})
	if x is Abc as test {
		assert test.val == 'test'
	}
}

struct Test {
	abc Alphabet
}

fn test_mutable_with_struct() {
	mut x := Test{Abc{'test'}}
	if x.abc is Abc as test {
		test.val = 'test'
		assert test.val == 'test'
	}
}

fn test_as_cast_with_struct() {
	x := Test{Abc{'test'}}
	if x.abc is Abc as test {
		assert test.val == 'test'
	}
}
