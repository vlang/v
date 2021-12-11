struct Abc {
	a byte
	b byte
	c int
}

fn decode<T>() T {
	mut x := T{}
	$for field in T.fields {
		$if field.typ is byte {
			x.$(field.name) = 1
		} $else {
			x.$(field.name) = 3
		}
		if x.$(field.name) == 1 {
			x.$(field.name) = 5
		}
	}
	return x
}

fn test_decode() {
	abc := decode<Abc>()
	assert abc.a == 5
	assert abc.b == 5
	assert abc.c == 3
}
