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

struct Abc2 {
	an_int   int
	a_byte   byte
	a_string string
}

fn decode2<T>() T {
	mut x := T{}
	$for field in T.fields {
		$if field.typ is byte {
			x.$(field.name) = byte(-1)
		} $else $if field.typ is int {
			x.$(field.name) = int(-1)
		} $else $if field.typ is string {
			x.$(field.name) = 'hi'
		}
	}
	return x
}

fn test_decode2() {
	abc := decode2<Abc2>()
	assert abc.an_int == -1
	assert abc.a_byte == 0xff
	assert abc.a_string == 'hi'
}
