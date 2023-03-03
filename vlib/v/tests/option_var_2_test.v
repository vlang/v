struct Foo {
	name ?string
}

fn test_option_var() {
	foo := Foo{}
	other := foo.name

	println(typeof(other).name)
	if name := other {
		println('with name: ${name}')
		assert false
	} else {
		println('without name')
		assert true
	}

	val := other or {
		println('without name')
		'default'
	}

	assert val == 'default'
}
