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

	mut counter := 0
	val := other or {
		counter++
		'default'
	}

	assert val == 'default'
	assert counter == 1
}
