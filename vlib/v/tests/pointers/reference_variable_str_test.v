@[heap]
struct Foo {
	bar string = 'bar'
	baz string = 'baz'
}

fn (foo Foo) str() string {
	return 'bar: ${foo.bar}, baz: ${foo.baz}'
}

fn test_reference_variable_str() {
	mut many_foos := []&Foo{len: 3, init: &Foo{}}
	println(many_foos.map(it.str()).join('\n'))
	println(many_foos)
	assert many_foos.map(it.str()) == ['&bar: bar, baz: baz', '&bar: bar, baz: baz',
		'&bar: bar, baz: baz']
}
