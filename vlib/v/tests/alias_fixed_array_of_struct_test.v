struct Foo {}

type Bar = [4]Foo

fn test_alias_fixed_array_of_struct() {
	bar := Bar([Foo{}, Foo{}, Foo{}, Foo{}]!)
	println(bar)
	assert '${bar}' == 'Bar([Foo{}, Foo{}, Foo{}, Foo{}])'
}
