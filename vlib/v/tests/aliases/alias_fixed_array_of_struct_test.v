struct Foo {}

type Bar = [4]Foo

struct Baz {
	data int
}

type BazFixed = [2]Baz
type NestedBazFixed = [2]BazFixed

struct Empty {}

type EmptyFixed = [2]Empty

fn test_alias_fixed_array_of_struct() {
	bar := Bar([Foo{}, Foo{}, Foo{}, Foo{}]!)
	println(bar)
	assert '${bar}' == 'Bar([Foo{}, Foo{}, Foo{}, Foo{}])'
}

fn test_nested_fixed_array_alias_of_named_struct() {
	nested := [2]NestedBazFixed{}
	assert nested.len == 2
	assert nested[0].len == 2
	assert nested[0][0][0].data == 0
}

fn test_fixed_array_alias_of_empty_struct() {
	nested := [2]EmptyFixed{}
	assert nested.len == 2
	assert nested[0].len == 2
}
