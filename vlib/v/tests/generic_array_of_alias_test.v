import datatypes

type Distance = int

struct Foo {
	field datatypes.LinkedList[Distance]
}

fn test_generic_array_of_alias() {
	foo := Foo{}
	println(foo)
	assert '${foo.field}' == '[]'
}
