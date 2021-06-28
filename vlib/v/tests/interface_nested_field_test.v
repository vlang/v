struct Base {
}

interface Foo {
	parent Foo
	thing(mut b Base, value i64) string
}

struct Bar {
	parent Foo
}

fn (f Bar) thing(mut b Base, value i64) string {
	return 'bar'
}

struct SubBar {
	parent Foo = Bar{}
}

fn (f SubBar) thing(mut b Base, value i64) string {
	return 'subbar'
}

fn test_interface_nested_field() {
	mut foo_group := []Foo{}
	foo_group << Bar{}
	foo_group << SubBar{}

	mut b := Base{}
	mut ret := []string{}
	for foo in foo_group {
		println(foo.thing(mut b, 22))
		ret << foo.thing(mut b, 22)
	}
	assert ret.len == 2
	assert ret[0] == 'bar'
	assert ret[1] == 'subbar'
}
