struct Base {
}

interface Foo {
	parent Foo2
	thing(mut b Base, value i64) string
}

interface Foo2 {
	thing(mut b Base, value i64) string
}

struct Bar {
	parent Foo2
}

struct Bar2 {}

fn (f Bar) thing(mut b Base, value i64) string {
	return 'bar'
}

fn (f Bar2) thing(mut b Base, value i64) string {
	return 'bar2'
}

struct SubBar {
	parent Foo2 = Bar2{}
}

fn (f SubBar) thing(mut b Base, value i64) string {
	return 'subbar'
}

fn test_interface_nested_field() {
	mut foo_group := []Foo2{}
	foo_group << Bar2{}
	foo_group << SubBar{}

	mut b := Base{}
	mut ret := []string{}
	for foo in foo_group {
		println(foo.thing(mut b, 22))
		ret << foo.thing(mut b, 22)
	}
	assert ret.len == 2
	assert ret[0] == 'bar2'
	assert ret[1] == 'subbar'
}

interface Root {
	v View
}

struct MyRoot {
	v View
}

interface View {
	render() int
}

struct MyView {}

fn (m MyView) render() int {
	return 24
}

fn receive_root(r Root) int {
	return r.v.render()
}

fn test_nested_interface_fields() {
	assert receive_root(MyRoot{MyView{}}) == 24
}
