interface IObject {
	foo()
}

struct Foo {}

fn (f Foo) foo() {
}

struct Array {
mut:
	array []IObject
}

fn (a Array) contains(x IObject) bool {
	for element in a.array {
		if element == x {
			return true
		}
	}
	return false
}

fn test_array_of_interfaces_equality() {
	foo := Foo{}
	mut ary := Array{}
	ary.array << foo
	ret := ary.contains(foo)
	println(ret)
	assert ret
}
