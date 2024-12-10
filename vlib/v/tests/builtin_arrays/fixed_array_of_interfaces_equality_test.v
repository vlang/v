interface IObject {
	foo()
}

struct Foo {}

fn (f Foo) foo() {
}

struct Array {
mut:
	array [1]IObject
}

fn (a Array) contains(x IObject) bool {
	for element in a.array {
		if element == x {
			return true
		}
	}
	return false
}

fn test_fixed_array_of_interfaces_equality() {
	foo := Foo{}
	mut ary := Array{}
	ary.array[0] = foo
	ret := ary.contains(foo)
	println(ret)
	assert ret
}
