import math
import datatypes

struct Foo {
	a int
}

struct Bar<T> {
	a int
}

fn (b Bar<T>) pop() {}

fn test_bar_foo_works_even_when_datatypes_is_imported_that_also_has_pop_methods() {
	mut a := Bar<Foo>{}
	println(a)
	assert true
}

fn test_datatypes_can_be_used_without_interfering_with_local_generic_structs() {
	mut stack := datatypes.Stack<int>{}
	stack.push(1)
	println(stack)
	assert true
}

fn test_generic_type_inference_on_generic_function_from_another_module_still_works() {
	x := -123
	a := math.abs(x)
	assert x == -123
	assert a == 123
}
