// Regression test for #19618.
// Generic interface inference should bind nested generic struct arguments.

interface MyInterface[T] {
	is_good(a T) bool
}

struct MyStruct[T] {}

struct Opt[T] {
	x ?T
}

fn (_ MyStruct[T]) is_good(a Opt[T]) bool {
	if _ := a.x {
		return true
	}
	return false
}

fn check[T](a MyInterface[T], b T) bool {
	return a.is_good(b)
}

fn test_generic_interface_infers_nested_generic_struct_argument() {
	a := MyStruct[int]{}
	b := Opt[int]{
		x: 6
	}
	assert check(a, b)
}
