module main

import imported_interface_auto_eq_18326.vest

struct ObjectImpl {}

fn generic_interface_equal[T](a T, b T) bool {
	return a == b
}

fn concrete_interface_equal(a vest.Object, b vest.Object) bool {
	return a == b
}

fn exercise_imported_interface_equality_helpers() bool {
	a := vest.Object(ObjectImpl{})
	b := vest.Object(ObjectImpl{})
	return generic_interface_equal[vest.Object](a, b) && concrete_interface_equal(a, b)
}
