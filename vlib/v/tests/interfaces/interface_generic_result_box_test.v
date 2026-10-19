interface GenericResultShape {
	f() int
}

struct GenericResultSquare {
	n int = 7
}

fn (s GenericResultSquare) f() int {
	return s.n
}

fn generic_result_identity[T](value T) T {
	return value
}

fn generic_result_unwrap[T](value ?T) T {
	return value or { panic('missing value') }
}

struct GenericResultHolder {
mut:
	shape ?GenericResultShape
}

fn generic_result_argument(shape GenericResultShape) int {
	return shape.f()
}

fn test_interface_cast_of_inferred_generic_result() {
	shape := GenericResultShape(generic_result_identity(GenericResultSquare{}))
	assert shape.f() == 7
	assert shape is GenericResultSquare
	assert generic_result_argument(generic_result_identity(GenericResultSquare{ n: 11 })) == 11
}

fn test_optional_interface_field_of_inferred_generic_result() {
	mut holder := GenericResultHolder{}
	holder.shape = generic_result_identity(GenericResultSquare{ n: 13 })
	shape := holder.shape or { panic('missing shape') }
	assert shape.f() == 13
	assert shape is GenericResultSquare
}

fn test_interface_cast_of_generic_unwrapped_pointer() {
	value := ?&GenericResultSquare(&GenericResultSquare{ n: 17 })
	shape := GenericResultShape(generic_result_unwrap(value))
	assert shape.f() == 17
	mut holder := GenericResultHolder{}
	holder.shape = generic_result_unwrap(value)
	stored := holder.shape or { panic('missing shape') }
	assert stored.f() == 17
}
