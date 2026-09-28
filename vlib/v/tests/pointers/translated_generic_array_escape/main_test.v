@[has_globals]
module main

import decay

type Values = [3]int

__global saved_pointer = forward(make_values())

fn make_values() Values {
	return Values([3, 5, 7]!)
}

fn forward(values Values) &int {
	return decay.pick(values)
}

fn forward_twice(values Values) &int {
	return forward(values)
}

fn escape_temporary() &int {
	return forward_twice(make_values())
}

fn escape_local() &int {
	mut values := make_values()
	pointer := forward_twice(values)
	values[1] = 13
	return pointer
}

fn invoke_picker(picker fn (Values) &int, values Values) &int {
	return picker(values)
}

fn escape_indirect_temporary() &int {
	picker := decay.pick[Values]
	return invoke_picker(picker, make_values())
}

fn escape_indirect_local() &int {
	picker := decay.pick[Values]
	mut values := make_values()
	pointer := invoke_picker(picker, values)
	values[1] = 17
	return pointer
}

fn invoke_generic_picker[T](picker T, values Values) &int {
	return picker(values)
}

fn escape_generic_callback() &int {
	picker := decay.pick[Values]
	return invoke_generic_picker(picker, make_values())
}

fn invoke_generic_arguments[T, U](picker T, values U) &int {
	return picker(values)
}

fn escape_generic_arguments() &int {
	mut values := make_values()
	picker := decay.pick[Values]
	pointer := invoke_generic_arguments(picker, values)
	values[1] = 23
	return pointer
}

fn escape_generic_argument_temporary() &int {
	picker := decay.pick[Values]
	return invoke_generic_arguments(picker, make_values())
}

fn escape_direct_generic_local() &int {
	mut values := make_values()
	pointer := decay.pick(values)
	values[1] = 29
	return pointer
}

fn retain(values Values) {
	decay.retain(values)
}

fn retain_local() {
	mut values := make_values()
	retain(values)
	values[1] = 19
}

fn read_pointer(pointer &int) int {
	return unsafe { *pointer }
}

fn test_generic_translated_signatures_preserve_array_storage() {
	assert read_pointer(saved_pointer) == 5
	assert read_pointer(escape_temporary()) == 5
	assert read_pointer(escape_local()) == 13
	assert read_pointer(escape_indirect_temporary()) == 5
	assert read_pointer(escape_indirect_local()) == 17
	assert read_pointer(escape_generic_callback()) == 5
	assert read_pointer(escape_generic_arguments()) == 23
	assert read_pointer(escape_generic_argument_temporary()) == 5
	assert read_pointer(escape_direct_generic_local()) == 29
	retain_local()
	assert decay.read_retained() == 19
	retain(make_values())
	assert decay.read_retained() == 5
}
