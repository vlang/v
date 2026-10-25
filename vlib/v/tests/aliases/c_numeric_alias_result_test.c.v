#include <stddef.h>
#define v_result_handle size_t
#define v_result_make_handle(value) ((v_result_handle)(value))

type C.v_result_handle = uint

fn C.v_result_make_handle(value uint) C.v_result_handle

fn result_c_handle(value uint) !C.v_result_handle {
	handle := C.v_result_make_handle(value)
	return handle
}

fn option_c_handle(value uint) ?C.v_result_handle {
	handle := C.v_result_make_handle(value)
	return handle
}

fn deferred_result_c_handle(value uint, mut seen []uint) !C.v_result_handle {
	defer { seen << value }
	handle := C.v_result_make_handle(value)
	return handle
}

fn deferred_option_c_handle(value uint, mut seen []uint) ?C.v_result_handle {
	defer { seen << value }
	handle := C.v_result_make_handle(value)
	return handle
}

fn failing_c_handle() !C.v_result_handle {
	return error_with_code('expected handle failure', 37)
}

fn absent_c_handle() ?C.v_result_handle {
	return none
}

fn test_c_numeric_alias_result_preserves_success_value() {
	values := [uint(0), uint(1), uint(0x12345678), ~uint(0)]
	mut deferred_values := []uint{}
	for value in values {
		handle := result_c_handle(value) or { panic('successful C handle became an error: ${err}') }
		assert uint(handle) == value
		optional := option_c_handle(value) or { panic('successful C handle became none') }
		assert uint(optional) == value
		deferred := deferred_result_c_handle(value, mut deferred_values) or { panic(err) }
		assert uint(deferred) == value
		deferred_option := deferred_option_c_handle(value, mut deferred_values) or { panic('none') }
		assert uint(deferred_option) == value
	}
	assert deferred_values.len == 2 * values.len
	for i, value in values {
		assert deferred_values[2 * i] == value
		assert deferred_values[2 * i + 1] == value
	}
}

fn test_c_numeric_alias_result_preserves_failure() {
	if _ := failing_c_handle() {
		assert false, 'an error became a successful C handle'
	} else {
		assert err.msg() == 'expected handle failure'
		assert err.code() == 37
	}
	if _ := absent_c_handle() {
		assert false, 'none became a successful C handle'
	}
}
