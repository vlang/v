fn foo() !int {
	return 1
}

fn test_return_int() {
	x := foo() or { 0 }
	assert x == 1
}

fn foo_err() !int {
	return error('throw')
}

fn test_return_err() {
	x := foo_err() or { 0 }
	assert x == 0
}

fn test_return_err_var() {
	foo_err() or { assert err.msg() == 'throw' }
}

fn test_str() {
	assert '$foo()' == 'result(1)'
}

fn result_void(err bool) ! {
	if err {
		return error('throw')
	}
}

fn test_result_void() {
	result_void(false) or { assert false }
}

fn test_result_void_err() {
	mut or_block := false
	result_void(true) or {
		assert err.msg() == 'throw'
		or_block = true
	}
	assert or_block
}

fn propagate() ! {
	result_void(false) !
}

fn test_propagation() {
	propagate() or { assert false }
}

fn function_that_can_return_error() !int {
	return error('abc')
}

fn util_error_propagation() ! {
	function_that_can_return_error() !
	assert false
}

fn test_return_on_error_propagation() {
	util_error_propagation() or { assert err.msg() == 'abc' }
}

fn unsafe_return_error() !int {
	unsafe {
		return error('abc')
	}
}

fn test_unsafe_return_error() {
	unsafe_return_error() or { assert err.msg() == 'abc' }
}
