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
	result_void(false)!
}

fn test_propagation() {
	propagate() or { assert false }
}

fn function_that_can_return_error() !int {
	return error('abc')
}

fn util_error_propagation() ! {
	function_that_can_return_error()!
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

fn return_reference_type(path string) !&string {
	if path.len == 0 {
		return error('vfopen called with ""')
	}
	str := ''
	return &str
}

fn read() !string {
	return ''
}

fn test_results_if_guard() {
	if fcontent := read() {
		assert fcontent == ''
		assert '$fcontent' == ''
		return
	}
	assert false
}

fn res_err_with_code(code int) !string {
	return error_with_code('hi', code)
}

fn test_err_with_code() {
	if w := res_err_with_code(137) {
		assert false
		_ := w
	} else {
		assert err.msg() == 'hi; code: 137'
		assert err.code() == 137
	}
	v := res_err_with_code(56) or {
		assert err.msg() == 'hi; code: 56'
		assert err.code() == 56
		return
	}
	assert false
	_ := v
}
