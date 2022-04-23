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
