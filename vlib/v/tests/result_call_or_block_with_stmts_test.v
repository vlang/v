fn str_ret_fn(str string) string {
	return str
}

fn foo(str string) !string {
	if str.contains('foo') {
		return str
	}
	return error('error')
}

fn test_result_call_or_block_with_stmts() {
	var := foo('bar') or {
		foo_var := str_ret_fn('foo')
		if foo_var == 'foo' {
			foo(foo_var) or {
				eprintln(err)
				exit(1)
			}
		} else {
			eprintln(err)
			exit(1)
		}
	}

	println(var)
	assert var == 'foo'
}
