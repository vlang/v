// Regression test for https://github.com/vlang/v/issues/27094
// `return $tmpl(...)` from a function whose return type is a Result/Option
// used to emit C code that returned the raw `string` without wrapping it in
// the surrounding `_result_string` / `_option_string` struct.

fn gen_result(flag int) !string {
	if flag <= 0 {
		return error('non-positive flag')
	}
	name := 'abc'
	return $tmpl('tmpl/template.txt')
}

fn gen_option(flag int) ?string {
	if flag <= 0 {
		return none
	}
	name := 'abc'
	return $tmpl('tmpl/template.txt')
}

fn test_tmpl_in_return_result() {
	assert gen_result(1)! == 'abc\n'
	if _ := gen_result(0) {
		assert false
	} else {
		assert err.msg() == 'non-positive flag'
	}
}

fn test_tmpl_in_return_option() {
	assert gen_option(1)? == 'abc\n'
	assert gen_option(0) or { 'none' } == 'none'
}
