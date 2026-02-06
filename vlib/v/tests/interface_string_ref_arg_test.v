module main

import arrays

interface Value {}

fn generate_params(s string) []Value {
	mut params := []Value{}
	params = arrays.concat(params, s)
	return params
}

fn test_interface_string_ref_arg() {
	params := generate_params('any_string')
	assert params == [Value('any_string')]
}
