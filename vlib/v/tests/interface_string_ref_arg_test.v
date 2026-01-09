module main

import arrays

interface Value {}

fn generate_params(s string) []Value {
	mut params := []Value{}
	params = arrays.concat(params, s)
	println(params)
	return params
}

fn test_interface_string_ref_arg() {
	params := generate_params('info@peony.com')
	println(params)
}
