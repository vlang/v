module main

import arrays

interface Value {}

struct Params {
	email string
}

fn generate_params(s string) []Value {
	mut params := []Value{}
	params = arrays.concat(params, s)
	return params
}

fn generate_selector_params(p Params) []Value {
	mut params := []Value{}
	params = arrays.concat(params, p.email)
	return params
}

fn generate_local_params() []Value {
	s := 'info@peony.com'
	mut params := []Value{}
	params = arrays.concat(params, s)
	return params
}

fn generate_local_selector_params() []Value {
	p := Params{
		email: 'info@peony.com'
	}
	mut params := []Value{}
	params = arrays.concat(params, p.email)
	return params
}

fn test_interface_string_ref_arg() {
	params := generate_params('any_string')
	assert params == [Value('any_string')]
}

fn test_interface_string_ref_local() {
	params := generate_local_params()
	assert params == [Value('info@peony.com')]
}

fn test_interface_string_ref_selector_arg() {
	params := generate_selector_params(Params{
		email: 'info@peony.com'
	})
	assert params == [Value('info@peony.com')]
}

fn test_interface_string_ref_selector_local() {
	params := generate_local_selector_params()
	assert params == [Value('info@peony.com')]
}
