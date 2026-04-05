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

struct OptionalParams {
	email ?string
}

fn generate_optional_params(p OptionalParams) []Value {
	mut params := []Value{}
	if email := p.email {
		params = arrays.concat(params, email)
	}
	return params
}

struct Statement {
}

struct Tx {
}

fn (mut s Statement) execute(params ...Value) !string {
	match params[0] {
		string {
			return params[0] as string
		}
		else {
			return error(typeof(params[0]).name)
		}
	}
}

fn (mut t Tx) prepare() !&Statement {
	return &Statement{}
}

fn (mut t Tx) execute(params ...Value) !string {
	mut stmt := t.prepare()!
	return stmt.execute(...params)!
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

fn test_interface_string_ref_arg_from_option_unwrap_forwarded_through_variadic_call() {
	params := generate_optional_params(OptionalParams{
		email: 'info@peony.com'
	})
	mut tx := Tx{}
	got := tx.execute(...params) or { panic(err) }
	assert got == 'info@peony.com'
}
