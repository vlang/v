module main

import arrays
import interface_array_concat_from_another_module.mod

fn test_imported_empty_interface_array_concat_string() {
	mut params := []mod.Value{}
	params = arrays.concat(params, 'hello')
	assert params == [mod.Value('hello')]
}

fn test_imported_empty_interface_array_concat_bytes() {
	bytes := [u8(1), 2, 3]
	mut params := []mod.Value{}
	params = arrays.concat(params, bytes)
	assert params.len == 1
	assert params[0] as []u8 == bytes
}

fn collect(values ...mod.Value) []mod.Value {
	return values
}

fn build_params(include_role bool) []mod.Value {
	user_id_bin := [u8(1), 2, 3]
	password_hash := [u8(4), 5, 6]
	password_salt := [u8(7), 8, 9]
	mut params := [mod.Value(user_id_bin), 'user-id', 'email@example.com', password_hash,
		password_salt]
	if include_role {
		params = arrays.concat(params, 'admin')
	}
	return params
}

fn test_imported_empty_interface_array_literal_and_variadic_forwarding() {
	params := build_params(true)
	got := collect(...params)
	assert got.len == 6
	assert got[0] as []u8 == [u8(1), 2, 3]
	assert got[5] as string == 'admin'
}
