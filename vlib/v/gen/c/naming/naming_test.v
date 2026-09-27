module naming

@[manualfree]
fn test_sanitize_dotted_name_owns_string_storage() {
	name := '.alpha..beta.'.clone()
	result := sanitize(name)
	unsafe { name.free() }
	assert result == '__alpha____beta__'
	// Check the C terminator as well as the V string contents.
	assert unsafe { result.str[result.len] } == 0
	// A normal string must own a freeable buffer, not an array's interior pointer.
	unsafe { result.free() }
}

fn test_sanitize_dotted_name_autofree() {
	result := sanitize('alpha.beta')
	assert result == 'alpha__beta'
}

fn test_static_type_method_c_name_is_disjoint_from_source_names() {
	static_name := c_name('int@static@tag')
	assert static_name.starts_with('${internal_symbol_c_prefix}static_')
	assert static_name != c_name('int_v_static_v_tag')
	assert static_name != c_name(static_name)
	assert c_name(static_name).starts_with('${internal_symbol_c_prefix}source_')
}

fn test_fn_ptr_encoded_keeps_flat_keys_unchanged() {
	assert fn_ptr_encoded('void', []) == 'fn_ptr:void|void'
	assert fn_ptr_encoded('i64', ['i64', 'void*']) == 'fn_ptr:i64|i64, void*'
	ret, params := fn_ptr_encoded_split('fn_ptr:i64|i64, void*')
	assert ret == 'i64'
	assert fn_ptr_encoded_params(params) == ['i64', 'void*']
	assert fn_ptr_encoded_params('void') == []string{}
}

fn test_fn_ptr_encoded_round_trips_nested_keys() {
	// issue #28935: `fn (int, fn (int, int) int, voidptr)` must keep three parameters
	inner := fn_ptr_encoded('i64', ['i64', 'i64'])
	outer := fn_ptr_encoded('void', ['i64', inner, 'void*'])
	assert outer == 'fn_ptr:void|i64, (fn_ptr:i64|i64, i64), void*'
	ret, params := fn_ptr_encoded_split(outer)
	assert ret == 'void'
	assert fn_ptr_encoded_params(params) == ['i64', inner, 'void*']
	// `fn (int, fn (int) int, int)` must not share the key of the call above
	assert fn_ptr_encoded('void', ['i64', fn_ptr_encoded('i64', ['i64']), 'i64']) != outer
	// nested return and two levels of nesting
	deep := fn_ptr_encoded(inner, [fn_ptr_encoded('i64', [inner, 'i64']), 'i64'])
	deep_ret, deep_params := fn_ptr_encoded_split(deep)
	assert deep_ret == inner
	deep_parts := fn_ptr_encoded_params(deep_params)
	assert deep_parts.len == 2
	assert deep_parts[1] == 'i64'
	nested_ret, nested_params := fn_ptr_encoded_split(deep_parts[0])
	assert nested_ret == 'i64'
	assert fn_ptr_encoded_params(nested_params) == [inner, 'i64']
}
