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
