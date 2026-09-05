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
