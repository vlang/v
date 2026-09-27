fn is_byte_type[T]() bool {
	$if T is byte {
		return true
	} $else {
		return false
	}
}

fn test_deprecated_byte_alias_remains_available_in_comptime_type_checks() {
	assert is_byte_type[u8]()
}
