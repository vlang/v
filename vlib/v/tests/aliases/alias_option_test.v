module main

// Test option type alias
pub type MaybeInt = ?int

pub fn is_some(val MaybeInt) bool {
	return val != none
}

fn test_alias_option() {
	v := ?int(42)
	result := is_some(v)
	println('Has value: ${result}')
	assert result == true
}
