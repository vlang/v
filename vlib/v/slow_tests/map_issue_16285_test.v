// For issue 16285 Raw string in map literal key triggers compiler error
fn test_raw_string_as_key() {
	mut m := {
		r'key': 1
	}
	assert m[r'key'] == 1

	m[r'key'] = 2
	assert m[r'key'] == 2
}
