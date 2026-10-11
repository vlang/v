import v.tests.single_letter_result

fn test_imported_single_letter_result_and_option() {
	result := single_letter_result.get([]u8{})!
	assert result.x == 42
	option := single_letter_result.optional(true) or { panic('missing imported M') }
	assert option.x == 24
	assert single_letter_result.optional(false) == none
}
