module single_letter_result

fn test_single_letter_concrete_result_and_option() {
	result := get([]u8{})!
	assert result.x == 42
	assert (get([]u8{}) or { M{} }).x == 42
	if value := get([u8(1)]) {
		assert false, value.str()
	} else {
		assert err.msg() == 'unexpected data'
	}
	option := optional(true) or { panic('missing M') }
	assert option.x == 24
	assert optional(false) == none
	assert (optional(false) or { M{ x: 99 } }).x == 99
}
