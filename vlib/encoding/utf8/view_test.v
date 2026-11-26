// vtest ownership
// vtest vflags: -ownership
module utf8

fn test_view_borrows_valid_utf8() {
	bytes := 'café\x00!'.bytes()
	text := view(&bytes) or { panic('expected valid UTF-8') }
	assert text.as_bytes().data == bytes.data
	assert text.str() == 'café\x00!'
}

fn test_view_supports_empty_utf8() {
	bytes := []u8{}
	text := view(&bytes) or { panic('expected an empty view') }
	assert text.as_bytes().len == 0
	assert text.str() == ''
}

fn test_view_rejects_malformed_utf8() {
	invalid := [u8(0xff)]
	truncated := [u8(0xe2), 0x82]
	assert view(&invalid) == none
	assert view(&truncated) == none
}
