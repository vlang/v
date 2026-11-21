// vtest vflags: -prod -check-overflow
import json2

// The decoder's word-wise scan for a quote or a backslash wraps on purpose.
fn test_strings_longer_than_a_word_decode_under_check_overflow() {
	decoded := json2.decode[map[string]string]('{"name":"abcdefghijklmnop","escaped":"abcdefgh\\"ijklmnop"}')!
	assert decoded['name'] == 'abcdefghijklmnop'
	assert decoded['escaped'] == 'abcdefgh"ijklmnop'
}

fn test_encoding_a_long_string_scans_it_word_by_word() {
	assert json2.encode('abcdefghijklmnop"q\\r') == '"abcdefghijklmnop\\"q\\\\r"'
	assert json2.encode({
		'name': 'abcdefghijklmnopqrstuvwxyz'
	}) == '{"name":"abcdefghijklmnopqrstuvwxyz"}'
}
