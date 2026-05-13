// Copyright (c) 2019-2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module s3

fn test_uri_encode_path_keeps_slashes() {
	assert uri_encode_path('/foo/bar baz/test.txt') == '/foo/bar%20baz/test.txt'
	assert uri_encode_path('/folder/file with %.txt') == '/folder/file%20with%20%25.txt'
}

fn test_uri_encode_path_normalises_backslashes() {
	// Windows-style separators must collapse to '/' in the canonical key.
	assert uri_encode_path('foo\\bar\\baz.txt') == 'foo/bar/baz.txt'
}

fn test_uri_encode_query_encodes_slashes() {
	assert uri_encode_query('a/b') == 'a%2Fb'
	assert uri_encode_query('hello world') == 'hello%20world'
}

fn test_uri_encode_unreserved_passthrough() {
	// RFC 3986 unreserved set must stay intact.
	assert uri_encode('AZaz09-_.~', false) == 'AZaz09-_.~'
}

fn test_uri_encode_percent_uppercase() {
	// AWS SigV4 mandates uppercase hex digits.
	assert uri_encode('é', true) == '%C3%A9'
}

fn test_strip_slashes_trims_both_ends() {
	assert strip_slashes('/foo/bar/') == 'foo/bar'
	assert strip_slashes('//foo//') == 'foo'
	assert strip_slashes('foo') == 'foo'
	assert strip_slashes('') == ''
	assert strip_slashes('///') == ''
}

fn test_contains_crlf() {
	assert contains_crlf('hello\r\nworld') == true
	assert contains_crlf('hello\rworld') == true
	assert contains_crlf('hello\nworld') == true
	assert contains_crlf('plain') == false
}

fn test_to_hex_lower() {
	assert to_hex_lower([u8(0x00), 0x0f, 0xff, 0xa0]) == '000fffa0'
	assert to_hex_lower([]) == ''
}
