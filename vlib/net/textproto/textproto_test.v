module textproto

fn test_canonical() {
	assert canonical_mime_header_key('content-type') == 'Content-Type'
	assert canonical_mime_header_key('x-custom-header') == 'X-Custom-Header'
	assert canonical_mime_header_key('') == ''
	assert canonical_mime_header_key('a') == 'A'
}

fn test_read_mime_header() {
	text := 'Content-Type: text/plain\nX-Custom: 1\nX-Custom: 2\n'
	h := read_mime_header(text)
	assert h['Content-Type'][0] == 'text/plain'
	assert h['X-Custom'].len == 2
	assert h['X-Custom'][0] == '1'
	assert h['X-Custom'][1] == '2'
}

fn test_continuation() {
	text := 'X-Long: first\n  second\n'
	h := read_mime_header(text)
	assert h['X-Long'][0] == 'first second'
}

fn test_blank_terminates() {
	text := 'A: 1\n\nB: 2\n'
	h := read_mime_header(text)
	assert 'A' in h
	assert 'B' !in h
}
