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

fn test_canonical_valid_token_characters() {
	assert canonical_mime_header_key("x!#$%&'*+.^_`|~-CUSTOM") == "X!#$%&'*+.^_`|~-Custom"
	assert canonical_mime_header_key('-a--BC-') == '-A--Bc-'
	assert canonical_mime_header_key('eTAG') == 'Etag'
}

fn test_canonical_invalid_names_are_unchanged() {
	for key in ['bad name', 'bad:name', 'bad\tname', 'bad\rname', 'bad\nname', 'bad\x00name', 'bad\x7fname',
		'bad/name', 'bad=name', 'é-TYPE'] {
		assert canonical_mime_header_key(key) == key
	}
}

fn test_header_line_endings_values_and_repeated_keys() {
	h := read_mime_header('x-key: \tfirst: value \t\r\nX-KEY: second\n\t continued \t\r\nEmpty:\r\n\r\nBody: ignored')
	assert h['X-Key'] == ['first: value', 'second continued']
	assert h['Empty'] == ['']
	assert h.len == 2
	assert read_mime_header('Last: no newline')['Last'] == ['no newline']
}

fn test_invalid_line_breaks_continuation_association() {
	for invalid in ['missing colon', ': empty name', 'Bad Name: value', 'Bad : value', 'Bad\tName: value',
		'Bad/Name: value', 'Bad: value\x00', 'Bad: value\x7f', 'Bad: value\rInjected: value'] {
		h := read_mime_header('Good: first\n${invalid}\n continued\nNext: last')
		assert h.len == 2
		assert h['Good'] == ['first']
		assert h['Next'] == ['last']
	}
}

fn test_invalid_and_orphan_continuations_are_ignored() {
	h := read_mime_header(' orphan\nGood: first\n invalid\x00\n another\nNext: last')
	assert h.len == 2
	assert h['Good'] == ['first']
	assert h['Next'] == ['last']
}

fn test_header_values_preserve_non_ascii_bytes() {
	value := [u8(0xc2), 0xa0, 0xff].bytestr()
	h := read_mime_header('X-Value: \t${value}\t \r\n continuation\t${value}\t ')
	assert h['X-Value'] == ['${value} continuation\t${value}']
}

fn test_empty_initial_value_does_not_add_leading_fold_whitespace() {
	for initial in ['', ' \t'] {
		h := read_mime_header('X: ${initial}\n \t\n continued\n\tmore\nX:\n final\nEmpty:\n \t')
		assert h['X'] == ['continued more', 'final']
		assert h['Empty'] == ['']
	}
}

fn test_bare_carriage_return_is_not_a_line_ending() {
	assert read_mime_header('Bad: value\rInjected: value\nGood: safe') == {
		'Good': ['safe']
	}
	assert read_mime_header('Bad: value\r').len == 0
}
