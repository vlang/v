module builtin

fn test_small_string_joins_copy_empty_utf8_and_embedded_nul_parts() {
	cases := [
		['', ''],
		['a', 'b'],
		['a', ''],
		['', 'b'],
		['ab\x00', '\x00cd'],
		['é', '水'],
		['', '', ''],
		['a', '', 'c'],
		['', 'b', 'c'],
		['a', 'b', ''],
		['ab\x00', '\x00', 'cd'],
		['é', '水', '🙂'],
	]
	expected := ['', 'ab', 'a', 'b', 'ab\x00\x00cd', 'é水', '', 'ac', 'bc', 'ab', 'ab\x00\x00cd',
		'é水🙂']
	for i, parts in cases {
		joined := unsafe { string_plus_many(parts.len, &parts[0]) }
		assert joined == expected[i]
		assert unsafe { joined.str[joined.len] } == 0
	}
}

fn test_small_string_joins_own_their_buffer() {
	mut bytes := 'abcdef'.bytes()
	left := unsafe { tos(bytes.data, 2) }
	right := unsafe { tos(&bytes[2], 4) }
	mut parts := [left, right]
	two := unsafe { string_plus_many(parts.len, &parts[0]) }
	parts.insert(1, ':')
	three := unsafe { string_plus_many(parts.len, &parts[0]) }
	bytes[0] = `X`
	bytes[5] = `Y`
	assert two == 'abcdef'
	assert three == 'ab:cdef'
	assert unsafe { two.str[two.len] } == 0
	assert unsafe { three.str[three.len] } == 0
}
