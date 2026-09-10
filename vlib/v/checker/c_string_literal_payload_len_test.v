module checker

fn test_c_string_literal_payload_len() {
	cases := {
		'':             0
		'abc':          3
		'é':            2
		r'\n':          1
		r'\\':          1
		r'\0':          1
		r'\141':        1
		r'\1412':       2
		r'\x61':        1
		r'\x4142':      1
		r'\x41z':       2
		r'\0a':         2
		r'\n\x61\141é': 5
	}
	for value, expected in cases {
		payload_len := c_string_literal_payload_len(value) or { -1 }
		assert payload_len == expected, '${value}: ${payload_len} != ${expected}'
	}
}

fn test_invalid_c_string_literal_payload_len() {
	for value in [r'\q', r'\x', r'\u00e9', r'\U0001f600'] {
		if payload_len := c_string_literal_payload_len(value) {
			assert false, '${value}: unexpectedly accepted with payload length ${payload_len}'
		}
	}
}
