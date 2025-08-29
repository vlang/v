import x.json2
import x.json2.decoder2

fn test_decode_escaped_string() {
	escaped_strings := ['test', 'test\\sd', 'test\nsd', '\ntest', 'test\\"', 'test\\', 'test\u1234ps',
		'test\u1234', '\u1234\\\t"', '']

	json_string := json2.encode[[]string](escaped_strings)
	decoded_strings := decoder2.decode[[]string](json_string)!

	assert escaped_strings == decoded_strings
}

fn test_surrogate() {
	assert decoder2.decode[string](r'"\ud83d\ude00"')! == '😀'
	assert decoder2.decode[string](r'"\ud83d\ude00 text"')! == '😀 text'
}

fn test_invalid_surrogate() {
	if x := decoder2.decode[string](r'"\ud83d"') {
		assert false
	} else {
		if err is decoder2.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected a trail surrogate after a head surrogate, but got no valid escape sequence.'
		}
	}

	if x := decoder2.decode[string](r'"\ud83d\n\n\n\n"') {
		assert false
	} else {
		if err is decoder2.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected a trail surrogate after a head surrogate, but got no valid escape sequence.'
		}
	}

	if x := decoder2.decode[string](r'"\ud83d\ud83d"') {
		assert false
	} else {
		if err is decoder2.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected a trail surrogate after a head surrogate, but got D83D.'
		}
	}

	if x := decoder2.decode[string](r'"\ude00\ud83d"') {
		assert false
	} else {
		if err is decoder2.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Got trail surrogate: DE00 before head surrogate.'
		}
	}
}
