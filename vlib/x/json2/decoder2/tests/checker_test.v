import x.json2.decoder2 as json
import x.json2

fn test_check_if_json_match() {
	// /* Test wrong string values */
	mut has_error := false

	json.decode[string]('{"key": "value"}') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected string, but got object'
		}
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	json.decode[map[string]string]('"value"') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected object, but got string_'
		}
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	json.decode[[]int]('{"key": "value"}') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected array, but got object'
		}
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	json.decode[string]('[1, 2, 3]') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected string, but got array'
		}
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	json.decode[int]('{"key": "value"}') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected number, but got object'
		}
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	json.decode[bool]('{"key": "value"}') or {
		if err is json.JsonDecodeError {
			assert err.line == 1
			assert err.character == 1
			assert err.message == 'Data: Expected boolean, but got object'
		}
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	// /* Right string values */
	json.decode[string]('"value"') or { assert false }

	json.decode[map[string]string]('{"key": "value"}') or { assert false }

	json.decode[[]int]('[1, 2, 3]') or { assert false }

	json.decode[string]('"string"') or { assert false }

	json.decode[int]('123') or { assert false }

	json.decode[bool]('true') or { assert false }

	json.decode[bool]('false') or { assert false }
}

fn test_check_json_format() {
	json_and_error_message := [
		{
			'json':  ']'
			'error': 'Syntax: unknown value kind'
		},
		{
			'json':  '}'
			'error': 'Syntax: unknown value kind'
		},
		{
			'json':  'truely'
			'error': 'Syntax: invalid value. Unexpected character after boolean end'
		},
		{
			'json':  '0[1]'
			'error': 'Syntax: invalid value. Unexpected character after number end'
		},
		{
			'json':  '[1, 2, g3]'
			'error': 'Syntax: unknown value kind'
		},
		{
			'json':  '[1, 2,, 3]'
			'error': 'Syntax: unknown value kind'
		},
		{
			'json':  '{"key": 123'
			'error': 'Syntax: EOF error: braces are not closed'
		},
		{
			'json':  '{"key": 123,'
			'error': 'Syntax: EOF error: Expecting object key after `,`'
		},
		{
			'json':  '{"key": 123, "key2": 456,}'
			'error': 'Syntax: Expecting object key after `,`'
		},
		{
			'json':  '[[1, 2, 3], [4, 5, 6],]'
			'error': 'Syntax: Cannot use `,`, before `]`'
		},
	]

	for json_and_error in json_and_error_message {
		mut has_error := false

		json.decode[json2.Any](json_and_error['json']) or {
			if err is json.JsonDecodeError {
				assert err.message == json_and_error['error']
			}
			has_error = true
		}
		assert has_error, 'Expected error ${json_and_error['error']}'
	}
}
