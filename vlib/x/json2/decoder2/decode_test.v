module decoder2

fn test_dummy() {}

// fn test_check_if_json_match() {
// 	// /* Test wrong string values */
// 	mut has_error := false

// 	decode[string]('{"key": "value"}') or {
// 		if err is JsonDecodeError {
// 			assert err.line == 1
// 			assert err.character == 1
// 			assert err.message == 'Data: Expected string, but got object'
// 		}
// 		has_error = true
// 	}
// 	assert has_error, 'Expected error'
// 	has_error = false

// 	decode[map[string]string]('"value"') or {
// 		if err is JsonDecodeError {
// 			assert err.line == 1
// 			assert err.character == 1
// 			assert err.message == 'Data: Expected object, but got string_'
// 		}
// 		has_error = true
// 	}
// 	assert has_error, 'Expected error'
// 	has_error = false

// 	decode[[]int]('{"key": "value"}') or {
// 		if err is JsonDecodeError {
// 			assert err.line == 1
// 			assert err.character == 1
// 			assert err.message == 'Data: Expected array, but got object'
// 		}
// 		has_error = true
// 	}
// 	assert has_error, 'Expected error'
// 	has_error = false

// 	decode[string]('[1, 2, 3]') or {
// 		if err is JsonDecodeError {
// 			assert err.line == 1
// 			assert err.character == 1
// 			assert err.message == 'Data: Expected string, but got array'
// 		}
// 		has_error = true
// 	}
// 	assert has_error, 'Expected error'
// 	has_error = false

// 	decode[int]('{"key": "value"}') or {
// 		if err is JsonDecodeError {
// 			assert err.line == 1
// 			assert err.character == 1
// 			assert err.message == 'Data: Expected number, but got object'
// 		}
// 		has_error = true
// 	}
// 	assert has_error, 'Expected error'
// 	has_error = false

// 	decode[bool]('{"key": "value"}') or {
// 		if err is JsonDecodeError {
// 			assert err.line == 1
// 			assert err.character == 1
// 			assert err.message == 'Data: Expected boolean, but got object'
// 		}
// 		has_error = true
// 	}
// 	assert has_error, 'Expected error'
// 	has_error = false

// 	// /* Right string values */
// 	decode[string]('"value"') or { assert false }

// 	decode[map[string]string]('{"key": "value"}') or { assert false }

// 	decode[[]int]('[1, 2, 3]') or { assert false }

// 	decode[string]('"string"') or { assert false }

// 	decode[int]('123') or { assert false }

// 	decode[bool]('true') or { assert false }

// 	decode[bool]('false') or { assert false }

// 	// TODO: test null
// }

// fn test_check_json_format() {
// 	// primitives
// 	for variable in ['""', '"string"', '123', '0', 'true'] {
// 		mut checker := Decoder{
// 			checker_idx: 0
// 			json:        variable
// 		}

// 		checker.check_json_format(variable) or { assert false, err.str() }
// 		assert checker.checker_idx == checker.json.len, 'Expected to reach the end of the json string ${checker.json}'
// 	}

// 	// simple objects
// 	for variable in ['{}', '{"key": null}', '{"key": "value"}', '{"key": 123}', '{"key": true}'] {
// 		mut checker := Decoder{
// 			checker_idx: 0
// 			json:        variable
// 		}

// 		checker.check_json_format(variable) or { assert false, err.str() }
// 		assert checker.checker_idx == checker.json.len, 'Expected to reach the end of the json string ${checker.json}'
// 	}

// 	// Nested objects
// 	for variable in ['{"key": {"key": 123}}'] {
// 		mut checker := Decoder{
// 			checker_idx: 0
// 			json:        variable
// 		}

// 		checker.check_json_format(variable) or { assert false, err.str() }
// 		assert checker.checker_idx == checker.json.len, 'Expected to reach the end of the json string ${checker.json}'
// 	}

// 	// simple arrays
// 	for variable in ['[]', '[1, 2, 3]', '["a", "b", "c"]', '[true, false]'] {
// 		mut checker := Decoder{
// 			checker_idx: 0
// 			json:        variable
// 		}

// 		checker.check_json_format(variable) or { assert false, err.str() }
// 		assert checker.checker_idx == checker.json.len, 'Expected to reach the end of the json string ${checker.json}'
// 	}

// 	// Nested arrays
// 	for variable in ['[[1, 2, 3], [4, 5, 6]]'] {
// 		mut checker := Decoder{
// 			checker_idx: 0
// 			json:        variable
// 		}

// 		checker.check_json_format(variable) or { assert false, err.str() }
// 		// assert checker.checker_idx == checker.json.len - 1, 'Expected to reach the end of the json string ${checker.json}'
// 	}

// 	// Wrong jsons

// 	json_and_error_message := [
// 		{
// 			'json':  ']'
// 			'error': 'Syntax: unknown value kind'
// 		},
// 		{
// 			'json':  '}'
// 			'error': 'Syntax: unknown value kind'
// 		},
// 		{
// 			'json':  'truely'
// 			'error': 'Syntax: invalid value. Unexpected character after boolean end'
// 		},
// 		{
// 			'json':  '0[1]'
// 			'error': 'Syntax: invalid value. Unexpected character after number end'
// 		},
// 		{
// 			'json':  '[1, 2, g3]'
// 			'error': 'Syntax: unknown value kind'
// 		},
// 		{
// 			'json':  '[1, 2,, 3]'
// 			'error': 'Syntax: unknown value kind'
// 		},
// 		{
// 			'json':  '{"key": 123'
// 			'error': 'Syntax: EOF error: braces are not closed'
// 		},
// 		{
// 			'json':  '{"key": 123,'
// 			'error': 'Syntax: EOF error: Expecting object key after `,`'
// 		},
// 		{
// 			'json':  '{"key": 123, "key2": 456,}'
// 			'error': 'Syntax: Expecting object key after `,`'
// 		},
// 		{
// 			'json':  '[[1, 2, 3], [4, 5, 6],]'
// 			'error': 'Syntax: Cannot use `,`, before `]`'
// 		},
// 	]

// 	for json_and_error in json_and_error_message {
// 		mut has_error := false
// 		mut checker := Decoder{
// 			checker_idx: 0
// 			json:        json_and_error['json']
// 		}

// 		checker.check_json_format(json_and_error['json']) or {
// 			if err is JsonDecodeError {
// 				assert err.message == json_and_error['error']
// 			}
// 			has_error = true
// 		}
// 		assert has_error, 'Expected error ${json_and_error['error']}'
// 	}
// }

// fn test_get_value_kind() {
// 	struct Object_ {
// 		byte_      u8
// 		value_kind ValueKind
// 	}

// 	array_ := [
// 		Object_{`"`, .string_},
// 		Object_{`t`, .boolean},
// 		Object_{`f`, .boolean},
// 		Object_{`{`, .object},
// 		Object_{`[`, .array},
// 		Object_{`0`, .number},
// 		Object_{`-`, .number},
// 		Object_{`n`, .null},
// 		Object_{`x`, .unknown},
// 	]

// 	for value in array_ {
// 		assert get_value_kind(value.byte_) == value.value_kind
// 	}
// }
