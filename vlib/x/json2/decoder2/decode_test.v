module decoder2

fn test_check_if_json_match() {
	// /* Test wrong string values */
	mut has_error := false

	check_if_json_match[string]('{"key": "value"}') or {
		assert err.str() == 'Expected string, but got object'
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	check_if_json_match[map[string]string]('"value"') or {
		assert err.str() == 'Expected object, but got string_'
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	check_if_json_match[[]int]('{"key": "value"}') or {
		assert err.str() == 'Expected array, but got object'
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	check_if_json_match[string]('[1, 2, 3]') or {
		assert err.str() == 'Expected string, but got array'
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	check_if_json_match[int]('{"key": "value"}') or {
		assert err.str() == 'Expected number, but got object'
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	check_if_json_match[bool]('{"key": "value"}') or {
		assert err.str() == 'Expected boolean, but got object'
		has_error = true
	}
	assert has_error, 'Expected error'
	has_error = false

	// /* Right string values */
	check_if_json_match[string]('"value"') or { assert false }

	check_if_json_match[map[string]string]('{"key": "value"}') or { assert false }

	check_if_json_match[[]int]('[1, 2, 3]') or { assert false }

	check_if_json_match[string]('"string"') or { assert false }

	check_if_json_match[int]('123') or { assert false }

	check_if_json_match[bool]('true') or { assert false }

	check_if_json_match[bool]('false') or { assert false }

	// TODO: test null
}

fn test_check_json_format() {
	// primitives
	for variable in ['""', '"string"', '123', '0', 'true'] {
		mut checker := Decoder{
			checker_idx: 0
			json_str:    variable.str
			json_len:    variable.len
		}

		checker.check_json_format() or { assert false, err.str() }
		assert checker.checker_idx == checker.json_len - 1, 'Expected to reach the end of the json string ${variable}'
	}

	// simple objects
	for variable in ['{}', '{"key": null}', '{"key": "value"}', '{"key": 123}', '{"key": true}'] {
		mut checker := Decoder{
			checker_idx: 0
			json_str:    variable.str
			json_len:    variable.len
		}

		checker.check_json_format() or { assert false, err.str() }
		assert checker.checker_idx == checker.json_len - 1, 'Expected to reach the end of the json string ${variable}'
	}

	// Nested objects
	for variable in ['{"key": {"key": 123}}'] {
		mut checker := Decoder{
			checker_idx: 0
			json_str:    variable.str
			json_len:    variable.len
		}

		checker.check_json_format() or { assert false, err.str() }
		assert checker.checker_idx == checker.json_len - 1, 'Expected to reach the end of the json string ${variable}'
	}

	// simple arrays
	for variable in ['[]', '[1, 2, 3]', '["a", "b", "c"]', '[true, false]'] {
		mut checker := Decoder{
			checker_idx: 0
			json_str:    variable.str
			json_len:    variable.len
		}

		checker.check_json_format() or { assert false, err.str() }
		assert checker.checker_idx == checker.json_len - 1, 'Expected to reach the end of the json string ${variable}'
	}

	// Nested arrays
	for variable in ['[[1, 2, 3], [4, 5, 6]]'] {
		mut checker := Decoder{
			checker_idx: 0
			json_str:    variable.str
			json_len:    variable.len
		}

		checker.check_json_format() or { assert false, err.str() }
		// assert checker.checker_idx == checker.json_len - 1, 'Expected to reach the end of the json string ${variable}'
	}

	// Wrong jsons

	json_and_error_message := [
		{
			'json':  ']'
			'error': '\n]\n^ unknown value kind'
		},
		{
			'json':  '}'
			'error': '\n}\n^ unknown value kind'
		},
		{
			'json':  'truely'
			'error': '\ntruel\n    ^ invalid value. Unexpected character after boolean end'
		},
		{
			'json':  '0[1]' //
			'error': '\n0[\n ^ invalid number'
		},
		{
			'json':  '[1, 2, g3]'
			'error': '\n[1, 2, g\n       ^ unknown value kind'
		},
		{
			'json':  '[1, 2,, 3]'
			'error': '\n[1, 2,,\n      ^ unknown value kind'
		},
		{
			'json':  '{"key": 123'
			'error': '\n{"key": 123\n          ^ EOF error: braces are not closed'
		},
		{
			'json':  '{"key": 123,'
			'error': '\n{"key": 123,\n           ^ EOF error: braces are not closed'
		},
		{
			'json':  '{"key": 123, "key2": 456,}'
			'error': '\n{"key": 123, "key2": 456,}\n                         ^ Expecting object key'
		},
		{
			'json':  '[[1, 2, 3], [4, 5, 6],]'
			'error': '\n[[1, 2, 3], [4, 5, 6],]\n                      ^ Cannot use `,`, before `]`'
		},
	]

	for json_and_error in json_and_error_message {
		mut has_error := false
		mut checker := Decoder{
			checker_idx: 0
			json_str:    json_and_error['json'].str
			json_len:    json_and_error['json'].len
		}

		checker.check_json_format() or {
			assert err.str() == json_and_error['error']
			has_error = true
		}
		assert has_error, 'Expected error ${json_and_error['error']}'
	}
}

fn test_get_value_kind() {

	struct Object_ {
		byte_      u8
		value_kind ValueKind
	}

	array_ := [
		Object_{`"`, .string_},
		Object_{`t`, .boolean},
		Object_{`f`, .boolean},
		Object_{`{`, .object},
		Object_{`[`, .array},
		Object_{`0`, .number},
		Object_{`-`, .number},
		Object_{`n`, .null},
		Object_{`x`, .unknown},
	]

	for value in array_ {
		assert get_value_kind(value.byte_) == value.value_kind
	}
}

pub struct Stru {
	val  int
	val2 string
	val3 Stru2
}

pub struct Stru2 {
	a               int
	brazilian_steak string
}

fn test_decode_from_http_request() {
	json_data := '{"_type": "Stru", "val": 1, "val2": "lala", "val3": {"a": 2, "brazilian_steak": "leleu"}}'
	mut http_request := 'HTTP/1.1 200 OK\r\n'
	http_request += 'Content-Type: application/json\r\n'
	http_request += 'Host: localhost:8080\r\n'
	http_request += 'User-Agent: curl/7.68.0\r\n'
	http_request += 'Accept: */*\r\n'
	http_request += 'Connection: close\r\n'
	http_request += 'Content-Length: ${json_data.len}\r\n'
	http_request += '\r\n'
	http_request += json_data // pos: 150

	mut decoder := Decoder{
		json_str: unsafe { http_request.str + 150 }
		json_len: json_data.len
	}

	decoder.check_json_format()!
	check_if_json_match[Stru](json_data)!

	mut result := Stru{}
	decoder.current_node = decoder.values_info.head
	decoder.decode_value(mut &result)!

	assert result == Stru{
		val:  1
		val2: 'lala'
		val3: Stru2{
			a:               2
			brazilian_steak: 'leleu'
		}
	}
}
