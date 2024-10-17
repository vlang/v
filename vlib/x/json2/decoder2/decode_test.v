module decoder2

fn test_nodes() {
	mut nodes := []Node{}

	mut decoder := Decoder{
		json: '{"val": "2"}'
	}

	decoder.fulfill_nodes(mut nodes)

	assert nodes.len == 1
	assert nodes[0].key_pos == 2
	assert nodes[0].key_len == 3
	assert nodes[0].children == none
	nodes = []

	decoder = Decoder{
		json: '{"val": 0, "val1": 1}'
	}
	decoder.fulfill_nodes(mut nodes)

	assert nodes.len == 2
	assert nodes[0].key_pos == 2
	assert nodes[0].key_len == 3

	assert nodes[1].key_pos == 12
	assert nodes[1].key_len == 4

	nodes = []

	decoder = Decoder{
		json: '{"val": {"val": 2}}'
	}
	decoder.fulfill_nodes(mut nodes)

	assert nodes.len == 1
	assert nodes[0].children != none
	assert nodes[0].children?.len == 1
	assert nodes[0].children?[0].key_pos == 10
	assert nodes[0].children?[0].children == none
}

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
			json:        variable
		}

		checker.check_json_format(variable) or { assert false, err.str() }
		assert checker.checker_idx == checker.json.len - 1, 'Expected to reach the end of the json string ${checker.json}'
	}

	// simple objects
	for variable in ['{}', '{"key": null}', '{"key": "value"}', '{"key": 123}', '{"key": true}'] {
		mut checker := Decoder{
			checker_idx: 0
			json:        variable
		}

		checker.check_json_format(variable) or { assert false, err.str() }
		assert checker.checker_idx == checker.json.len - 1, 'Expected to reach the end of the json string ${checker.json}'
	}

	// Nested objects
	for variable in ['{"key": {"key": 123}}'] {
		mut checker := Decoder{
			checker_idx: 0
			json:        variable
		}

		checker.check_json_format(variable) or { assert false, err.str() }
		assert checker.checker_idx == checker.json.len - 1, 'Expected to reach the end of the json string ${checker.json}'
	}

	// simple arrays
	for variable in ['[]', '[1, 2, 3]', '["a", "b", "c"]', '[true, false]'] {
		mut checker := Decoder{
			checker_idx: 0
			json:        variable
		}

		checker.check_json_format(variable) or { assert false, err.str() }
		assert checker.checker_idx == checker.json.len - 1, 'Expected to reach the end of the json string ${checker.json}'
	}

	// Nested arrays
	for variable in ['[[1, 2, 3], [4, 5, 6]]'] {
		mut checker := Decoder{
			checker_idx: 0
			json:        variable
		}

		checker.check_json_format(variable) or { assert false, err.str() }
		// assert checker.checker_idx == checker.json.len - 1, 'Expected to reach the end of the json string ${checker.json}'
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
			json:        json_and_error['json']
		}

		checker.check_json_format(json_and_error['json']) or {
			assert err.str() == json_and_error['error']
			has_error = true
		}
		assert has_error, 'Expected error ${json_and_error['error']}'
	}
}

fn test_get_value_kind() {
	assert get_value_kind(`"`) == .string_
	assert get_value_kind(`t`) == .boolean
	assert get_value_kind(`f`) == .boolean
	assert get_value_kind(`{`) == .object
	assert get_value_kind(`[`) == .array
	assert get_value_kind(`0`) == .number
	assert get_value_kind(`-`) == .number
	assert get_value_kind(`n`) == .null
	assert get_value_kind(`x`) == .unknown
}

fn test_checker_values_info() {
	// Test for string value
	mut checker := Decoder{
		checker_idx: 0
		json:        '"value"'
	}
	checker.check_json_format(checker.json) or { assert false, err.str() }
	assert checker.values_info.len == 1
	assert checker.values_info[0].position == 0
	assert checker.values_info[0].length == 7
	assert checker.values_info[0].value_kind == .string_

	// Test for number value
	checker = Decoder{
		checker_idx: 0
		json:        '123'
	}
	checker.check_json_format(checker.json) or { assert false, err.str() }
	assert checker.values_info.len == 1
	assert checker.values_info[0].position == 0
	assert checker.values_info[0].length == 3
	assert checker.values_info[0].value_kind == .number

	// Test for boolean value
	checker = Decoder{
		checker_idx: 0
		json:        'true'
	}
	checker.check_json_format(checker.json) or { assert false, err.str() }
	assert checker.values_info.len == 1
	assert checker.values_info[0].position == 0
	assert checker.values_info[0].length == 4
	assert checker.values_info[0].value_kind == .boolean

	// Test for null value
	checker = Decoder{
		checker_idx: 0
		json:        'null'
	}
	checker.check_json_format(checker.json) or { assert false, err.str() }
	assert checker.values_info.len == 1
	assert checker.values_info[0].position == 0
	assert checker.values_info[0].length == 4
	assert checker.values_info[0].value_kind == .null

	// Test for object value
	checker = Decoder{
		checker_idx: 0
		json:        '{"key": "value"}'
	}
	checker.check_json_format(checker.json) or { assert false, err.str() }
	assert checker.values_info.len == 3
	assert checker.values_info[0].position == 0
	assert checker.values_info[0].length == 16
	assert checker.values_info[0].value_kind == .object
	assert checker.values_info[1].position == 1
	assert checker.values_info[1].length == 5
	assert checker.values_info[1].value_kind == .string_
	assert checker.values_info[2].position == 8
	assert checker.values_info[2].length == 7
	assert checker.values_info[2].value_kind == .string_

	// Test for nested object value
	checker = Decoder{
		checker_idx: 0
		// json: '0<-{1"key1": 9<-{10"key2": 18"value1"}}'
		json: '{"key1": {"key2": "value1"}'
	}
	checker.check_json_format(checker.json) or { assert false, err.str() }
	dump(checker.values_info)
	assert checker.values_info.len == 5
	assert checker.values_info[0].position == 0
	assert checker.values_info[0].length == 27
	assert checker.values_info[0].value_kind == .object
	assert checker.values_info[1].position == 1
	assert checker.values_info[1].length == 6
	assert checker.values_info[1].value_kind == .string_
	assert checker.values_info[2].position == 9
	assert checker.values_info[2].length == 18
	assert checker.values_info[2].value_kind == .object
	assert checker.values_info[3].position == 10
	assert checker.values_info[3].length == 6
	assert checker.values_info[3].value_kind == .string_
	assert checker.values_info[4].position == 18
	assert checker.values_info[4].length == 8

	// Test for array value
	checker = Decoder{
		checker_idx: 0
		json:        '[1, 22, 333]'
	}
	checker.check_json_format(checker.json) or { assert false, err.str() }
	assert checker.values_info.len == 4
	assert checker.values_info[0].position == 0
	assert checker.values_info[0].length == 12
	assert checker.values_info[0].value_kind == .array
	assert checker.values_info[1].position == 1
	assert checker.values_info[1].length == 1
	assert checker.values_info[1].value_kind == .number
	assert checker.values_info[2].position == 4
	assert checker.values_info[2].length == 2
	assert checker.values_info[2].value_kind == .number
	assert checker.values_info[3].position == 8
	assert checker.values_info[3].length == 3
	assert checker.values_info[3].value_kind == .number
}
