module decoder2

struct NestedValueKinds {
	id     int
	active bool
}

struct EscapedStructKey {
	name string
}

type DiscriminatorAnimal = DiscriminatorCat | DiscriminatorDog

struct DiscriminatorCat {
	cat_name string
}

struct DiscriminatorDog {
	dog_name string
}

fn assert_invalid_json(input string) {
	mut checker := Decoder{
		json: input
	}
	mut failed := false
	checker.check_json_format(input) or { failed = true }
	assert failed, 'Expected invalid JSON: `${input}`'
}

fn assert_decode_error[T](input string, expected_error string) {
	mut failed := false
	decode[T](input) or {
		failed = true
		assert err.msg() == expected_error
	}
	assert failed, 'Expected `${input}` to fail decoding'
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

fn test_rejects_trailing_root_bytes() {
	for input in ['truex', '{}x', '[]x', 'nullx', '"value"x', '0x', 'true}', '{}]'] {
		assert_invalid_json(input)
	}
}

fn test_accepts_trailing_root_whitespace() {
	for input in ['true ', '{}\n', '[]\t', 'null\r\n', '"value" \t\r\n', '0\n'] {
		mut checker := Decoder{
			json: input
		}
		checker.check_json_format(input) or { assert false, err.str() }
	}
}

fn test_decode_skips_leading_root_whitespace() {
	assert decode[int](' \t\n\r1')! == 1
	assert decode[bool]('\n true')! == true
	assert decode[[]int]('\r\n [1, 2]')! == [1, 2]
	assert decode[NestedValueKinds]('\n {"id": 1, "active": true}')! == NestedValueKinds{
		id:     1
		active: true
	}
}

fn test_decode_rejects_nested_value_kind_mismatches() {
	assert_decode_error[NestedValueKinds]('{"id": "1", "active": true}',
		'Expected number, but got string_')
	assert_decode_error[NestedValueKinds]('{"id": 1, "active": "yes"}',
		'Expected boolean, but got string_')
	assert_decode_error[[]int]('[1, "2"]', 'Expected number, but got string_')
	assert_decode_error[map[string]int]('{"id": "1"}', 'Expected number, but got string_')
}

fn test_decode_unescapes_map_keys() {
	decoded := decode[map[string]int](r'{"a\nb": 1, "quote\"key": 2, "unicode\u2714": 3}')!
	assert decoded == {
		'a\nb':      1
		'quote"key': 2
		'unicode✔':  3
	}
}

fn test_rejects_unescaped_control_bytes_in_strings() {
	for input in ['"line\nbreak"', '"tab\tcharacter"', '"carriage\rreturn"'] {
		assert_invalid_json(input)
	}
}

fn test_truncated_objects_return_errors() {
	for input in ['{', '{ ', '{\n\t', '{"key"', '{"key":', '{"key": ', '{"key": 1 ', '{"key": null\n',
		'{"key": {}, '] {
		assert_invalid_json(input)
	}
}

fn test_rejects_invalid_delimiters_after_object_values() {
	for input in ['{"a": 1: "b"}', '{"a": true]', '{"a": []: "b"}', '{"a": {}: "b"}'] {
		assert_invalid_json(input)
		mut failed := false
		decode[map[string]int](input) or { failed = true }
		assert failed, 'Expected `${input}` to fail object decoding'
	}
}

fn test_truncated_arrays_return_errors() {
	for input in ['[', '[ ', '[1', '[1 ', '[1,', '[1, ', '[1,\n\t'] {
		assert_invalid_json(input)
		mut failed := false
		decode[[]int](input) or { failed = true }
		assert failed, 'Expected `${input}` to fail array decoding'
	}
}

fn test_decode_unescapes_struct_keys() {
	assert decode[EscapedStructKey](r'{"na\u006de": "Ada"}')! == EscapedStructKey{
		name: 'Ada'
	}
}

fn test_sumtype_discriminator_ignores_nested_values() {
	for input in [
		r'{"meta": {"_type": "DiscriminatorDog"}, "_type": "DiscriminatorCat", "cat_name": "Tom"}',
		r'{"meta": [{"_type": "DiscriminatorDog"}], "_type": "DiscriminatorCat", "cat_name": "Tom"}',
	] {
		assert decode[DiscriminatorAnimal](input)! == DiscriminatorAnimal(DiscriminatorCat{
			cat_name: 'Tom'
		})
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
