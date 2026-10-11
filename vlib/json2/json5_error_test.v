module json2

struct Json5ErrorFields {
	name  string
	count int
}

fn assert_json5_decode_error[T](input string, line int, character int, highlighted string) {
	decode[T](input, json5: true, strict: true) or {
		if err is JsonDecodeError {
			assert err.line == line
			assert err.character == character
			assert err.context.contains('\\e[31m${highlighted}\\e[0m'), err.context
		} else {
			assert false, err.msg()
		}
		return
	}
	assert false, 'expected a typed JSON5 decoding error'
}

fn test_json5_typed_error_after_comments_uses_source_line_and_context() {
	input := "// heading\n{\n  name: 'V',\n  count: /* note */ false,\n}"
	decode[Json5ErrorFields](input, json5: true) or {
		if err is JsonDecodeError {
			assert err.line == 4
			assert err.character == 21
			assert err.context.starts_with('  count: /* note */ \\e[31mfalse\\e[0m,')
			assert !err.context.contains('"count"')
		} else {
			assert false, err.msg()
		}
		return
	}
	assert false, 'expected a typed JSON5 decoding error'
}

fn test_json5_typed_error_after_normalized_strings_and_keys() {
	input := "{name: '\\x41\\u0042', count: true,}"
	position := input.index('true') or { panic('missing value') }
	assert_json5_decode_error[Json5ErrorFields](input, 1, position + 1, 'true')
	quoted := "/* leading */ '\\x31'"
	assert_json5_decode_error[int](quoted, 1, 15, "'\\x31'")
}

fn test_json5_typed_error_preserves_container_source_span() {
	input := '// heading\n  { items: [1, /* note */ 2,], }'
	assert_json5_decode_error[[]int](input, 2, 3, '{ items: [1, /* note */ 2,], }')
	assert_json5_decode_error[map[string]int](input, 2, 12, '[1, /* note */ 2,]')
	assert_json5_decode_error[[]int]('// heading\n[1, /* note */ false,]', 2, 16, 'false')
}

fn test_json5_typed_error_preserves_unicode_and_bom_byte_offsets() {
	input := "\uFEFF{ 'ключ': '🎼', count: false, }"
	position := input.index('false') or { panic('missing value') }
	assert_json5_decode_error[Json5ErrorFields](input, 1, position + 1, 'false')
	assert_json5_decode_error[int]('\uFEFF\u00A0false', 1, 6, 'false')
	assert_json5_decode_error[int]('// heading\r\n\tfalse', 2, 9, 'false')
}

fn test_json5_error_source_mapping_is_independent_of_reused_token_storage() {
	mut buffer := DecodeBuffer{}
	for input in ['// heading\n{count: false,}', "/* heading */ {name:'\\x41',count:false}"] {
		decode_reuse[Json5ErrorFields](input, mut buffer, json5: true) or {
			if err is JsonDecodeError {
				assert err.context.contains('count:')
				assert err.context.contains('\\e[31mfalse\\e[0m')
			} else {
				assert false, err.msg()
			}
			assert decode_reuse[Json5ErrorFields]("{name:'valid',count:1}", mut buffer,
				json5: true
			)!.count == 1
			continue
		}
		assert false, 'expected a typed JSON5 decoding error'
	}
}
