module json2

import encoding.utf8

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

fn assert_json5_decode_error_context[T](input string, line int, character int, context string) {
	decode[T](input, json5: true, strict: true) or {
		if err is JsonDecodeError {
			assert err.line == line
			assert err.character == character
			assert utf8.validate_str(err.context), err.context
			assert err.context == context
		} else {
			assert false, err.msg()
		}
		return
	}
	assert false, 'expected a typed JSON5 decoding error'
}

fn test_json5_typed_error_preview_trims_prefix_by_code_points() {
	for ch in ['é', '界', '🎼'] {
		input := "{'" + ch.repeat(75) + "': false}"
		context := '...' + ch.repeat(47) + "': \\e[31mfalse\\e[0m}\n" +
			' '.repeat(53) + '\\e[31m~~~~~\\e[0m'
		assert_json5_decode_error_context[map[string]int](input, 1, 81, context)
	}
	input := '{' + 'é'.repeat(75) + ': false}'
	context := '...' + 'é'.repeat(48) + ': \\e[31mfalse\\e[0m}\n' +
		' '.repeat(53) + '\\e[31m~~~~~\\e[0m'
	assert_json5_decode_error_context[map[string]int](input, 1, 79, context)
	assert_json5_decode_error_context[map[string]int]('\uFEFF' + input, 1, 79, context)
	assert_json5_decode_error_context[int]('\uFEFFfalse', 1, 1,
		'\\e[31mfalse\\e[0m\n\\e[31m~~~~~\\e[0m')
}

fn test_json5_typed_error_preview_prefix_cutoff_boundary() {
	for count in [44, 45, 46] {
		input := "{'" + 'é'.repeat(count) + "': false}"
		prefix := if count == 46 { "...'" } else { "{'" }
		columns := if count == 46 { 53 } else { count + 5 }
		context := prefix + 'é'.repeat(count) + "': \\e[31mfalse\\e[0m}\n" +
			' '.repeat(columns) + '\\e[31m~~~~~\\e[0m'
		assert_json5_decode_error_context[map[string]int](input, 1, count + 6, context)
	}
}

fn test_json5_typed_error_preview_highlight_and_suffix_end_on_rune_boundaries() {
	for ch in ['é', '界', '🎼'] {
		input := "'" + ch.repeat(60) + "'"
		context := "\\e[31m'" + ch.repeat(49) + '\\e[0m' + ch.repeat(5) + '\n' +
			'\\e[31m' + '~'.repeat(50) + '\\e[0m'
		assert_json5_decode_error_context[int](input, 1, 1, context)
		trailing := 'false/*' + ch.repeat(4) + '*/'
		trailing_context := '\\e[31mfalse\\e[0m/*' + ch.repeat(3) + '\n\\e[31m~~~~~\\e[0m'
		assert_json5_decode_error_context[int](trailing, 1, 1, trailing_context)
	}
}

fn test_json5_typed_error_preview_tabs_use_visible_padding() {
	for count in [1, 6, 7, 64] {
		cutoff := count > 6
		padding := if cutoff { 51 } else { count * 8 }
		prefix := if cutoff { '...' } else { '' }
		context := prefix + ' '.repeat(if cutoff { 48 } else { count * 8 }) +
			'\\e[31mfalse\\e[0m\n' + ' '.repeat(padding) + '\\e[31m~~~~~\\e[0m'
		assert_json5_decode_error_context[int]('\t'.repeat(count) + 'false', 1, count * 8 + 1,
			context)
	}
	input := '\t'.repeat(7) + ' /*é*/ false'
	context := '...' + ' '.repeat(40) + ' /*é*/ \\e[31mfalse\\e[0m\n' +
		' '.repeat(50) + '\\e[31m~~~~~\\e[0m'
	assert_json5_decode_error_context[int](input, 1, 64, context)
	assert_json5_decode_error_context[int]("'é\tz'", 1, 1,
		"\\e[31m'é        z'\\e[0m\n\\e[31m" + '~'.repeat(12) + '\\e[0m')
}

fn test_json5_typed_error_preview_stays_on_source_line() {
	for separator in ['\n', '\r', '\r\n', '\u2028', '\u2029'] {
		input := '// previous' + separator + "{'" + 'é'.repeat(75) + "': false}" +
			separator + '// next'
		context := '...' + 'é'.repeat(47) + "': \\e[31mfalse\\e[0m}\n" +
			' '.repeat(53) + '\\e[31m~~~~~\\e[0m'
		assert_json5_decode_error_context[map[string]int](input, 2, 81, context)
		assert_json5_decode_error_context[int]('{' + separator + 'name: 1}', 1, 1,
			'\\e[31m{\\e[0m\n\\e[31m~\\e[0m')
	}
	for separator in ['\u2028', '\u2029'] {
		assert_json5_decode_error_context[int]("'é" + separator + "rest'", 1, 1,
			"\\e[31m'é\\e[0m\n\\e[31m~~\\e[0m")
	}
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
	assert_json5_decode_error[Json5ErrorFields](input, 1, input[3..position].runes().len + 1,
		'false')
	assert_json5_decode_error[int]('\uFEFF\u00A0false', 1, 2, 'false')
	assert_json5_decode_error[int]('// heading\r\n\tfalse', 2, 9, 'false')
}

fn test_json5_typed_error_columns_count_code_points() {
	assert_json5_decode_error[map[string]int]('{é: false}', 1, 5, 'false')
	assert_json5_decode_error[map[string]int]('{界: false}', 1, 5, 'false')
	assert_json5_decode_error[map[string]int]('{e\u0301: false}', 1, 6, 'false')
	assert_json5_decode_error[map[string]int]("{'🎼': false}", 1, 7, 'false')
	assert_json5_decode_error[map[string]int]('{"é🎼": false}', 1, 8, 'false')
	assert_json5_decode_error[int]('/* 🎼 é */ false', 1, 11, 'false')
	assert_json5_decode_error[map[string]int](r'{\u00e9: false}', 1, 10, 'false')
	assert_json5_decode_error[map[string]int]('{\té:\tfalse}', 1, 20, 'false')
	for separator in ['\n', '\r', '\r\n', '\u2028', '\u2029'] {
		input := '// 🎼 é' + separator + '{é: false}'
		assert_json5_decode_error[map[string]int](input, 2, 5, 'false')
	}
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

fn test_json5_typed_error_counts_each_line_terminator_and_limits_context() {
	for separator in ['\n', '\r', '\r\n', '\u2028', '\u2029'] {
		input := '// heading' + separator + '/* another line */' + separator +
			'\tfalse' + separator + '/* following line */'
		decode[int](input, json5: true) or {
			if err is JsonDecodeError {
				assert err.line == 3
				assert err.character == 9
				assert err.context.starts_with('        \\e[31mfalse\\e[0m\n')
				assert !err.context.contains('heading')
				assert !err.context.contains('another line')
				assert !err.context.contains('following line')
			} else {
				assert false, err.msg()
			}
			continue
		}
		assert false, 'expected a typed JSON5 decoding error'
	}
	assert_json5_decode_error[int]('// heading\r\n\r\u2028\u2029false', 5, 1, 'false')
}
