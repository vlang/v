module flat

fn assert_source_type_plain_flags_match_legacy(text string) {
	expected_shape := source_type_spelling_shape(text)
	expected_query := source_type_spelling_query(text, expected_shape)
	shape, query := source_type_spelling_flags(text)
	assert shape == expected_shape, text
	assert query == expected_query, text
	if text.len > 0 {
		id := source_type_id(text)
		assert source_type_shape(id) == expected_shape, text
		assert source_type_query(id) == expected_query, text
	}
}

fn test_source_type_plain_flags_keep_raw_builtin_and_provisional_names() {
	for text in ['', '.', '..', '...', '....', 'A', 'C', 'T', 'U', 'V', 'K', '_', '9', 'bool',
		'string', 'void', 'int', 'i8', 'i16', 'i32', 'i64', 'u8', 'u16', 'u32', 'u64', 'f32', 'f64',
		'rune', 'uint', 'isize', 'usize', 'voidptr', 'byteptr', 'charptr', 'char', 'nil', 'none',
		'i128', 'u128', 'array', 'map', 'struct', 'chan', 'thread', 'threaded', 'shared', 'some_name',
		'pkg.struct', 'struct.', '.struct', 'Option', 'Option_', 'Option_T', 'Optionx', 'Option.T',
		'Option__', '__v_option', '__v_option_', '__v_option_T', '__v_optionx', '__v_option.T',
		'Result', 'Result_', 'Result_T', 'Resultx', 'Result.T', 'pkg.Option_T', 'C.', 'JS.', 'main.',
		'.T', 'pkg..T', 'T.U', '_T', 'T_', 'TT', 'T1', '1T', 'pkg.TT', 'pkg.T_', 'pkg.T.'] {
		assert_source_type_plain_flags_match_legacy(text)
	}
	assert source_type_id('') == 0
	assert source_type_shape(0) == unsafe { SourceTypeShape(0) }
	assert source_type_query(0) == SourceTypeQuery.complete
}

fn test_source_type_plain_flags_preserve_combined_query_markers() {
	markers := ['generic', 'nongeneric', 'unknown', 'notunknown', 'typeof', '_T_', 'Arc_', 'struct',
		'thread', '_Ctx', 'Option', '__v_option', 'Result', 'Array_', 'FixedArray_', 'Map_',
		'AnonStruct_', 'AnonUnion_', '__vf_thread_']
	for marker in markers {
		for prefix in ['', 'pkg.', '...', 'C.', 'JS.', 'main.'] {
			for suffix in ['', '.T', '.A', '_Ctx', '.thread', '.struct'] {
				assert_source_type_plain_flags_match_legacy('${prefix}${marker}${suffix}')
			}
		}
		for other in markers {
			assert_source_type_plain_flags_match_legacy('${marker}.${other}.T_Ctx.thread')
		}
	}
}

fn test_source_type_plain_flags_leave_non_name_bytes_to_legacy_classifiers() {
	for text in [' ', '\t', '\n', '\v', '\f', '\r', ' T ', 'pkg. T', 'pkg. T ', 'éT', '東京.T',
		'&', '&&', '?', '!', '[]', '[2]int', 'map[K]V', 'fn (T) U', '([)]', '[(])', '][', ')(',
		'chan int', 'chan\tint', '& mut & chan int', 'shared T', '[]shared T', 'typeof(__expr_1)',
		'Option [T]', ' O p t i o n _ T ', '__v_ option _ T', 'Result [T]', 'int*', 'Arc[T]', 'fn_ptr:T',
		'A@B', 'a/b', 'T\nX', 'pkg.\tT', '$d(flag)', 'a-b', 'T,U', 'map[K]shared V'] {
		assert_source_type_plain_flags_match_legacy(text)
	}
	// Include every byte, including NUL, non-ASCII bytes and all trim characters.
	for code in 0 .. 256 {
		text := u8(code).ascii_str()
		assert_source_type_plain_flags_match_legacy(text)
		assert_source_type_plain_flags_match_legacy('pkg.${text}.T')
	}
}
