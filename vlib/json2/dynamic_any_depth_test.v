module json2

fn nested_any_array(levels int) string {
	return '['.repeat(levels) + '0' + ']'.repeat(levels)
}

fn nested_any_object(levels int) string {
	return '{"key":'.repeat(levels) + '0' + '}'.repeat(levels)
}

fn mixed_any_nesting(levels int) string {
	mut parts := []string{cap: levels * 2 + 1}
	for i in 0 .. levels {
		parts << if i % 2 == 0 { '[' } else { '{"key":' }
	}
	parts << '0'
	for i := levels - 1; i >= 0; i-- {
		parts << if i % 2 == 0 { ']' } else { '}' }
	}
	return parts.join('')
}

fn commented_any_array(levels int) string {
	mut parts := []string{cap: levels * 2 + 1}
	for _ in 0 .. levels {
		parts << '['
		parts << '/* nested */'
	}
	parts << '0'
	for _ in 0 .. levels {
		parts << ',]'
	}
	return parts.join('')
}

fn assert_any_depth_error[T](text string, options DecoderOptions, character int) {
	if _ := decode[T](text, options) {
		assert false, 'expected dynamic Any nesting over the limit to return an error'
	} else {
		assert err is JsonDecodeError
		decode_error := err as JsonDecodeError
		assert decode_error.message.contains('maximum nesting depth for dynamic Any values')
		assert decode_error.line == 1
		assert decode_error.character == character
	}
}

fn test_dynamic_any_nesting_at_max_depth_is_accepted() {
	for options in [DecoderOptions{}, DecoderOptions{ json5: true }] {
		arrays := nested_any_array(max_any_nesting_depth)
		assert decode[Any](arrays, options)! is []Any
		objects := nested_any_object(max_any_nesting_depth)
		assert decode[Any](objects, options)! is map[string]Any
		assert decode[Any](mixed_any_nesting(max_any_nesting_depth), options)! is []Any
	}
}

fn test_dynamic_any_nesting_over_max_depth_returns_positioned_error() {
	for options in [DecoderOptions{}, DecoderOptions{ json5: true }] {
		depth := max_any_nesting_depth + 1
		assert_any_depth_error[Any](nested_any_array(depth), options, depth)
		object_error_character := max_any_nesting_depth * '{"key":'.len + 1
		assert_any_depth_error[Any](nested_any_object(depth), options, object_error_character)
		mixed_error_character := max_any_nesting_depth / 2 * ('['.len + '{"key":'.len) + 1
		assert_any_depth_error[Any](mixed_any_nesting(depth), options, mixed_error_character)
	}
}

fn test_sibling_dynamic_any_containers_do_not_accumulate_depth() {
	sub := nested_any_array(max_any_nesting_depth - 1)
	text := '[${sub}, ${sub}]'
	for options in [DecoderOptions{}, DecoderOptions{ json5: true }] {
		assert decode[Any](text, options)! is []Any
	}
}

fn test_dynamic_any_depth_is_shared_by_any_elements_and_parser_entrypoints() {
	// The typed outer array is outside the Any subtree; its Any element can contain
	// up to max_any_nesting_depth additional containers.
	decoded := decode[[]Any](nested_any_array(max_any_nesting_depth + 1), json5: true)!
	assert decoded.len == 1
	assert_any_depth_error[[]Any](nested_any_array(max_any_nesting_depth + 2),
		DecoderOptions{ json5: true }, max_any_nesting_depth + 2)

	max_input := nested_any_array(max_any_nesting_depth)
	assert parse(max_input, json5: true)! is []Any
	assert parse_text(max_input, json5: true)!.root is []Any

	too_deep := nested_any_array(max_any_nesting_depth + 1)
	if _ := parse(too_deep, json5: true) {
		assert false, 'expected parse to reject deep dynamic Any values'
	} else {
		assert err is JsonDecodeError
		decode_error := err as JsonDecodeError
		assert decode_error.character == max_any_nesting_depth + 1
	}
	if _ := parse_text(too_deep, json5: true) {
		assert false, 'expected parse_text to reject deep dynamic Any values'
	} else {
		assert err is JsonDecodeError
		decode_error := err as JsonDecodeError
		assert decode_error.character == max_any_nesting_depth + 1
	}
}

fn test_dynamic_any_depth_comments_do_not_change_the_container_limit() {
	text := commented_any_array(max_any_nesting_depth)
	assert decode[Any](text, json5: true)! is []Any

	too_deep := commented_any_array(max_any_nesting_depth + 1)
	if _ := decode[Any](too_deep, json5: true) {
		assert false, 'expected comments not to bypass the dynamic Any depth limit'
	} else {
		assert err is JsonDecodeError
		decode_error := err as JsonDecodeError
		expected_character := max_any_nesting_depth * ('['.len + '/* nested */'.len) + 1
		assert decode_error.character == expected_character
	}
}

fn test_dynamic_any_depth_error_does_not_poison_reused_buffer() {
	mut buffer := DecodeBuffer{}
	too_deep := nested_any_array(max_any_nesting_depth + 1)
	if _ := decode_reuse[Any](too_deep, mut buffer, json5: true) {
		assert false, 'expected reused decode to reject deep dynamic Any values'
	} else {
		assert err is JsonDecodeError
		decode_error := err as JsonDecodeError
		assert decode_error.character == max_any_nesting_depth + 1
	}
	value := decode_reuse[Any]('{key: [1]}', mut buffer, json5: true)!
	assert value is map[string]Any
}
