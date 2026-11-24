module json2

// These tests guard against a regression to the stack-overflow abort of the
// checker pass: before max_nesting_depth, deeply nested input killed the
// whole process (`V panic: stack overflow`) at a few thousand `[`/`{` in a
// row instead of returning an error.

fn assert_parser_accepts_typed_input(text string) {
	if _ := decode[int](text, DecoderOptions{}) {
		assert false, 'container input must not decode as a number'
	} else {
		assert err is JsonDecodeError
		assert !err.msg().contains('maximum nesting depth')
	}
}

fn assert_standard_parser_depth_error(text string, character int) {
	if _ := decode[int](text, DecoderOptions{}) {
		assert false, 'expected standard JSON over the parser limit to return an error'
	} else {
		assert err is JsonDecodeError
		decode_error := err as JsonDecodeError
		assert decode_error.line == 1
		assert decode_error.character == character
	}
}

fn test_nesting_at_max_nesting_depth_still_parses() {
	arr := '['.repeat(max_nesting_depth) + ']'.repeat(max_nesting_depth)
	assert_parser_accepts_typed_input(arr)

	obj := '{"a":'.repeat(max_nesting_depth) + '0' + '}'.repeat(max_nesting_depth)
	assert_parser_accepts_typed_input(obj)
}

fn test_nesting_over_max_nesting_depth_errors_instead_of_crashing() {
	arr_bad := '['.repeat(max_nesting_depth + 1) + ']'.repeat(max_nesting_depth + 1)
	assert_standard_parser_depth_error(arr_bad, max_nesting_depth + 1)

	obj_bad := '{"a":'.repeat(max_nesting_depth + 1) + '0' + '}'.repeat(max_nesting_depth + 1)
	if _ := decode[int](obj_bad, DecoderOptions{}) {
		assert false
	} else {
		assert err.msg().contains('maximum nesting depth')
	}
}

// nested values inside a document guarded on either side, deepest last
fn test_deeply_nested_value_inside_a_document_is_rejected() {
	prefix := '{"key": [1, 2, '
	suffix := ', 3]}'
	body := '['.repeat(max_nesting_depth + 1) + ']'.repeat(max_nesting_depth + 1)
	doc := prefix + body + suffix
	if _ := decode[int](doc, DecoderOptions{}) {
		assert false
	} else {
		assert err.msg().contains('maximum nesting depth')
	}
}

// mixed_nesting_string alternates array and object containers so the shared
// counter counts both forms in one chain.
fn mixed_nesting_string(levels int) string {
	if levels <= 0 {
		return '0'
	}
	mut parts := []string{}
	for i in 0 .. levels {
		parts << if i % 2 == 0 { '[' } else { '{"k":' }
	}
	parts << '0'
	for i := levels - 1; i >= 0; i-- {
		parts << if i % 2 == 0 { ']' } else { '}' }
	}
	return parts.join('')
}

fn test_mixed_array_and_object_nesting_at_both_boundaries() {
	assert_parser_accepts_typed_input(mixed_nesting_string(max_nesting_depth))

	if _ := decode[int](mixed_nesting_string(max_nesting_depth + 1), DecoderOptions{}) {
		assert false
	} else {
		assert err.msg().contains('maximum nesting depth')
	}
}

// sibling containers sit at the same level: the deferred decrements must return
// the counter between siblings, or every sibling after the first would be
// rejected. Each of the three reaches exactly max_nesting_depth.
fn test_sibling_containers_do_not_accumulate_depth() {
	sub := '['.repeat(max_nesting_depth - 1) + '0' + ']'.repeat(max_nesting_depth - 1)
	assert_parser_accepts_typed_input('[${sub},${sub},${sub}]')
}

// a bounded nesting error must not poison the reused buffer of decode_reuse
fn test_decode_reuse_recovers_after_a_depth_error() {
	mut buffer := DecodeBuffer{}
	over := mixed_nesting_string(max_nesting_depth + 1)
	if _ := decode_reuse[int](over, mut buffer, DecoderOptions{}) {
		assert false
	} else {
		assert err.msg().contains('maximum nesting depth')
	}
	shallow := '{"a":[1,{"b":[0]}]}'
	norm := decode_reuse[Any](shallow, mut buffer, DecoderOptions{}) or {
		panic('a reused buffer must recover after a bounded error: ${err.msg()}')
	}
	assert norm.str().replace(' ', '') == shallow
}
