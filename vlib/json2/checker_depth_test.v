module json2

// These tests guard against a regression to the stack-overflow abort of the
// checker pass: before max_nesting_depth, deeply nested input killed the
// whole process (`V panic: stack overflow`) at a few thousand `[`/`{` in a
// row instead of returning an error.

fn test_nesting_at_max_nesting_depth_still_parses() {
	arr := '['.repeat(max_nesting_depth) + ']'.repeat(max_nesting_depth)
	a := parse(arr, DecoderOptions{}) or {
		panic('an array nested exactly max_nesting_depth deep must parse, got: ${err.msg()}')
	}
	assert a.str().len > 0

	obj := '{"a":'.repeat(max_nesting_depth) + '0' + '}'.repeat(max_nesting_depth)
	o := parse(obj, DecoderOptions{}) or {
		panic('an object nested exactly max_nesting_depth deep must parse, got: ${err.msg()}')
	}
	assert o.str().len > 0
}

fn test_nesting_over_max_nesting_depth_errors_instead_of_crashing() {
	arr_bad := '['.repeat(max_nesting_depth + 1) + ']'.repeat(max_nesting_depth + 1)
	if a := parse(arr_bad, DecoderOptions{}) {
		// a successful parse here would mean the nesting guard regressed
		// into a potential stack-overflow again
		println('over-limit array parsed: ${a.str().len} chars')
		assert false
	} else {
		assert err.msg().contains('maximum nesting depth')
	}

	obj_bad := '{"a":'.repeat(max_nesting_depth + 1) + '0' + '}'.repeat(max_nesting_depth + 1)
	if a := parse(obj_bad, DecoderOptions{}) {
		println('over-limit object parsed: ${a.str().len} chars')
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
	if a := parse(doc, DecoderOptions{}) {
		println('deep nested document parsed: ${a.str().len} chars')
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
	at_limit := parse(mixed_nesting_string(max_nesting_depth), DecoderOptions{}) or {
		panic('mixed array/object nesting exactly at the limit must parse: ${err.msg()}')
	}
	assert at_limit.str().len > 0

	if a := parse(mixed_nesting_string(max_nesting_depth + 1), DecoderOptions{}) {
		println('over-limit mixed nesting parsed: ${a.str().len} chars')
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
	a := parse('[${sub},${sub},${sub}]', DecoderOptions{}) or {
		panic('three maximal siblings must parse without depth accumulation: ${err.msg()}')
	}
	assert a.str().len > 0
}

// a bounded nesting error must not poison the reused buffer of decode_reuse
fn test_decode_reuse_recovers_after_a_depth_error() {
	mut buffer := DecodeBuffer{}
	over := mixed_nesting_string(max_nesting_depth + 1)
	if v := decode_reuse[Any](over, mut buffer, DecoderOptions{}) {
		println('over-limit reuse input parsed: ${v.str().len} chars')
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
