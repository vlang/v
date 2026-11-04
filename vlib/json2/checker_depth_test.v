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
