// An `==`/`!=` operand narrowed by `x != none` is compared as its declared
// option when the other operand is an option (`doc != x`), instead of being
// rejected as an unwrapped payload.
@[heap]
struct OptCmpDoc {
	n int
}

fn opt_cmp_rejects_other_document(document ?&OptCmpDoc, parser_document ?&OptCmpDoc) bool {
	if parser_document != none && document != parser_document {
		return false
	}
	return true
}

fn opt_cmp_same_then_unwrap(document ?&OptCmpDoc, parser_document ?&OptCmpDoc) int {
	if parser_document != none && parser_document == document {
		return parser_document.n
	}
	return -1
}

fn opt_cmp_scalar(a ?int, b ?int) bool {
	return a != none && b == a
}

fn test_option_compare_with_narrowed_pointer_operand() {
	first := &OptCmpDoc{
		n: 1
	}
	second := &OptCmpDoc{
		n: 2
	}
	assert opt_cmp_rejects_other_document(first, first)
	assert !opt_cmp_rejects_other_document(first, second)
	assert !opt_cmp_rejects_other_document(none, first)
	assert opt_cmp_rejects_other_document(first, none)
	assert opt_cmp_rejects_other_document(none, none)
	assert opt_cmp_same_then_unwrap(second, second) == 2
	assert opt_cmp_same_then_unwrap(first, second) == -1
	assert opt_cmp_same_then_unwrap(none, second) == -1
}

fn test_option_compare_with_narrowed_scalar_operand() {
	assert opt_cmp_scalar(3, 3)
	assert !opt_cmp_scalar(3, 4)
	assert !opt_cmp_scalar(3, none)
	assert !opt_cmp_scalar(none, none)
}
