// `if x == none { panic(...) }` and the parenthesized `if (x == none) { return }`
// unwrap `x` after the guard, the same as a plain `return` guard.
fn none_guard_paren_exit(o ?int) int {
	// vfmt off
	if (o == none) {
		return -1
	}
	// vfmt on
	return o + 1
}

fn none_guard_panic_exit(o ?string) int {
	if o == none {
		panic('none_guard_panic_exit: missing value')
	}
	return o.len
}

fn test_none_guard_exit_forms_unwrap_reads() {
	assert none_guard_paren_exit(none) == -1
	assert none_guard_paren_exit(3) == 4
	assert none_guard_panic_exit('four') == 4
}
