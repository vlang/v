fn test_reserved_keywords() {
	@continue := 'abc'
	@sizeof := 'def'
	@union := 'xyz'
	assert [@continue, @sizeof, @union] == ['abc', 'def', 'xyz']
}

fn test_duplicated_name() {
	// should just compile
	// @for was compiled to v_for before, now it's _v_for
	@for := 0
	v_for := 0
}
