fn test_reserved_keywords() {
	@continue := 'abc'
	@sizeof := 'def'
	@union := 'xyz'
	assert [@continue, @sizeof, @union] == ['abc', 'def', 'xyz']
}
