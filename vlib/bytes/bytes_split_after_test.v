module bytes

fn test_split_after() {
	parts := split_after('a,b,'.bytes(), ','.bytes())
	assert parts.len == 2
}
