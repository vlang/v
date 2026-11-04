module strings

fn test_compare() {
	assert compare('a', 'b') == -1
	assert compare('b', 'a') == 1
	assert compare('a', 'a') == 0
}
