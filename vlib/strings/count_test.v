module strings

fn test_count() {
	assert count('abababa', 'aba') == 2
	assert count('hello', 'z') == 0
}
