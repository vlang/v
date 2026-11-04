module strings

fn test_contains_any() {
	assert contains_any('hello', 'aeiou')
	assert !contains_any('xyz', 'aeiou')
}
