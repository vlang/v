fn test_array() {
	mut strs := ['abc', 'cde']!
	strs[0] += 'def'
	assert strs[0] == 'abcdef'
}

fn test_fixed_array() {
	mut strs := ['abc', 'cde']
	strs[0] += 'def'
	assert strs[0] == 'abcdef'
}
