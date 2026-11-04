module strings

fn test_reverse() {
	assert reverse('abc') == 'cba'
	assert reverse('') == ''
}
