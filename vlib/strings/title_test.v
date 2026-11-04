module strings

fn test_title() {
	assert title('hello world') == 'Hello World'
	assert title('already Title') == 'Already Title'
	assert title('') == ''
}
