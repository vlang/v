fn test_assert_with_newlines_in_the_labels() {
	println('start')
	s := '123
456'
	assert s == '123
456'
	println('done')
}
