fn test_main() {
	a := error
	assert dump(a('foo')) == error('foo')
	assert dump(error) == error
}
