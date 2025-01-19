fn test_chan_try_push_literal() {
	foo := chan string{}
	foo.try_push('some string')
	assert true
}
