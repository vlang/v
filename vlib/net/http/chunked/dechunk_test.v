module chunked

fn test_invalid_chunk() {
	mut is_failure := false

	decode('eee') or { is_failure = true }

	assert is_failure
}

fn test_valid_chunk() {
	chunks := '4\r\nWiki\r\n7\r\npedia i\r\nB\r\nn \r\nchunks.\r\n0\r\n\r\n'
	str := decode(chunks) or { panic('uh oh') }

	assert str == 'Wikipedia in \r\nchunks.'
}
