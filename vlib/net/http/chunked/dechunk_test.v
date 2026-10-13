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

fn test_malformed_chunk_size_is_an_error() {
	// a chunk size that is not hexadecimal is malformed input, not the
	// zero-size terminator: it must error instead of silently returning
	// an empty (or truncated) body.
	// See https://github.com/vlang/v/issues/30444
	for bad in ['ZZ\r\nhello\r\n0\r\n\r\n', '\r\nhello\r\n0\r\n\r\n',
		'100000000\r\n' + 'A'.repeat(100) + '\r\n0\r\n\r\n'] {
		if _ := decode(bad) {
			assert false, 'malformed chunk size must error: ${bad[..10]}...'
		}
	}
	// well-formed bodies still decode, including multi-chunk ones
	assert decode('4\r\nWiki\r\n0\r\n\r\n')! == 'Wiki'
}
