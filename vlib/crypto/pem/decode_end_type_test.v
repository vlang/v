module pem

// https://github.com/vlang/v/issues/30691
// The type on the END line was never compared with the type on the BEGIN line.

const mismatched_end_lines = [
	'-----BEGIN RSA PRIVATE KEY-----\nQUJD\n-----END CERTIFICATE-----\n',
	'-----BEGIN CERTIFICATE REQUEST-----\nQUJD\n-----END CERTIFICATE-----\n',
	'-----BEGIN CERTIFICATE-----\nQUJD\n-----END CERTIFICATE REQUEST-----\n',
	'-----BEGIN TEST-----\nQUJD\n-----END test-----\n',
	'-----BEGIN TEST-----\nQUJD\n-----END TEST\n',
	'-----BEGIN TEST-----\nQUJD\n-----END -----\n',
	'-----BEGIN TEST-----\n-----END OTHER-----\n',
	'-----BEGIN TEST-----\nProc-Type: 4,ENCRYPTED\n\nQUJD\n-----END OTHER-----\n',
]

fn test_mismatched_end_type_is_rejected() {
	for input in mismatched_end_lines {
		if _, _ := decode(input) {
			assert false, 'decode should return `none` for `${input}`'
		}
		if _ := decode_only(input) {
			assert false, 'decode_only should return `none` for `${input}`'
		}
	}
}

fn test_matching_end_type_is_accepted() {
	block, rest := decode('-----BEGIN RSA PRIVATE KEY-----\nQUJD\n-----END RSA PRIVATE KEY-----\n') or {
		assert false, 'decode failed'
		return
	}
	assert block.block_type == 'RSA PRIVATE KEY'
	assert block.data == 'ABC'.bytes()
	assert rest == '\n'
}

fn test_text_after_matching_end_line_is_returned_as_rest() {
	// as in `pem_test.v`, text after the closing dashes of the END line is
	// handed back to the caller instead of making the block invalid
	block, rest := decode('-----BEGIN TEST-----\nQUJD\n-----END TEST-----Extra stuff\nmore') or {
		assert false, 'decode failed'
		return
	}
	assert block.data == 'ABC'.bytes()
	assert rest == 'Extra stuff\nmore'
}

fn test_consecutive_blocks_of_different_types() {
	input := '-----BEGIN PRIVATE KEY-----\nQUJD\n-----END PRIVATE KEY-----\n-----BEGIN CERTIFICATE-----\nREVG\n-----END CERTIFICATE-----\n'
	first, rest := decode(input) or {
		assert false, 'decode of the first block failed'
		return
	}
	assert first.block_type == 'PRIVATE KEY'
	assert first.data == 'ABC'.bytes()
	second, last := decode(rest) or {
		assert false, 'decode of the second block failed'
		return
	}
	assert second.block_type == 'CERTIFICATE'
	assert second.data == 'DEF'.bytes()
	assert last == '\n'
}
