module pem

// https://github.com/vlang/v/issues/30693
// Without the blank line after the headers, the headers were lost and their
// text was decoded as part of the base64 body.

fn test_header_without_blank_line() {
	block, rest := decode('-----BEGIN TEST-----\nProc-Type: 4,ENCRYPTED\nQUJD\n-----END TEST-----\n') or {
		assert false, 'decode failed'
		return
	}
	assert block.block_type == 'TEST'
	assert block.headers == {
		'Proc-Type': ['4,ENCRYPTED']
	}
	assert block.data == 'ABC'.bytes()
	assert rest == '\n'
}

fn test_several_headers_without_blank_line() {
	block := decode_only('-----BEGIN TEST-----\nProc-Type: 4,ENCRYPTED\nDEK-Info: DES-CBC,BFF968AA74691AC1\nSGVsbG8s\nIFBFTSE=\n-----END TEST-----\n') or {
		assert false, 'decode_only failed'
		return
	}
	assert block.headers == {
		'Proc-Type': ['4,ENCRYPTED']
		'DEK-Info':  ['DES-CBC,BFF968AA74691AC1']
	}
	assert block.data == 'Hello, PEM!'.bytes()
}

fn test_header_without_blank_line_and_crlf() {
	block := decode_only('-----BEGIN TEST-----\r\nProc-Type: 4,ENCRYPTED\r\nQUJD\r\n-----END TEST-----\r\n') or {
		assert false, 'decode_only failed'
		return
	}
	assert block.headers == {
		'Proc-Type': ['4,ENCRYPTED']
	}
	assert block.data == 'ABC'.bytes()
}

fn test_blank_line_and_no_header_forms_are_unchanged() {
	with_blank := decode_only('-----BEGIN TEST-----\nProc-Type: 4,ENCRYPTED\n\nQUJD\n-----END TEST-----\n') or {
		assert false, 'decode_only failed'
		return
	}
	assert with_blank.headers == {
		'Proc-Type': ['4,ENCRYPTED']
	}
	assert with_blank.data == 'ABC'.bytes()

	no_headers := decode_only('-----BEGIN TEST-----\nQUJD\nQUJD\n-----END TEST-----\n') or {
		assert false, 'decode_only failed'
		return
	}
	assert no_headers.headers.len == 0
	assert no_headers.data == 'ABCABC'.bytes()
}
