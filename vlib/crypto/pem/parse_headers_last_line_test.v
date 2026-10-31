module pem

// https://github.com/vlang/v/issues/30689
// parse_headers used to stop one line early, so the last header line was dropped.

fn test_single_header_is_parsed() {
	block, rest := decode('-----BEGIN TEST-----\nProc-Type: 4,ENCRYPTED\n\nQUJD\n-----END TEST-----\n') or {
		assert false, 'decode failed'
		return
	}
	assert block.headers == {
		'Proc-Type': ['4,ENCRYPTED']
	}
	assert block.data == 'ABC'.bytes()
	assert rest == '\n'
}

fn test_last_of_several_headers_is_parsed() {
	block := decode_only('-----BEGIN TEST-----\nProc-Type: 4,ENCRYPTED\nDEK-Info: DES-CBC,BFF968AA74691AC1\n\nQUJD\n-----END TEST-----\n') or {
		assert false, 'decode_only failed'
		return
	}
	assert block.headers == {
		'Proc-Type': ['4,ENCRYPTED']
		'DEK-Info':  ['DES-CBC,BFF968AA74691AC1']
	}
	assert block.header_by_key(.dekinfo) == ['DES-CBC,BFF968AA74691AC1']
	assert block.data == 'ABC'.bytes()
}

fn test_last_header_after_a_continuation_line_is_parsed() {
	block := decode_only('-----BEGIN TEST-----\nKey-Info: DES-ECB,RSA-MD2,\n  9FD3AAD2F2691B9A\nDEK-Info: DES-CBC,F8143EDE5960C597\n\nQUJD\n-----END TEST-----\n') or {
		assert false, 'decode_only failed'
		return
	}
	assert block.headers == {
		'Key-Info': ['DES-ECB,RSA-MD2,9FD3AAD2F2691B9A']
		'DEK-Info': ['DES-CBC,F8143EDE5960C597']
	}
	assert block.data == 'ABC'.bytes()
}

fn test_repeated_header_as_last_line_is_kept() {
	block := decode_only('-----BEGIN TEST-----\nRecipient-ID-Symmetric: a,b,1\nRecipient-ID-Symmetric: c,d,2\n\nQUJD\n-----END TEST-----\n') or {
		assert false, 'decode_only failed'
		return
	}
	assert block.headers == {
		'Recipient-ID-Symmetric': ['a,b,1', 'c,d,2']
	}
}

fn test_encoded_headers_round_trip() {
	mut original := Block.new('TEST')
	original.headers['Proc-Type'] = ['4,ENCRYPTED']
	original.headers['DEK-Info'] = ['DES-CBC,BFF968AA74691AC1']
	original.data = 'ABC'.bytes()
	encoded := original.encode() or {
		assert false, 'encode failed'
		return
	}
	decoded := decode_only(encoded) or {
		assert false, 'decode_only failed'
		return
	}
	assert decoded == original
}
