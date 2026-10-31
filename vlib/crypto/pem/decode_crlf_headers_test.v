module pem

fn test_encoded_folded_headers_round_trip_with_lf_and_crlf() {
	original := Block{
		block_type: 'TEST'
		headers:    {
			'Proc-Type': ['4,ENCRYPTED']
			'Key-Info':  ['DES-ECB,RSA-MD2,9FD3AAD2F2691B9A']
		}
		data:       'Hello, PEM!'.bytes()
	}
	for ending in ['\n', '\r\n'] {
		for width in [16, 32] {
			encoded := original.encode(line_length: width, line_ending: ending) or {
				panic(err)
			}
			block, rest := decode(encoded + '\nremaining') or {
				assert false, 'decode failed for ${ending.bytes()} and width ${width}'
				return
			}
			assert block == original
			assert rest == '\nremaining'
			assert decode_only(encoded) or { Block{} } == original
		}
	}
}

fn test_lf_header_separator_precedes_crlf_blank_lines_in_body() {
	block := decode_only('-----BEGIN TEST-----\nProc-Type: 4,ENCRYPTED\n\nQUJD\r\n\r\nREVG\n-----END TEST-----\n') or {
		assert false, 'decode_only failed'
		return
	}
	assert block.headers == {
		'Proc-Type': ['4,ENCRYPTED']
	}
	assert block.data == 'ABCDEF'.bytes()
}

fn test_crlf_folded_headers_do_not_bypass_body_validation() {
	input := '-----BEGIN TEST-----\r\nKey-Info: DES-ECB,RSA-MD2,\r\n  9FD3AAD2F2691B9A\r\n\r\nQUJD!\r\n-----END TEST-----\r\n'
	if _ := decode_only(input) {
		assert false, 'invalid base64 after folded headers must be rejected'
	}
}

fn test_blank_lines_in_headerless_body_with_lf_and_crlf() {
	for ending in ['\n', '\r\n'] {
		input := '-----BEGIN TEST-----${ending}QUJD${ending}${ending}REVG${ending}-----END TEST-----${ending}'
		block, rest := decode(input) or {
			assert false, 'decode failed for headerless body with ${ending.bytes()}'
			return
		}
		assert block.headers.len == 0
		assert block.data == 'ABCDEF'.bytes()
		assert rest == ending
	}
}

fn test_crlf_body_blank_line_does_not_extend_an_inline_header() {
	input := '-----BEGIN TEST-----\r\nProc-Type: 4,ENCRYPTED\r\nQUJD\r\n\r\nREVG\r\n-----END TEST-----\r\n'
	block, rest := decode(input) or {
		assert false, 'decode failed for a body without a header separator'
		return
	}
	assert block.headers == {
		'Proc-Type': ['4,ENCRYPTED']
	}
	assert block.data == 'ABCDEF'.bytes()
	assert rest == '\r\n'
}

fn test_crlf_indented_folded_header_with_inline_value() {
	input := '-----BEGIN TEST-----\r\nKey-Info: DES-ECB,RSA-MD2,\r\n  9FD3AAD2F2691B9A\r\n\r\nQUJD\r\n-----END TEST-----\r\n'
	block := decode_only(input) or {
		assert false, 'decode_only failed for an indented continuation'
		return
	}
	assert block.headers == {
		'Key-Info': ['DES-ECB,RSA-MD2,9FD3AAD2F2691B9A']
	}
	assert block.data == 'ABC'.bytes()
}

fn test_crlf_unindented_continuation_after_a_comma() {
	// as in the RFC 1421 examples, also for the last header before the blank line
	input := '-----BEGIN TEST-----\r\nKey-Info: RSA,\r\nI3rRIGXU\r\nwGrtiUm/\r\nMIC-Info: RSA-MD5,RSA,\r\njV2OfH+n\r\n\r\nQUJD\r\n-----END TEST-----\r\n'
	block := decode_only(input) or {
		assert false, 'decode_only failed for unindented continuations'
		return
	}
	assert block.headers == {
		'Key-Info': ['RSA,I3rRIGXUwGrtiUm/']
		'MIC-Info': ['RSA-MD5,RSA,jV2OfH+n']
	}
	assert block.data == 'ABC'.bytes()
}

fn test_lf_headers_with_a_crlf_blank_line() {
	input := '-----BEGIN TEST-----\nKey-Info: \n9FD3AAD2F2691B9A\n\r\nQUJD\n-----END TEST-----\n'
	block := decode_only(input) or {
		assert false, 'decode_only failed for a CRLF blank line after LF headers'
		return
	}
	assert block.headers == {
		'Key-Info': ['9FD3AAD2F2691B9A']
	}
	assert block.data == 'ABC'.bytes()
}

fn test_empty_block_keeps_header_like_trailing_input() {
	for ending in ['\n', '\r\n'] {
		trailer := '${ending}trailer: value${ending}${ending}remaining'
		block, rest := decode('-----BEGIN TEST-----${ending}-----END TEST-----' + trailer) or {
			assert false, 'decode failed for an empty block with ${ending.bytes()}'
			return
		}
		assert block == Block.new('TEST')
		assert rest == trailer
	}
}

fn test_empty_block_followed_by_a_block_with_headers() {
	for ending in ['\n', '\r\n'] {
		second := '-----BEGIN TEST-----${ending}Proc-Type: 4,ENCRYPTED${ending}${ending}REVG${ending}-----END TEST-----'
		first, rest := decode('-----BEGIN TEST-----${ending}-----END TEST-----${ending}${second}') or {
			assert false, 'decode failed for an empty first block with ${ending.bytes()}'
			return
		}
		assert first == Block.new('TEST')
		assert rest == ending + second
		block := decode_only(rest) or {
			assert false, 'decode_only failed for the second block with ${ending.bytes()}'
			return
		}
		assert block.headers == {
			'Proc-Type': ['4,ENCRYPTED']
		}
		assert block.data == 'DEF'.bytes()
	}
}
