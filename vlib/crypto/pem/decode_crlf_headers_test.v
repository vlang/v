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
