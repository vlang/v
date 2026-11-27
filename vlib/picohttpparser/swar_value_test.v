module picohttpparser

// Regression coverage for the 8-byte SWAR fast path in
// get_token_length_to_eol. The pad varies the request-line length so the
// header value starts at every byte alignment; the unaligned u64 load in the
// fast path must behave exactly like the old byte-at-a-time scan.

fn parse_value_at(pad int, value string) !string {
	mut req := Request{}
	path := '/' + 'a'.repeat(pad)
	req.parse_request('GET ${path} HTTP/1.1\r\nX: ${value}\r\n\r\n')!
	return req.headers[0].value
}

pub fn test_value_lengths_at_every_alignment() {
	values := ['', 'a', '1234567', '12345678', '12345678901234567', 'a b c d e f g h']
	for pad in 0 .. 12 {
		for value in values {
			got := parse_value_at(pad, value) or {
				assert false, 'pad=${pad} value="${value}": ${err}'
				''
			}
			assert got == value, 'pad=${pad} got="${got}" want="${value}"'
		}
	}
}

pub fn test_value_with_tab_is_accepted() {
	value := 'ab\tcd\tef'
	for pad in 0 .. 12 {
		got := parse_value_at(pad, value) or {
			assert false, 'pad=${pad}: ${err}'
			''
		}
		assert got == value, 'pad=${pad} got="${got}"'
	}
}

pub fn test_value_with_high_bytes_is_accepted() {
	value := 'ab\xffcd\x80ef'
	for pad in 0 .. 12 {
		got := parse_value_at(pad, value) or {
			assert false, 'pad=${pad}: ${err}'
			''
		}
		assert got == value, 'pad=${pad} got="${got}"'
	}
}

pub fn test_value_with_del_is_rejected() {
	for pad in 0 .. 12 {
		mut got_err := ''
		_ = parse_value_at(pad, 'ab\x7fcd') or {
			got_err = err.msg()
			''
		}
		assert got_err.contains('after header'), 'pad=${pad}: ${got_err}'
	}
}
