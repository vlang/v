module picohttpparser

fn is_header_token_byte(c u8) bool {
	return (c >= `0` && c <= `9`) || (c >= `A` && c <= `Z`)
		|| (c >= `a` && c <= `z`) || c in "!#$%&'*+-.^_`|~".bytes()
}

fn test_header_token_table_covers_every_byte() {
	assert token_char_map.len == 256
	for i in 0 .. 256 {
		assert (token_char_map[i] != 0) == is_header_token_byte(u8(i)), 'byte ${i}'
	}
}

fn test_header_name_accepts_http_token_characters() {
	for i in 0 .. 256 {
		c := u8(i)
		if !is_header_token_byte(c) {
			continue
		}
		name := 'A' + c.ascii_str() + 'B'
		request := 'GET / HTTP/1.1\r\n${name}: value\r\n\r\n'
		mut req := Request{}
		assert req.parse_request(request)! == request.len
		assert req.num_headers == 1
		assert req.headers[0].name == name
		assert req.headers[0].value == 'value'
	}
}

fn test_header_name_rejects_non_token_bytes() {
	for i in 0 .. 256 {
		c := u8(i)
		// A colon ends the header name instead of being part of it.
		if is_header_token_byte(c) || c == `:` {
			continue
		}
		name := 'A' + c.ascii_str() + 'B'
		mut req := Request{}
		if _ := req.parse_request('GET / HTTP/1.1\r\n${name}: value\r\n\r\n') {
			assert false, 'accepted byte ${i} in a header name'
		}
	}
}
