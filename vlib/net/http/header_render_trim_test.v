module http

import strings

// The spaces and tabs at the start and end of a field value are optional
// whitespace (OWS), not part of the value (RFC 9110 section 5.5). Like Go's
// `Header.Write`, `Header.render` does not write them, while the stored value
// keeps them. See https://github.com/vlang/v/issues/30617 .

fn test_render_trims_spaces_around_a_value_and_storage_keeps_them() {
	mut h := new_header()
	h.add_custom('X-Pad', '  padded  ')!
	assert h.render() == 'X-Pad: padded\r\n'
	assert h.get_custom('X-Pad')? == '  padded  '
	assert h.custom_values('X-Pad') == ['  padded  ']
	assert h.str() == 'X-Pad: padded\r\n'
}

fn test_render_trims_tabs_and_keeps_whitespace_inside_a_value() {
	for value, wire in {
		'\tvalue\t':            'value'
		' \t a  b\tc \t ':      'a  b\tc'
		'text/html':            'text/html'
		'col1\tcol2':           'col1\tcol2'
		'\x0bvt and ff\x0c':    '\x0bvt and ff\x0c'
		' \x01 control bytes ': '\x01 control bytes'
		'h\xc3\xa9llo \xff ':   'h\xc3\xa9llo \xff'
	} {
		mut h := new_header()
		h.add_custom('X-Foo', value)!
		assert h.render() == 'X-Foo: ${wire}\r\n', value.bytes().str()
		assert h.custom_values('X-Foo') == [value]
	}
}

fn test_a_value_of_only_whitespace_renders_like_an_empty_value() {
	mut empty := new_header()
	empty.add_custom('X-Foo', '')!
	assert empty.render() == 'X-Foo: \r\n'
	for value in [' ', '\t', '   ', ' \t \t '] {
		mut h := new_header()
		h.add_custom('X-Foo', value)!
		assert h.render() == empty.render()
		assert h.custom_values('X-Foo') == [value]
	}
}

fn test_every_writer_renders_the_trimmed_value() {
	value := ' \tv  1\t '
	mut h := new_header()
	h.add(.location, value)
	h.set(.etag, value)
	h.add_map({
		CommonHeader.vary: value
	})
	h.add_custom('X-Foo', value)!
	h.set_custom('X-Bar', value)!
	h.add_custom_map({
		'X-Baz': value
	})!
	assert h.render() == 'Location: v  1\r\nETag: v  1\r\nVary: v  1\r\nX-Foo: v  1\r\nX-Bar: v  1\r\nX-Baz: v  1\r\n'
	for k in h.keys() {
		assert h.custom_values(k) == [value]
	}
	assert new_header().join(h).render() == h.render()
	assert new_header(key: .location, value: value).render() == 'Location: v  1\r\n'
	assert new_header_from_map({
		CommonHeader.location: value
	}).render() == 'Location: v  1\r\n'
	assert new_custom_header_from_map({
		'X-Foo': value
	})!.render() == 'X-Foo: v  1\r\n'
	// the other render flags write the same value
	assert h.render(canonicalize: true) == h.render()
	assert h.render(version: .v2_0) == h.render().to_lower()
	mut sb := strings.new_builder(64)
	h.render_into_sb(mut sb)
	assert sb.str() == h.render()
}

fn test_a_value_with_cr_lf_replaced_is_trimmed_after_the_replacement() {
	// add stores a space for each CR and LF, which Go also does before it
	// trims the value.
	mut h := new_header()
	h.add(.location, '\r\n/next\r\n')
	assert h.values(.location) == ['  /next  ']
	assert h.render() == 'Location: /next\r\n'
}

fn test_the_response_writers_send_the_trimmed_value() {
	mut resp := new_response(
		status: .ok
		header: new_header(key: .content_type, value: ' text/plain ')
		body:   'ok'
	)
	resp.header.add_custom('X-Pad', '  padded  ')!
	resp.header.add_custom('X-Empty', ' \t ')!
	expected := 'HTTP/1.1 200 OK\r\n' + 'Content-Type: text/plain\r\n' + 'Content-Length: 2\r\n' +
		'X-Pad: padded\r\n' + 'X-Empty: \r\n' + '\r\n' + 'ok'
	assert resp.bytestr() == expected
	assert resp.bytes().bytestr() == expected
	mut sb := strings.new_builder(128)
	resp.write_to(mut sb)
	assert sb.bytestr() == expected
	// what a client reads back is the value without the whitespace
	parsed := parse_response(expected)!
	assert parsed.header.custom_values('X-Pad') == ['padded']
	assert parsed.header.get(.content_type)? == 'text/plain'
}

fn test_the_request_writer_also_sends_the_trimmed_value() {
	mut req := Request{}
	req.header.add_custom('X-Pad', '  padded  ')!
	raw := req.build_request_headers(.get, 'example.com', 80, '/')!
	assert raw.contains('\r\nX-Pad: padded\r\n'), raw
	assert req.header.custom_values('X-Pad') == ['  padded  ']
}
