module fasthttp

// Split-point fuzzing for the exact-length request framing layer: feed every
// growing prefix of a request and assert the framer says "incomplete" (-1) until
// the exact final byte, then reports exactly the message length. This is the
// contract the pipelining read loop relies on.

fn assert_frames_at(req []u8, expected_total int) {
	// Every strict prefix must be incomplete.
	for n in 1 .. req.len {
		got := frame_request_length_lim_idx(req[..n], 0, 0)
		assert got == -1, 'prefix of ${n}/${req.len} bytes should be incomplete, got ${got}'
	}
	// The full buffer must report exactly `expected_total`.
	full := frame_request_length_lim_idx(req, 0, 0)
	assert full == expected_total, 'full buffer should frame at ${expected_total}, got ${full}'
}

fn test_frame_simple_get() {
	req := 'GET / HTTP/1.1\r\nHost: localhost\r\n\r\n'.bytes()
	assert_frames_at(req, req.len)
}

fn test_frame_get_no_headers() {
	req := 'GET / HTTP/1.1\r\n\r\n'.bytes()
	assert_frames_at(req, req.len)
}

fn test_frame_post_with_content_length() {
	body := 'name=fasthttp&x=1'
	req :=
		'POST /submit HTTP/1.1\r\nHost: localhost\r\nContent-Length: ${body.len}\r\n\r\n${body}'.bytes()
	assert_frames_at(req, req.len)
}

fn test_frame_body_larger_than_declared_still_frames_at_declared_end() {
	// Two pipelined POSTs: the first must frame exactly at the end of its declared
	// body, NOT swallow the second request's bytes.
	first := 'POST /a HTTP/1.1\r\nContent-Length: 3\r\n\r\nabc'
	second := 'GET /b HTTP/1.1\r\n\r\n'
	combined := (first + second).bytes()
	first_len := first.len
	got := frame_request_length_lim_idx(combined, 0, 0)
	assert got == first_len, 'first pipelined request should frame at ${first_len}, got ${got}'
	// The remainder must frame as the second complete request.
	rest := combined[got..]
	got2 := frame_request_length_lim_idx(rest, 0, 0)
	assert got2 == second.len, 'second pipelined request should frame at ${second.len}, got ${got2}'
}

fn test_frame_pipelined_three_gets() {
	one := 'GET /1 HTTP/1.1\r\nHost: h\r\n\r\n'
	buf := (one + one + one).bytes()
	mut pos := 0
	mut count := 0
	for pos < buf.len {
		n := frame_request_length_lim_idx(buf[pos..], 0, 0)
		assert n > 0, 'expected a complete request at offset ${pos}, got ${n}'
		assert n == one.len
		pos += n
		count++
	}
	assert count == 3
	assert pos == buf.len
}

fn test_frame_chunked_body() {
	// "Wikipedia" in chunks, terminated by a zero chunk.
	req :=
		'POST /c HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n5\r\npedia\r\n0\r\n\r\n'.bytes()
	assert_frames_at(req, req.len)
}

fn test_frame_incomplete_returns_neg1() {
	// Header block not terminated yet.
	assert frame_request_length_lim_idx('GET / HTTP/1.1\r\nHost: '.bytes(), 0, 0) == -1
	// Content-Length declared but body not fully arrived.
	partial := 'POST /a HTTP/1.1\r\nContent-Length: 10\r\n\r\nshort'.bytes()
	assert frame_request_length_lim_idx(partial, 0, 0) == -1
}

fn test_frame_header_limit_431() {
	req := 'GET / HTTP/1.1\r\nHost: localhost\r\n\r\n'.bytes()
	// A tiny max_header must trip the 431 sentinel.
	assert frame_request_length_lim_idx(req, 8, 0) == frame_err_header
	mut got_code := 0
	frame_request_length_lim(req, 8, 0) or { got_code = err.code() }
	assert got_code == 431
}

fn test_frame_body_limit_413() {
	body := '0123456789'
	req := 'POST /a HTTP/1.1\r\nContent-Length: ${body.len}\r\n\r\n${body}'.bytes()
	// max_body smaller than the declared Content-Length trips 413 before buffering.
	assert frame_request_length_lim_idx(req, 0, 3) == frame_err_body
	mut got_code := 0
	frame_request_length_lim(req, 0, 3) or { got_code = err.code() }
	assert got_code == 413
}

fn test_frame_expected_total_hint() {
	body := 'hello world'
	req := 'POST /a HTTP/1.1\r\nContent-Length: ${body.len}\r\n\r\n${body}'.bytes()
	assert frame_expected_total(req) == req.len
	// Even with only the head present, the hint knows the final size.
	head_only := 'POST /a HTTP/1.1\r\nContent-Length: ${body.len}\r\n\r\n'.bytes()
	assert frame_expected_total(head_only) == head_only.len + body.len
	// Chunked / bodyless requests are not pre-sizable.
	assert frame_expected_total('GET / HTTP/1.1\r\n\r\n'.bytes()) == -1
}

fn test_frame_head_len() {
	head := 'GET /x HTTP/1.1\r\nHost: h\r\n\r\n'
	req := (head + 'BODYBYTES').bytes()
	assert frame_head_len(req) == head.len
	assert frame_head_len('GET / HTTP/1.1\r\nHost: '.bytes()) == -1
}

fn test_frame_public_result_wrappers() {
	req := 'GET / HTTP/1.1\r\nHost: h\r\n\r\n'.bytes()
	assert frame_request_length(req)! == req.len
	assert frame_request_length('GET / HTTP/1.1\r\nHost:'.bytes())! == -1
}

fn test_frame_content_length_overflow_rejected() {
	// A Content-Length far beyond i32 must be rejected (400), not wrapped to a
	// negative int that frames the request as body-less (a smuggling primitive).
	req := 'POST /a HTTP/1.1\r\nContent-Length: 99999999999\r\n\r\n'.bytes()
	assert frame_request_length_lim_idx(req, 0, 0) == frame_err_malformed
	mut code := 0
	frame_request_length_lim(req, 0, 0) or { code = err.code() }
	assert code == 400
}

fn test_frame_chunked_size_overflow_rejected() {
	// A hex chunk size beyond i32 must be rejected, not wrapped negative (which
	// would make the next offset negative and read out of bounds).
	req := 'POST /c HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\nffffffffff\r\nx'.bytes()
	assert frame_request_length_lim_idx(req, 0, 0) == frame_err_malformed
}

fn test_frame_chunked_with_trailers() {
	// A trailing header line after the terminating zero chunk must still frame.
	req :=
		'POST /c HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n4\r\nWiki\r\n0\r\nX-Trailer: v\r\n\r\n'.bytes()
	assert_frames_at(req, req.len)
}

fn test_frame_bare_lf_content_length() {
	// Headers terminated with bare LF (no CR): Content-Length must be measured
	// correctly so the body is framed at its true end, not one byte short.
	body := 'abcdef'
	req := ('POST /a HTTP/1.1\nContent-Length: ${body.len}\n\n' + body).bytes()
	assert frame_request_length_lim_idx(req, 0, 0) == req.len
}

fn test_frame_content_length_trailing_ows() {
	// Trailing whitespace around a Content-Length value must be tolerated.
	body := 'hello'
	req := 'POST /a HTTP/1.1\r\nContent-Length: ${body.len}  \r\n\r\n${body}'.bytes()
	assert frame_request_length_lim_idx(req, 0, 0) == req.len
}
