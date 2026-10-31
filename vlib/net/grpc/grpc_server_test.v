module grpc

import net.http

// EchoSvc is a hand-rolled GrpcService so the server can be driven without
// generated stubs: /echo.Echo/Ok echoes every request message back and stamps
// leading + trailing metadata; /echo.Echo/Fan turns one request into three
// responses (server-streaming shape); /echo.Echo/Boom fails with a unicode
// message; anything else is a routing miss.
struct EchoSvc {}

fn (mut e EchoSvc) grpc_call(path string, reqs [][]u8, mut ctx ServerContext) !([][]u8, bool) {
	match path {
		'/echo.Echo/Ok' {
			ctx.set_header('x-lead', 'L')
			ctx.set_trailer('x-trail', 'T')
			return reqs, true
		}
		'/echo.Echo/Fan' {
			return [reqs[0], reqs[0], reqs[0]], true
		}
		'/echo.Echo/Boom' {
			return StatusError{
				status: Status{
					code:    .invalid_argument
					message: 'nope 🚀'
				}
			}
		}
		'/echo.Echo/Deadline' {
			// hand the grpc-timeout header back as a response header, so the
			// test can assert what the client actually put on the wire
			ctx.set_header('x-seen-timeout', ctx.header('grpc-timeout'))
			ctx.set_header('x-seen-te', ctx.header('te'))
			return [reqs[0]], true
		}
		else {
			return [][]u8{}, false
		}
	}
}

fn grpc_request_multi(path string, payloads [][]u8) http.Request {
	mut h := http.new_header()
	h.add(.content_type, 'application/grpc+proto')
	mut body := []u8{}
	for p in payloads {
		body << encode_frame(p, false)
	}
	return http.Request{
		method: .post
		url:    path
		header: h
		data:   body.bytestr()
	}
}

fn grpc_request(path string, payload []u8) http.Request {
	return grpc_request_multi(path, [payload])
}

fn new_grpc_test_server() GrpcServer {
	mut s := GrpcServer{}
	s.mount(EchoSvc{})
	return s
}

fn test_grpc_ok_frames_reply_and_sets_status_trailer() {
	mut s := new_grpc_test_server()
	msg := 'hello world'.bytes()
	resp := s.handle(grpc_request('/echo.Echo/Ok', msg))

	assert resp.status_code == 200
	ct := resp.header.get(.content_type) or { '' }
	assert ct.starts_with('application/grpc')
	// body is exactly one uncompressed frame carrying the echoed payload
	frames := decode_frames(resp.body.bytes()) or { panic(err) }
	assert frames.len == 1
	assert !frames[0].compressed
	assert frames[0].payload == msg
	// grpc-status: 0 rides in the trailers
	assert (resp.trailers.get_custom('grpc-status') or { '' }) == '0'
}

fn test_grpc_metadata_leading_vs_trailing() {
	mut s := new_grpc_test_server()
	resp := s.handle(grpc_request('/echo.Echo/Ok', 'x'.bytes()))
	// set_header -> leading response header; set_trailer -> real h2 trailer
	assert (resp.header.get_custom('x-lead') or { '' }) == 'L'
	assert (resp.trailers.get_custom('x-trail') or { '' }) == 'T'
}

fn test_grpc_server_streaming_fans_out_frames() {
	mut s := new_grpc_test_server()
	msg := 'tick'.bytes()
	resp := s.handle(grpc_request('/echo.Echo/Fan', msg))
	assert resp.status_code == 200
	// one request -> three response frames, then grpc-status: 0
	frames := decode_frames(resp.body.bytes()) or { panic(err) }
	assert frames.len == 3
	for f in frames {
		assert f.payload == msg
	}
	assert (resp.trailers.get_custom('grpc-status') or { '' }) == '0'
}

fn test_grpc_client_streaming_reads_every_request_frame() {
	mut s := new_grpc_test_server()
	payloads := ['a'.bytes(), 'bb'.bytes(), 'ccc'.bytes()]
	resp := s.handle(grpc_request_multi('/echo.Echo/Ok', payloads))
	// server saw all three request messages (Ok echoes them straight back)
	frames := decode_frames(resp.body.bytes()) or { panic(err) }
	assert frames.len == 3
	assert frames[0].payload == 'a'.bytes()
	assert frames[2].payload == 'ccc'.bytes()
}

fn test_grpc_error_is_trailers_only_with_encoded_message() {
	mut s := new_grpc_test_server()
	resp := s.handle(grpc_request('/echo.Echo/Boom', 'x'.bytes()))

	assert resp.status_code == 200 // gRPC failures are still HTTP 200
	assert resp.body == '' // no body -> Trailers-Only fold
	assert (resp.trailers.get_custom('grpc-status') or { '' }) == '3' // invalid_argument
	// unicode message round-trips through percent-encoding
	enc := resp.trailers.get_custom('grpc-message') or { '' }
	assert percent_decode(enc) == 'nope 🚀'
}

fn test_grpc_unknown_procedure_is_unimplemented() {
	mut s := new_grpc_test_server()
	resp := s.handle(grpc_request('/echo.Echo/Missing', 'x'.bytes()))
	assert resp.status_code == 200
	assert (resp.trailers.get_custom('grpc-status') or { '' }) == '12' // unimplemented
}

fn test_grpc_rejects_non_grpc_content_type() {
	mut s := new_grpc_test_server()
	mut h := http.new_header()
	h.add(.content_type, 'application/json')
	req := http.Request{
		method: .post
		url:    '/echo.Echo/Ok'
		header: h
		data:   encode_frame('x'.bytes(), false).bytestr()
	}
	resp := s.handle(req)
	assert (resp.trailers.get_custom('grpc-status') or { '' }) == '13' // internal
}

fn test_grpc_rejects_non_post_method() {
	mut s := new_grpc_test_server()
	mut h := http.new_header()
	h.add(.content_type, 'application/grpc+proto')
	resp := s.handle(http.Request{
		method: .get
		url:    '/echo.Echo/Ok'
		header: h
	})
	assert (resp.trailers.get_custom('grpc-status') or { '' }) == '13' // internal
}

// a compressed request frame is a protocol-level rejection, not a crash
fn test_grpc_rejects_compressed_request() {
	mut s := new_grpc_test_server()
	mut h := http.new_header()
	h.add(.content_type, 'application/grpc+proto')
	resp := s.handle(http.Request{
		method: .post
		url:    '/echo.Echo/Ok'
		header: h
		data:   encode_frame('x'.bytes(), true).bytestr()
	})
	assert (resp.trailers.get_custom('grpc-status') or { '' }) == '12' // unimplemented
	assert (resp.trailers.get_custom('grpc-message') or { '' }).contains('compressed')
}

// a truncated frame means the body ended mid-message: a decode failure, which
// the server reports as internal rather than routing it anywhere
fn test_grpc_rejects_truncated_request() {
	mut s := new_grpc_test_server()
	mut h := http.new_header()
	h.add(.content_type, 'application/grpc+proto')
	resp := s.handle(http.Request{
		method: .post
		url:    '/echo.Echo/Ok'
		header: h
		data:   'abc'
	})
	assert (resp.trailers.get_custom('grpc-status') or { '' }) == '13' // internal
}

// test_grpc_timeout_header_reaches_the_handler pins the two request headers
// gRPC requires of every call: `te: trailers`, and `grpc-timeout` when the
// caller set a deadline. Neither is observable without a socket to observe, so
// the handler echoes them back into the response headers instead.
fn test_grpc_timeout_header_reaches_the_handler() {
	mut s := new_grpc_test_server()
	mut h := http.new_header()
	h.add(.content_type, 'application/grpc+proto')
	h.add_custom('te', 'trailers') or {}
	h.add_custom('grpc-timeout', '150000000u') or {} // 150ms, per encode_grpc_timeout
	resp := s.handle(http.Request{
		method: .post
		url:    '/echo.Echo/Deadline'
		header: h
		data:   encode_frame('x'.bytes(), false).bytestr()
	})
	assert resp.status_code == 200
	assert (resp.header.get_custom('x-seen-timeout') or { '' }) == '150000000u'
	assert (resp.header.get_custom('x-seen-te') or { '' }) == 'trailers'
}
