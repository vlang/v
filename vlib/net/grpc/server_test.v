module grpc

import net.http

// FakeService is a hand-rolled Connect Service: it echoes the body back for one
// path, mirrors request metadata for another, and fails in the various shapes a
// handler can fail in.
struct FakeService {
mut:
	last_codec Codec
}

fn (mut s FakeService) call(path string, codec Codec, body []u8, mut ctx ServerContext) !([]u8, bool) {
	s.last_codec = codec
	match path {
		'/t.Echo/Do' {
			return body, true
		}
		'/t.Echo/Meta' {
			// echo a request header into a response header + trailer, so
			// tests can exercise the ServerContext round-trip without a socket
			ctx.set_header('x-out', ctx.header('x-in'))
			ctx.set_trailer('x-tr', ctx.header('x-in'))
			return body, true
		}
		'/t.Echo/Detail' {
			return StatusError{
				status: Status{
					code:    .failed_precondition
					message: 'needs detail'
					details: [
						ErrorDetail{
							type_name: 'test.Detail'
							value:     [u8(1), 2, 3]
						},
					]
				}
			}
		}
		'/t.Echo/Boom' {
			return StatusError{
				status: Status{
					code:    .not_found
					message: 'nope'
				}
			}
		}
		'/t.Echo/Stop' {
			return StatusError{
				status: Status{
					code: .cancelled
				}
			}
		}
		'/t.Echo/Fail' {
			// a plain (non-StatusError) failure, e.g. a body that failed to decode
			return error('handler blew up')
		}
		else {
			return []u8{}, false
		}
	}
}

fn connect_post(mut s ConnectServer, path string, ct string, body string) http.Response {
	mut h := http.new_header()
	if ct != '' {
		h.add(.content_type, ct)
	}
	return s.handle(http.Request{
		method: .post
		url:    path
		data:   body
		header: h
	})
}

fn new_connect_test_server() ConnectServer {
	mut s := ConnectServer{}
	s.mount(FakeService{})
	return s
}

fn test_connect_proto_roundtrip() {
	mut s := new_connect_test_server()
	resp := connect_post(mut s, '/t.Echo/Do', 'application/proto', 'payload')
	assert resp.status_code == 200
	assert resp.body == 'payload'
	assert resp.header.get(.content_type) or { '' } == 'application/proto'
}

fn test_connect_json_codec_selected() {
	mut s := new_connect_test_server()
	resp := connect_post(mut s, '/t.Echo/Do', 'application/json; charset=utf-8', '{"a":1}')
	assert resp.status_code == 200
	assert resp.header.get(.content_type) or { '' } == 'application/json'
}

fn test_connect_status_error_mapping() {
	mut s := new_connect_test_server()
	resp := connect_post(mut s, '/t.Echo/Boom', 'application/proto', '')
	assert resp.status_code == 404
	assert resp.body.contains('"code":"not_found"')
	assert resp.body.contains('"message":"nope"')
	assert resp.header.get(.content_type) or { '' } == 'application/json'
}

fn test_connect_canceled_spelling_and_499() {
	mut s := new_connect_test_server()
	resp := connect_post(mut s, '/t.Echo/Stop', 'application/proto', '')
	assert resp.status_code == 499
	assert resp.body.contains('"code":"canceled"')
}

fn test_connect_unknown_procedure() {
	mut s := new_connect_test_server()
	resp := connect_post(mut s, '/t.Other/Nope', 'application/proto', '')
	// Connect maps a routing miss to HTTP 404 with the unimplemented code
	assert resp.status_code == 404
	assert resp.body.contains('unimplemented')
}

fn test_connect_non_post_rejected() {
	mut s := new_connect_test_server()
	resp := s.handle(http.Request{
		method: .get
		url:    '/t.Echo/Do'
	})
	assert resp.status_code == 405
}

fn test_connect_unsupported_codec() {
	mut s := new_connect_test_server()
	resp := connect_post(mut s, '/t.Echo/Do', 'text/xml', '<x/>')
	assert resp.status_code == 415
}

fn test_connect_compression_refused() {
	mut s := new_connect_test_server()
	mut h := http.new_header()
	h.add(.content_type, 'application/proto')
	h.add(.content_encoding, 'gzip')
	resp := s.handle(http.Request{
		method: .post
		url:    '/t.Echo/Do'
		data:   'x'
		header: h
	})
	assert resp.status_code == 501
	assert resp.body.contains('compression')
}

fn test_connect_non_status_error_becomes_internal() {
	mut s := new_connect_test_server()
	resp := connect_post(mut s, '/t.Echo/Fail', 'application/proto', '')
	assert resp.status_code == 500
	assert resp.body.contains('"code":"internal"')
	assert resp.body.contains('handler blew up')
}

fn test_connect_missing_content_type_rejected() {
	mut s := new_connect_test_server()
	resp := connect_post(mut s, '/t.Echo/Do', '', 'x')
	assert resp.status_code == 415
}

fn test_connect_identity_encoding_allowed() {
	mut s := new_connect_test_server()
	mut h := http.new_header()
	h.add(.content_type, 'application/proto')
	h.add(.content_encoding, 'identity')
	resp := s.handle(http.Request{
		method: .post
		url:    '/t.Echo/Do'
		data:   'ok'
		header: h
	})
	assert resp.status_code == 200
	assert resp.body == 'ok'
}

fn test_connect_query_string_stripped_from_path() {
	mut s := new_connect_test_server()
	resp := connect_post(mut s, '/t.Echo/Do?trace=1', 'application/proto', 'q')
	assert resp.status_code == 200
	assert resp.body == 'q'
}

// test_connect_error_code_table pins the full Connect error-code table
// (connectrpc.com/docs/protocol#error-codes) to both the HTTP status and the
// wire code name, for every gRPC code — the interop only exercises one, so
// these guard the other 16 without a socket.
fn test_connect_error_code_table() {
	cases := [
		Code.cancelled,
		.unknown,
		.invalid_argument,
		.deadline_exceeded,
		.not_found,
		.already_exists,
		.permission_denied,
		.resource_exhausted,
		.failed_precondition,
		.aborted,
		.out_of_range,
		.unimplemented,
		.internal,
		.unavailable,
		.data_loss,
		.unauthenticated,
	]
	want_status := {
		Code.cancelled:           499
		Code.unknown:             500
		Code.invalid_argument:    400
		Code.deadline_exceeded:   504
		Code.not_found:           404
		Code.already_exists:      409
		Code.permission_denied:   403
		Code.resource_exhausted:  429
		Code.failed_precondition: 400
		Code.aborted:             409
		Code.out_of_range:        400
		Code.unimplemented:       501
		Code.internal:            500
		Code.unavailable:         503
		Code.data_loss:           500
		Code.unauthenticated:     401
	}
	for c in cases {
		resp := connect_error_response(c, 'x', ServerContext{})
		assert resp.status_code == want_status[c], '${c}: got HTTP ${resp.status_code}'
		// cancelled is the one code whose wire name diverges from the enum
		// (US spelling)
		name := if c == .cancelled { 'canceled' } else { c.str() }
		assert resp.body.contains('"code":"${name}"'), '${c}: body ${resp.body}'
	}
}

fn test_connect_response_metadata_roundtrip() {
	mut s := new_connect_test_server()
	mut h := http.new_header()
	h.add(.content_type, 'application/proto')
	h.add_custom('x-in', 'hello') or {}
	resp := s.handle(http.Request{
		method: .post
		url:    '/t.Echo/Meta'
		data:   'body'
		header: h
	})
	assert resp.status_code == 200
	// leading metadata is a plain response header
	assert resp.header.get_custom('x-out', exact: true) or { '' } == 'hello'
	// trailing metadata rides as a Trailer--prefixed response header
	assert resp.header.get_custom('trailer-x-tr', exact: true) or { '' } == 'hello'
}

fn test_connect_request_metadata_preserves_mixed_case_value_order() {
	req := http.parse_request_str('POST /t.Echo/Meta HTTP/1.1\r\nContent-Type: application/proto\r\nX-In: first\r\nx-in: second\r\nX-In: third\r\nX-IN: fourth\r\nContent-Length: 4\r\n\r\nbody')!
	metadata := request_metadata(req.header)
	assert metadata['x-in'] == ['first', 'second', 'third', 'fourth']
	assert 'X-In' !in metadata
	assert 'X-IN' !in metadata
	mut s := new_connect_test_server()
	resp := s.handle(req)
	assert resp.status_code == 200
	assert (resp.header.get_custom('x-out') or { '' }) == 'first'
}

fn test_connect_error_details_serialized() {
	mut s := new_connect_test_server()
	resp := connect_post(mut s, '/t.Echo/Detail', 'application/proto', '')
	assert resp.status_code == 400
	assert resp.body.contains('"code":"failed_precondition"')
	assert resp.body.contains('"type":"test.Detail"')
	// base64 of [1, 2, 3] is AQID
	assert resp.body.contains('"value":"AQID"')
}
