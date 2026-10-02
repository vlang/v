module grpc

import net.http

// grpc_hdr builds a response header set from plain string pairs, standing in
// for what the HTTP/2 layer hands back with trailers folded into the headers.
fn grpc_hdr(pairs map[string]string) http.Header {
	mut h := http.new_header()
	for k, v in pairs {
		h.add_custom(k, v) or { panic(err) }
	}
	return h
}

const grpc_ok_hdr = {
	'content-type': 'application/grpc+proto'
	'grpc-status':  '0'
}

// expect_status asserts that parse_unary_response rejects this response with a
// StatusError of the expected code, whose message contains msg_part.
fn expect_status(status_code int, h http.Header, body []u8, want Code, msg_part string) {
	if _ := parse_unary_response(status_code, h, body) {
		assert false, 'expected StatusError ${want}'
	} else {
		if err is StatusError {
			assert err.status.code == want, 'got ${err.status.code}: ${err.status.message}'
			assert err.status.message.contains(msg_part), err.status.message
		} else {
			assert false, 'not a StatusError: ${err.msg()}'
		}
	}
}

fn test_client_ok_response() ! {
	payload := 'response bytes'.bytes()
	body := encode_frame(payload, false)
	got := parse_unary_response(200, grpc_hdr(grpc_ok_hdr), body)!
	assert got == payload
}

fn test_client_error_status_with_percent_message() {
	h := grpc_hdr({
		'content-type': 'application/grpc+proto'
		'grpc-status':  '3'
		'grpc-message': 'bad%20arg%3A%20id'
	})
	expect_status(200, h, [], .invalid_argument, 'bad arg: id')
}

fn test_client_missing_grpc_status() {
	h := grpc_hdr({
		'content-type': 'application/grpc+proto'
	})
	expect_status(200, h, [], .unknown, 'missing grpc-status')
}

// a non-numeric grpc-status must not read as 0 (which would mean OK)
fn test_client_malformed_grpc_status() {
	h := grpc_hdr({
		'content-type': 'application/grpc+proto'
		'grpc-status':  'abc'
	})
	expect_status(200, h, [], .unknown, 'malformed')
}

fn test_client_out_of_range_grpc_status() {
	h := grpc_hdr({
		'content-type': 'application/grpc+proto'
		'grpc-status':  '99'
	})
	expect_status(200, h, [], .unknown, 'invalid status code')
}

fn test_client_http_error_mapping() {
	expect_status(404, grpc_hdr({}), [], .unimplemented, 'HTTP 404')
	expect_status(503, grpc_hdr({}), [], .unavailable, 'HTTP 503')
	expect_status(418, grpc_hdr({}), [], .unknown, 'HTTP 418')
}

fn test_client_explicit_grpc_status_overrides_http_mapping() {
	h := grpc_hdr({
		'content-type': 'application/grpc+proto'
		'grpc-status':  '7'
		'grpc-message': 'permission%20denied'
	})
	for http_status in [401, 404, 503] {
		expect_status(http_status, h, [], .permission_denied, 'permission denied')
	}
	assert parse_unary_response(503, grpc_hdr(grpc_ok_hdr), encode_frame('ok'.bytes(), false))! == 'ok'.bytes()
}

fn test_client_malformed_grpc_status_does_not_use_http_mapping() {
	for grpc_status in ['', 'abc', '99'] {
		h := grpc_hdr({
			'content-type': 'application/grpc+proto'
			'grpc-status':  grpc_status
		})
		expect_status(503, h, [], .unknown, '')
	}
}

fn test_client_wrong_content_type() {
	h := grpc_hdr({
		'content-type': 'text/html'
		'grpc-status':  '0'
	})
	expect_status(200, h, [], .unknown, 'content-type')
}

fn test_client_multiple_frames_rejected() {
	mut body := encode_frame('a'.bytes(), false)
	body << encode_frame('b'.bytes(), false)
	expect_status(200, grpc_hdr(grpc_ok_hdr), body, .internal, 'expected 1')
}

fn test_client_compressed_frame_rejected() {
	body := encode_frame('a'.bytes(), true)
	expect_status(200, grpc_hdr(grpc_ok_hdr), body, .unimplemented, 'compressed')
}

fn test_client_percent_decodes_grpc_message() {
	assert percent_decode('plain') == 'plain'
	assert percent_decode('a%20b%3a%3b') == 'a b:;'
	assert percent_decode('bad%zz%2') == 'bad%zz%2'
	assert percent_decode('%F0%9F%9A%80') == '🚀'
}
