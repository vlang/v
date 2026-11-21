// handle_raw_test.v - `handle_raw`, the raw-bytes in-process request path.
//
// `render_request` takes an already parsed `http.Request`, so it never
// exercises the wire parser. These tests pin the gap `handle_raw` closes: raw
// bytes go through the same buffered reader the server accepts connections
// with (`read_request_from_buffered_reader`) and then through the dispatcher,
// with no socket involved.
module veb

import net.http
import os

struct RawApp {
	veb.StaticHandler
	file_path string
}

struct RawContext {
	veb.Context
}

@['/']
fn (mut app RawApp) index(mut ctx RawContext) Result {
	return ctx.text('home')
}

@['/docs/getting-started']
fn (mut app RawApp) getting_started(mut ctx RawContext) Result {
	return ctx.text('getting started')
}

@['/echo'; post]
fn (mut app RawApp) echo(mut ctx RawContext) Result {
	return ctx.text(ctx.req.data)
}

fn raw_app_response(raw string) http.Response {
	mut app := RawApp{}
	return http.parse_response(handle_raw[RawApp, RawContext](mut app, raw.bytes()).bytestr()) or {
		panic(err)
	}
}

fn test_handle_raw_simple_get() {
	// A well-formed request line answers 200 with the route body, serialized
	// as a complete raw response the same way the server would write it.
	resp := raw_app_response('GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n')
	assert resp.status_code == int(http.Status.ok)
	assert resp.body == 'home'
	assert resp.header.get(.content_length) or { '' } == '4'
}

fn test_handle_raw_post_body_passes_through_the_parser() {
	// The body is framed by Content-Length inside the wire reader, not copied
	// from a pre-parsed request, so this fails if the parser misframes it.
	resp := raw_app_response('POST /echo HTTP/1.1\r\nHost: localhost\r\nContent-Length: 5\r\nConnection: close\r\n\r\nhello')
	assert resp.status_code == int(http.Status.ok)
	assert resp.body == 'hello'
}

fn test_handle_raw_pipelined_requests() {
	// Two requests in one byte stream are answered in order with no socket.
	// HTTP/1.1 defaults to keep-alive, so neither response closes the stream.
	mut app := RawApp{}
	raw := 'GET / HTTP/1.1\r\nHost: localhost\r\n\r\nGET /docs/getting-started HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'.bytes()
	out := handle_raw[RawApp, RawContext](mut app, raw).bytestr()
	assert out.count('HTTP/1.1 200') == 2
	assert out.contains('home')
	assert out.contains('getting started')
}

fn test_handle_raw_malformed_input_is_400_not_a_panic() {
	// Garbage that the wire parser rejects becomes a 400-class response,
	// never a panic, mirroring the server closing a bad connection.
	mut app := RawApp{}
	out := handle_raw[RawApp, RawContext](mut app, 'NOT A VALID REQUEST\r\n\r\n'.bytes()).bytestr()
	resp := http.parse_response(out) or { panic(err) }
	assert resp.status_code == int(http.Status.bad_request)
}

fn test_handle_raw_malformed_request_line_is_400_with_body() {
	// An empty request target or a doubled space before the HTTP version is a
	// client error: the server answers 400 with a framed body, never an empty
	// 500. See https://github.com/vlang/v/issues/32333.
	for raw in ['GET  HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n',
		'GET /  HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'] {
		resp := raw_app_response(raw)
		assert resp.status_code == int(http.Status.bad_request)
		assert resp.body == '400 Bad Request'
		assert resp.header.get(.content_length) or { '' } == resp.body.len.str()
	}
}

fn test_handle_raw_head_dispatches_without_a_panic() {
	// HEAD is accepted by the request-line parser and routed like any other
	// method; this route only allows GET, so the dispatcher answers 405 with
	// an `Allow` header instead of panicking on the unexpected method.
	resp := raw_app_response('HEAD / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n')
	assert resp.status_code == int(http.Status.method_not_allowed)
	assert resp.header.get_custom('Allow') or { '' }.contains('GET')
}

@['/file']
fn (mut app RawApp) file_response(mut ctx RawContext) Result {
	return ctx.file(app.file_path)
}

fn test_handle_raw_file_bytes_and_pipelined_framing() {
	path := os.join_path(os.temp_dir(), 'veb_handle_raw_binary_file.bin')
	data := [u8(0), 255, 13, 10, 65]
	os.write_file_array(path, data) or { panic(err) }
	defer { os.rm(path) or {} }
	mut app := RawApp{ file_path: path }
	out := handle_raw[RawApp, RawContext](mut app,
		'GET /file HTTP/1.1\r\nHost: localhost\r\n\r\nGET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'.bytes())
	header_end := out.bytestr().index('\r\n\r\n') or { panic('missing headers') } + 4
	assert out[header_end..header_end + data.len] == data
	first := http.parse_response(out[..header_end + data.len].bytestr()) or { panic(err) }
	assert first.status_code == 200
	assert first.header.get(.content_length) or { '' } == data.len.str()
	second := http.parse_response(out[header_end + data.len..].bytestr()) or { panic(err) }
	assert second.status_code == 200
	assert second.body == 'home'
}

fn test_handle_raw_missing_file_is_a_complete_response() {
	mut app := RawApp{ file_path: os.join_path(os.temp_dir(), 'veb_handle_raw_missing.dat') }
	resp := http.parse_response(handle_raw[RawApp, RawContext](mut app,
		'GET /file HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'.bytes()).bytestr()) or { panic(err) }
	assert resp.status_code == 404
	assert resp.header.get(.content_length) or { '' } == resp.body.len.str()
}

@['/deleted']
fn (mut app RawApp) deleted_file_response(mut ctx RawContext) Result {
	result := ctx.file(app.file_path)
	os.rm(app.file_path) or { panic(err) }
	return result
}

fn test_handle_raw_file_read_failure_closes_with_complete_500() {
	path := os.join_path(os.temp_dir(), 'veb_handle_raw_deleted_file.txt')
	os.write_file(path, 'original contents') or { panic(err) }
	defer { os.rm(path) or {} }
	mut app := RawApp{ file_path: path }
	out := handle_raw[RawApp, RawContext](mut app,
		'GET /deleted HTTP/1.1\r\nHost: localhost\r\n\r\nGET / HTTP/1.1\r\nHost: localhost\r\n\r\n'.bytes()).bytestr()
	resp := http.parse_response(out) or { panic(err) }
	assert resp.status_code == 500
	assert resp.header.get(.content_length) or { '' } == resp.body.len.str()
	assert resp.header.get(.connection) or { '' } == 'close'
	assert !out.contains('HTTP/1.1 200')
}

fn raw_sub_static_app() (RawApp, string) {
	dir := os.join_path(os.temp_dir(), 'veb_handle_raw_sub_static')
	os.mkdir_all(os.join_path(dir, 'sub')) or { panic(err) }
	os.write_file(os.join_path(dir, 'index.html'), '<h1>index</h1>') or { panic(err) }
	os.write_file(os.join_path(dir, 'sub', 'index.html'), '<h1>sub</h1>') or { panic(err) }
	mut app := RawApp{}
	app.handle_static(dir, true) or { panic(err) }
	return app, dir
}

fn test_handle_raw_static_dir_without_trailing_slash_serves_index() {
	mut app, dir := raw_sub_static_app()
	defer { os.rmdir_all(dir) or {} }
	resp := http.parse_response(handle_raw[RawApp, RawContext](mut app,
		'GET /sub HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'.bytes()).bytestr()) or {
		panic(err)
	}
	assert resp.status_code == int(http.Status.ok)
	assert resp.body == '<h1>sub</h1>'
}

fn test_handle_raw_static_dir_with_trailing_slash_serves_index() {
	mut app, dir := raw_sub_static_app()
	defer { os.rmdir_all(dir) or {} }
	resp := http.parse_response(handle_raw[RawApp, RawContext](mut app,
		'GET /sub/ HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'.bytes()).bytestr()) or {
		panic(err)
	}
	assert resp.status_code == int(http.Status.ok)
	assert resp.body == '<h1>sub</h1>'
}

fn raw_head_static_app() (RawApp, string) {
	dir := os.join_path(os.temp_dir(), 'veb_handle_raw_head_static')
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'index.html'), '<h1>index</h1>') or { panic(err) }
	mut app := RawApp{}
	app.handle_static(dir, true) or { panic(err) }
	return app, dir
}

fn test_handle_raw_static_head_has_headers_but_no_body() {
	// A HEAD response carries the GET-equivalent headers, including
	// Content-Length and validators, but no content (RFC 9110 9.3.2).
	mut app, dir := raw_head_static_app()
	defer { os.rmdir_all(dir) or {} }
	resp := http.parse_response(handle_raw[RawApp, RawContext](mut app,
		'HEAD /index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'.bytes()).bytestr()) or {
		panic(err)
	}
	assert resp.status_code == int(http.Status.ok)
	assert resp.body == ''
	assert resp.header.get(.content_length) or { '' } == '<h1>index</h1>'.len.str()
	assert resp.header.contains(.etag)
}

fn test_handle_raw_static_head_conditional_match_is_304() {
	// A matching validator on HEAD still answers 304 through send_file.
	mut app, dir := raw_head_static_app()
	defer { os.rmdir_all(dir) or {} }
	resp := http.parse_response(handle_raw[RawApp, RawContext](mut app,
		'HEAD /index.html HTTP/1.1\r\nHost: localhost\r\nIf-None-Match: *\r\nConnection: close\r\n\r\n'.bytes()).bytestr()) or {
		panic(err)
	}
	assert resp.status_code == int(http.Status.not_modified)
	assert resp.body == ''
}

fn raw_static_app() (RawApp, string) {
	dir := os.join_path(os.temp_dir(), 'veb_handle_raw_static')
	os.mkdir_all(dir) or { panic(err) }
	os.write_file(os.join_path(dir, 'index.html'), '<h1>index</h1>') or { panic(err) }
	mut app := RawApp{}
	app.handle_static(dir, true) or { panic(err) }
	return app, dir
}

fn test_handle_raw_static_post_is_405_with_allow() {
	mut app, dir := raw_static_app()
	defer { os.rmdir_all(dir) or {} }
	resp := http.parse_response(handle_raw[RawApp, RawContext](mut app,
		'POST /index.html HTTP/1.1\r\nHost: localhost\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()).bytestr()) or {
		panic(err)
	}
	assert resp.status_code == int(http.Status.method_not_allowed)
	allow := resp.header.get_custom('Allow') or { '' }
	assert allow.contains('GET')
	assert allow.contains('HEAD')
}

fn test_handle_raw_static_delete_is_405() {
	mut app, dir := raw_static_app()
	defer { os.rmdir_all(dir) or {} }
	resp := http.parse_response(handle_raw[RawApp, RawContext](mut app,
		'DELETE /index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'.bytes()).bytestr()) or {
		panic(err)
	}
	assert resp.status_code == int(http.Status.method_not_allowed)
}

fn test_handle_raw_static_get_still_serves_the_file() {
	mut app, dir := raw_static_app()
	defer { os.rmdir_all(dir) or {} }
	resp := http.parse_response(handle_raw[RawApp, RawContext](mut app,
		'GET /index.html HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'.bytes()).bytestr()) or {
		panic(err)
	}
	assert resp.status_code == int(http.Status.ok)
	assert resp.body == '<h1>index</h1>'
}

fn test_handle_raw_static_conditional_non_retrieval_is_405() {
	// RFC 9110 section 13.2.1 gives the method check precedence over
	// conditional evaluation, so conditional headers never smuggle a body
	// out through a non-retrieval method.
	mut app, dir := raw_static_app()
	defer { os.rmdir_all(dir) or {} }
	cases := [
		'POST /index.html with Range',
		'POST /index.html HTTP/1.1\r\nHost: localhost\r\nRange: bytes=0-2\r\nConnection: close\r\n\r\n',
		'POST /index.html with If-Modified-Since garbage',
		'POST /index.html HTTP/1.1\r\nHost: localhost\r\nIf-Modified-Since: garbage\r\nConnection: close\r\n\r\n',
		'POST /index.html with mismatching If-None-Match',
		'POST /index.html HTTP/1.1\r\nHost: localhost\r\nIf-None-Match: "not-the-etag"\r\nConnection: close\r\n\r\n',
		'POST /index.html with matching If-None-Match',
		'POST /index.html HTTP/1.1\r\nHost: localhost\r\nIf-None-Match: *\r\nConnection: close\r\n\r\n',
		'DELETE /index.html with Range',
		'DELETE /index.html HTTP/1.1\r\nHost: localhost\r\nRange: bytes=0-2\r\nConnection: close\r\n\r\n',
		'DELETE /index.html with If-Modified-Since garbage',
		'DELETE /index.html HTTP/1.1\r\nHost: localhost\r\nIf-Modified-Since: garbage\r\nConnection: close\r\n\r\n',
		'DELETE /index.html with mismatching If-None-Match',
		'DELETE /index.html HTTP/1.1\r\nHost: localhost\r\nIf-None-Match: "not-the-etag"\r\nConnection: close\r\n\r\n',
	]
	for i := 0; i < cases.len; i += 2 {
		resp := http.parse_response(handle_raw[RawApp, RawContext](mut app,
			cases[i + 1].bytes()).bytestr()) or { panic(err) }
		assert resp.status_code == int(http.Status.method_not_allowed), cases[i]
		assert resp.body != '<h1>index</h1>', cases[i]
	}
}
