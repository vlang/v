module veb

import compress.gzip
import net.http
import os

struct StaticMiddlewareContext {
	Context
}

@[heap]
struct StaticMiddlewareApp {
	StaticHandler
	Middleware[StaticMiddlewareContext]
}

@['/hello'; get]
fn (app &StaticMiddlewareApp) hello(mut ctx StaticMiddlewareContext) Result {
	return ctx.text('hello')
}

fn static_middleware_before(mut ctx StaticMiddlewareContext) bool {
	if ctx.query['block'] == '1' {
		ctx.text('blocked')
		return false
	}
	ctx.res.header.add_custom('X-Before', '1') or { panic(err) }
	return true
}

fn static_middleware_after(mut ctx StaticMiddlewareContext) bool {
	ctx.res.header.add_custom('X-After', '1') or { panic(err) }
	return true
}

fn static_middleware_handle(path string) &Context {
	mut app := &StaticMiddlewareApp{}
	app.handle_static(os.join_path(os.dir(@FILE), 'tests', 'testdata', 'sub_folder'), true) or {
		panic(err)
	}
	app.use(handler: static_middleware_before)
	app.use(handler: static_middleware_after, after: true)
	routes := generate_routes[StaticMiddlewareApp, StaticMiddlewareContext](app) or { panic(err) }
	return handle_request_and_route[StaticMiddlewareApp, StaticMiddlewareContext](mut app,
		http.Request{
			method: .get
			url:    path
			header: http.new_header_from_map({
				http.CommonHeader.host: 'localhost'
			})
		}, 0, RequestParams{ routes: &routes })
}

fn test_static_middleware_runs_for_routes() {
	completed := static_middleware_handle('/hello')
	assert completed.res.body == 'hello'
	assert completed.res.header.get_custom('X-Before') or { '' } == '1'
	assert completed.res.header.get_custom('X-After') or { '' } == '1'
}

fn test_static_middleware_runs_for_static_files() {
	completed := static_middleware_handle('/sub.txt')
	assert completed.return_type == .file
	assert completed.res.header.get_custom('X-Before') or { '' } == '1'
	assert completed.res.header.get_custom('X-After') or { '' } == '1'
}

fn test_static_middleware_refusal_blocks_static_files() {
	completed := static_middleware_handle('/sub.txt?block=1')
	assert completed.return_type != .file
	assert completed.res.body == 'blocked'
	assert completed.res.header.get_custom('X-After') or { '' } == ''
}

struct ReplaceStaticContext {
	Context
}

@[heap]
struct ReplaceStaticApp {
	StaticHandler
	Middleware[ReplaceStaticContext]
}

// replace_static_after sends an alternate response for every request,
// including static files: the pending file transfer must be cancelled.
fn replace_static_after(mut ctx ReplaceStaticContext) bool {
	ctx.res.set_status(.forbidden)
	ctx.text('blocked')
	return false
}

fn test_static_middleware_after_replacement_emits_only_the_replacement() {
	// `index.htm` serves as text/html so a stale file mime would show: the
	// replacement must be text/plain with its own length, and the raw bytes
	// must not contain the file representation at all.
	mut app := &ReplaceStaticApp{}
	app.handle_static(os.join_path(os.dir(@FILE), 'tests', 'testdata', 'sub_folder'), true) or {
		panic(err)
	}
	app.use(handler: replace_static_after, after: true)
	out := handle_raw[ReplaceStaticApp, ReplaceStaticContext](mut app,
		'GET /index.htm HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n'.bytes()).bytestr()
	resp := http.parse_response(out) or { panic(err) }
	assert resp.status_code == int(http.Status.forbidden)
	assert resp.body == 'blocked'
	assert resp.header.get(.content_length) or { '' } == 'blocked'.len.str()
	assert resp.header.get(.content_type) or { '' } == 'text/plain'
	assert !out.contains('htm')
}

struct FileReplaceContext {
	Context
}

@[heap]
struct FileReplaceApp {
	StaticHandler
	Middleware[FileReplaceContext]
}

@['/hello'; get]
fn (app &FileReplaceApp) hello(mut ctx FileReplaceContext) Result {
	return ctx.text('hello')
}

// file_replace_after replaces whatever the handler sent (static file or
// dynamic text) with a different static file. The replacement must carry
// its own representation headers: the live server emits headers via
// `write_to` and then streams `return_file` separately, so a stale
// Content-Length would frame the wrong body on the wire.
fn file_replace_after(mut ctx FileReplaceContext) bool {
	ctx.file(os.join_path(os.dir(@FILE), 'tests', 'testdata', 'root.txt'))
	return false
}

fn file_replace_handle(path string, extra_headers map[http.CommonHeader]string) &Context {
	mut app := &FileReplaceApp{}
	app.handle_static(os.join_path(os.dir(@FILE), 'tests', 'testdata', 'sub_folder'), true) or {
		panic(err)
	}
	app.use(handler: file_replace_after, after: true)
	routes := generate_routes[FileReplaceApp, FileReplaceContext](app) or { panic(err) }
	mut header := http.new_header_from_map({
		http.CommonHeader.host: 'localhost'
	})
	for key, value in extra_headers {
		header.set(key, value)
	}
	return handle_request_and_route[FileReplaceApp, FileReplaceContext](mut app,
		http.Request{
			method: .get
			url:    path
			header: header
		}, 0, RequestParams{ routes: &routes })
}

// live_wire_response reassembles the bytes the live fasthttp server emits:
// the serialized headers plus the streamed file. Unlike `handle_raw` (which
// rereads the file and recomputes Content-Length), this preserves a wrong
// length so the regression actually fails on the framing bug.
fn live_wire_response(completed &Context) (string, http.Response) {
	mut head := []u8{}
	completed.res.write_to(mut head)
	head_str := head.bytestr()
	body := if completed.return_type == .file {
		os.read_file(completed.return_file) or { panic(err) }
	} else {
		completed.res.body
	}
	wire := head_str + body
	return wire, http.parse_response(wire) or { panic(err) }
}

fn test_static_middleware_file_to_file_keeps_replacement_headers() {
	// `/sub.txt` is the 3-byte static file `sub`; the after handler
	// replaces it with the 4-byte `root.txt`.
	completed := file_replace_handle('/sub.txt', {})
	want_file := os.join_path(os.dir(@FILE), 'tests', 'testdata', 'root.txt')
	assert completed.return_type == .file
	assert completed.return_file == want_file
	assert completed.res.body == ''
	assert completed.res.header.get(.content_length) or { '' } == '4'
	assert completed.res.header.get(.content_type) or { '' } == 'text/plain'
	assert (completed.res.header.get(.etag) or { '' }) != ''
	wire, resp := live_wire_response(completed)
	assert resp.status_code == int(http.Status.ok)
	assert resp.body == 'root'
	assert resp.header.get(.content_length) or { '' } == '4'
	assert !wire.contains('sub') || wire.contains('root')
	assert wire.ends_with('root')
}

fn test_static_middleware_text_to_file_keeps_replacement_headers() {
	// `/hello` answers the 5-byte text `hello`; the after handler replaces
	// it with the 4-byte `root.txt` file.
	completed := file_replace_handle('/hello', {})
	want_file := os.join_path(os.dir(@FILE), 'tests', 'testdata', 'root.txt')
	assert completed.return_type == .file
	assert completed.return_file == want_file
	assert completed.res.body == ''
	assert completed.res.header.get(.content_length) or { '' } == '4'
	wire, resp := live_wire_response(completed)
	assert resp.status_code == int(http.Status.ok)
	assert resp.body == 'root'
	assert resp.header.get(.content_length) or { '' } == '4'
	assert !wire.contains('hello')
}

fn test_static_middleware_file_to_compressed_file_keeps_encoding_and_length() {
	// A precompressed `.gz` sidecar replacement must keep its
	// Content-Encoding and the compressed length, not a zero length with
	// the encoding stripped.
	dir := os.join_path(os.temp_dir(), 'veb_file_replace_gzip')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	orig := os.join_path(dir, 'orig.txt')
	repl := os.join_path(dir, 'repl.txt')
	os.write_file(orig, 'sub') or { panic(err) }
	repl_contents := 'replacement contents'
	os.write_file(repl, repl_contents) or { panic(err) }
	compressed := gzip.compress(repl_contents.bytes()) or { panic(err) }
	os.write_file(repl + '.gz', compressed.bytestr()) or { panic(err) }

	mut req := http.Request{
		method: .get
		url:    '/orig.txt'
		header: http.new_header_from_map({
			http.CommonHeader.host:            'localhost'
			http.CommonHeader.accept_encoding: 'gzip'
		})
	}
	mut ctx := &Context{
		req:                         req
		enable_static_gzip:          true
		static_compression_max_size: 1048576
	}
	ctx.send_file('text/plain', orig)
	assert ctx.return_type == .file
	ctx.done = false
	ctx.send_file('text/plain', repl)
	assert ctx.return_type == .file
	assert ctx.return_file == repl + '.gz'
	assert ctx.res.header.get(.content_encoding) or { '' } == 'gzip'
	assert ctx.res.header.get(.vary) or { '' } == 'Accept-Encoding'
	gz_len := os.file_size(repl + '.gz')
	assert (ctx.res.header.get(.content_length) or { '' }) == gz_len.str()
	assert ctx.res.header.get(.content_length) or { '' } != '0'
	// The live wire is headers plus the streamed gzip bytes: Content-Length
	// must match those bytes so the client does not truncate or over-read.
	mut head := []u8{}
	ctx.res.write_to(mut head)
	head_str := head.bytestr()
	assert head_str.contains('Content-Encoding: gzip')
	assert head_str.contains('Content-Length: ${gz_len}')
	raw_gz := os.read_bytes(repl + '.gz') or { panic(err) }
	mut wire := head.clone()
	wire << raw_gz
	assert wire.len == head.len + raw_gz.len
}
