module veb

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
