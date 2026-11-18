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
