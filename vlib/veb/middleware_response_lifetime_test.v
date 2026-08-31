module veb

import net.http

struct MiddlewareLifetimeContext {
	Context
}

@[heap]
struct MiddlewareLifetimeApp {
	Middleware[MiddlewareLifetimeContext]
}

fn (mut app MiddlewareLifetimeApp) lookup(mut ctx MiddlewareLifetimeContext) bool {
	mut rows := []string{cap: 128}
	for i in 0 .. 128 {
		rows << '${ctx.req.url}:${i}:allocated before rejection'
	}
	_ = rows.len
	return true
}

fn (mut app MiddlewareLifetimeApp) reject_posts(mut ctx MiddlewareLifetimeContext) bool {
	if ctx.req.method == .post {
		ctx.res.set_status(.forbidden)
		ctx.res.header.add(.set_cookie, 'first=1')
		ctx.res.header.add_custom('X-Between-Cookies', 'yes') or { panic(err) }
		ctx.res.header.add(.set_cookie, 'second=2')
		ctx.res.header.add_custom('X-Rejected-By', 'middleware') or { panic(err) }
		ctx.text('Forbidden')
		return false
	}
	return true
}

@['/'; get; post]
fn (mut app MiddlewareLifetimeApp) index(mut ctx MiddlewareLifetimeContext) Result {
	return ctx.text('ok')
}

fn test_middleware_terminated_response_survives_after_handler_returns() {
	mut app := &MiddlewareLifetimeApp{}
	app.use(handler: app.lookup)
	app.use(handler: app.reject_posts)
	routes := generate_routes[MiddlewareLifetimeApp, MiddlewareLifetimeContext](app) or {
		panic(err)
	}
	controllers_sorted := check_duplicate_routes_in_controllers[MiddlewareLifetimeApp](app, routes) or {
		panic(err)
	}
	params := RequestParams{
		global_app:         unsafe { voidptr(app) }
		controllers_sorted: controllers_sorted
		routes:             &routes
	}
	req := http.Request{
		method: .post
		url:    '/'
		data:   'x=1'
		header: http.new_custom_header_from_map({
			'Host':           '127.0.0.1'
			'Content-Length': '3'
		}) or { panic(err) }
	}
	completed_context := handle_request_and_route[MiddlewareLifetimeApp, MiddlewareLifetimeContext](mut app,
		req, 0, params)
	mut churn := []string{cap: 512}
	for i in 0 .. 512 {
		churn << 'after route ${i}'
	}
	_ = churn.len
	gc_collect()
	raw := completed_context.res.bytestr()
	assert raw.starts_with('HTTP/1.1 403 Forbidden\r\n'), raw
	assert raw.contains('X-Rejected-By: middleware\r\n'), raw
	assert raw.contains('X-Between-Cookies: yes\r\n'), raw
	assert raw.count('Set-Cookie: first=1\r\n') == 1, raw
	assert raw.count('Set-Cookie: second=2\r\n') == 1, raw
	assert raw.count('Set-Cookie:') == 2, raw
	assert raw.ends_with('\r\n\r\nForbidden'), raw
}
