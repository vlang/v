module veb

import net.http

struct SharedDispatchContext {
	Context
}

@[heap]
struct SharedDispatchApp {
	Middleware[SharedDispatchContext]
}

@['/:path...'; get]
fn (mut app SharedDispatchApp) fallback(mut ctx SharedDispatchContext, path string) Result {
	return ctx.text('fallback:${path}')
}

@['/api/:path...'; get]
fn (mut app SharedDispatchApp) api_fallback(mut ctx SharedDispatchContext, path string) Result {
	return ctx.text('api:${path}')
}

@['/api/items/:id'; get; delete]
fn (mut app SharedDispatchApp) item(mut ctx SharedDispatchContext, id int) Result {
	return ctx.text('item:${id}')
}

@['/fixed'; get; host: 'example.test']
fn (mut app SharedDispatchApp) fixed(mut ctx SharedDispatchContext) Result {
	return ctx.text('fixed')
}

fn shared_dispatch_request(method http.Method, path string, host string) &Context {
	mut app := &SharedDispatchApp{}
	app.route_use('/fixed',
		handler: fn (mut ctx SharedDispatchContext) bool {
			if ctx.query['block'] == '1' {
				ctx.text('blocked-before')
				return false
			}
			ctx.req.header.add_custom('X-Before', 'yes') or { panic(err) }
			return true
		}
	)
	app.use(
		after:   true
		handler: fn (mut ctx SharedDispatchContext) bool {
			ctx.res.body += ':global-after'
			return true
		}
	)
	app.route_use('/fixed',
		after:   true
		handler: fn (mut ctx SharedDispatchContext) bool {
			assert ctx.req.header.get_custom('X-Before') or { '' } == 'yes'
			ctx.res.body += ':route-after'
			return true
		}
	)
	routes := generate_routes[SharedDispatchApp, SharedDispatchContext](app) or { panic(err) }
	return handle_request_and_route[SharedDispatchApp, SharedDispatchContext](mut app,
		http.Request{
			method: method
			url:    path
			header: http.new_header_from_map({
				http.CommonHeader.host: host
			})
		}, 0, RequestParams{ routes: &routes })
}

fn test_shared_dispatch_preserves_variadic_host_and_middleware_rules() {
	assert shared_dispatch_request(.get, '/other/path', 'example.test').res.body == 'fallback:/other/path:global-after'
	assert shared_dispatch_request(.get, '/api/other/path', 'example.test').res.body == 'api:other/path:global-after'
	assert shared_dispatch_request(.get, '/api/items/42', 'example.test').res.body == 'item:42:global-after'
	assert shared_dispatch_request(.delete, '/api/items/43', 'example.test').res.body == 'item:43:global-after'
	assert shared_dispatch_request(.get, '/fixed', 'example.test').res.body == 'fixed:global-after:route-after'
	assert shared_dispatch_request(.get, '/fixed', 'other.test').res.body == 'fallback:/fixed:global-after'
	assert shared_dispatch_request(.post, '/fixed', 'example.test').res.status_code == 404
	assert shared_dispatch_request(.get, '/fixed?block=1', 'example.test').res.body == 'blocked-before'
}
