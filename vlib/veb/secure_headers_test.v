// secure_headers_test.v - the secure_headers middleware sets safe defaults.
//
// Every browser-facing veb response should carry the standard hardening
// headers, and nothing in the framework set them: an app had to remember all
// five in every handler. The middleware sets them once for every response, a
// handler that sets its own value keeps it, and an empty option disables that
// header.
module veb

import net.http

struct SecureApp {
	Middleware[SecureContext]
}

struct SecureContext {
	Context
}

@['/page']
fn (app &SecureApp) page(mut ctx SecureContext) Result {
	return ctx.text('page')
}

// secure_ctx builds a Context whose response header the option methods can be
// driven against without a server.
fn secure_ctx() Context {
	return Context{
		req:   http.Request{
			method: .get
			url:    '/'
			header: http.new_header()
		}
		query: {}
		form:  {}
		files: {}
		res:   http.Response{
			header: http.new_header()
		}
	}
}

fn secure_header(header http.Header, name string) string {
	return header.get_custom(name) or { '' }
}

fn test_secure_headers_set_every_default() {
	mut ctx := secure_ctx()
	SecureHeadersOptions{}.set_headers(mut ctx)
	assert secure_header(ctx.res.header, 'Content-Security-Policy') == "default-src 'self'"
	assert secure_header(ctx.res.header, 'Strict-Transport-Security') == 'max-age=31536000; includeSubDomains'
	assert secure_header(ctx.res.header, 'X-Frame-Options') == 'SAMEORIGIN'
	assert secure_header(ctx.res.header, 'X-Content-Type-Options') == 'nosniff'
	assert secure_header(ctx.res.header, 'Referrer-Policy') == 'strict-origin-when-cross-origin'
}

fn test_secure_headers_honours_custom_values() {
	mut ctx := secure_ctx()
	SecureHeadersOptions{
		content_security_policy: "default-src 'self' https://cdn.example"
		x_frame_options:         'DENY'
	}.set_headers(mut ctx)
	assert secure_header(ctx.res.header, 'Content-Security-Policy') == "default-src 'self' https://cdn.example"
	assert secure_header(ctx.res.header, 'X-Frame-Options') == 'DENY'
}

fn test_secure_headers_empty_disables_only_that_header() {
	// An empty option must be a working setup, not a broken one: the disabled
	// header stays absent and every other default is still set.
	mut ctx := secure_ctx()
	SecureHeadersOptions{
		strict_transport_security: ''
	}.set_headers(mut ctx)
	assert secure_header(ctx.res.header, 'Strict-Transport-Security') == ''
	assert secure_header(ctx.res.header, 'X-Frame-Options') == 'SAMEORIGIN'
	assert secure_header(ctx.res.header, 'X-Content-Type-Options') == 'nosniff'
}

fn test_secure_headers_yields_to_a_value_the_handler_sets() {
	// In a request the middleware runs before the handler, so the handler's
	// value must survive: the default goes on first, then the handler's.
	mut ctx := secure_ctx()
	SecureHeadersOptions{}.set_headers(mut ctx)
	ctx.res.header.set_custom('Content-Security-Policy', "script-src 'none'") or {
		assert false, 'test setup failed: ${err}'
	}
	assert secure_header(ctx.res.header, 'Content-Security-Policy') == "script-src 'none'"
	// ...and the headers the handler did not touch keep their defaults.
	assert secure_header(ctx.res.header, 'X-Frame-Options') == 'SAMEORIGIN'
}

fn test_secure_headers_middleware_lets_the_request_continue() {
	// The middleware must not answer or stop the chain: the handler still runs
	// and its body reaches the client with the headers attached.
	mut app := SecureApp{}
	app.use(secure_headers[SecureContext]())
	ctx := render_request[SecureApp, SecureContext](mut app, http.Request{
		method: .get
		url:    '/page'
	}) or { panic(err) }
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'page'
	assert secure_header(ctx.res.header, 'X-Content-Type-Options') == 'nosniff'
}
