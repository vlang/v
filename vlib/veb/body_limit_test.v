// body_limit_test.v - oversized bodies are refused with 413 before the handler.
//
// The middleware exists so one route can accept megabytes while its neighbour
// accepts kilobytes. Both cases are asserted on the same app: the strict
// route refuses what the lenient route answers, which is the whole point of a
// per-route limit.
module veb

import net.http

struct LimitBodyApp {
	Middleware[LimitBodyContext]
}

struct LimitBodyContext {
	Context
}

@['/upload'; post]
fn (app &LimitBodyApp) upload(mut ctx LimitBodyContext) Result {
	return ctx.text('stored ${ctx.req.data.len} bytes')
}

@['/ping'; post]
fn (app &LimitBodyApp) ping(mut ctx LimitBodyContext) Result {
	return ctx.text('pong')
}

// limited_request posts one body through an app whose `/upload` route allows
// ten bytes.
fn limited_request(url string, body string) &Context {
	return request_with_limit(10, url, body)
}

// request_with_limit posts one body through an app whose `/upload` route allows
// `max_bytes` bytes.
fn request_with_limit(max_bytes int, url string, body string) &Context {
	mut app := LimitBodyApp{}
	app.route_use('/upload', body_limit[LimitBodyContext](max_bytes))
	return render_request[LimitBodyApp, LimitBodyContext](mut app, http.Request{
		method: .post
		url:    url
		data:   body
		header: http.new_header_from_map({
			http.CommonHeader.host:           'localhost'
			http.CommonHeader.content_type:   'text/plain'
			http.CommonHeader.content_length: '${body.len}'
		})
	}) or { panic(err) }
}

fn test_a_body_within_the_limit_reaches_the_handler() {
	ctx := limited_request('/upload', '12345')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'stored 5 bytes'
}

fn test_a_body_at_exactly_the_limit_reaches_the_handler() {
	ctx := limited_request('/upload', '1234567890')
	assert ctx.res.status_code == int(http.Status.ok)
}

fn test_a_body_past_the_limit_is_refused_with_413() {
	ctx := limited_request('/upload', '12345678901')
	assert ctx.res.status_code == int(http.Status.request_entity_too_large)
	assert ctx.res.body.contains('exceeds the limit of 10 bytes')
}

fn test_a_route_without_the_limit_is_unaffected() {
	// The same oversized body on an unprotected route answers normally: the
	// limit is per route, not global.
	ctx := limited_request('/ping', '12345678901')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'pong'
}

fn test_a_limit_of_zero_refuses_any_non_empty_body() {
	empty := request_with_limit(0, '/upload', '')
	assert empty.res.status_code == int(http.Status.ok)
	assert empty.res.body == 'stored 0 bytes'
	one_byte := request_with_limit(0, '/upload', '1')
	assert one_byte.res.status_code == int(http.Status.request_entity_too_large)
	assert one_byte.res.body == 'request body of 1 bytes exceeds the limit of 0 bytes'
}
