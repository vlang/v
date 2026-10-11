// vrender_test.v - `render_request`, the in-process render path.
//
// The point of these tests is that a route can be rendered without a listener.
// veb's own suite has done this by hand since long before this helper existed
// (route_arguments_test.v, cors_before_request_test.v,
// middleware_response_lifetime_test.v all build a `RequestParams` and call the
// dispatcher directly); this file pins the single public way to do it, so an app
// does not have to know veb's internals to test a route or export a site.
//
// The last test is the one that would fail if `render_request` ever started a
// server: it renders, then asserts the context's connection is nil. A helper
// that quietly opened a port would still pass every assertion below, which is
// exactly why it gets one of its own.
module veb

import net.http

// VrenderApp is deliberately small. The point is the entry path, not the routes.
struct VrenderApp {
	veb.StaticHandler
}

struct VrenderContext {
	veb.Context
}

@['/']
fn (mut app VrenderApp) index(mut ctx VrenderContext) Result {
	return ctx.text('home')
}

@['/docs/getting-started']
fn (mut app VrenderApp) getting_started(mut ctx VrenderContext) Result {
	return ctx.text('getting started')
}

// vrender_request renders one URL through the same path a server request takes.
fn vrender_request(url string) &VrenderContext {
	mut app := VrenderApp{}
	return render_request[VrenderApp, VrenderContext](mut app, http.Request{
		method: .get
		url:    url
	})
}

fn test_render_request_answers_the_route_body() {
	assert vrender_request('/').res.body == 'home'
	assert vrender_request('/docs/getting-started').res.body == 'getting started'
}

fn test_render_request_reports_a_200_for_a_route_that_answered() {
	// Not merely "not an error": `res.status_code` is filled by the same
	// `send_response_to_client` the server path uses, so a route that answered
	// 200 in a browser answers 200 here.
	assert vrender_request('/').res.status_code == int(http.Status.ok)
}

fn test_render_request_of_an_unknown_path_is_a_404() {
	// The export case that matters: a static site writes its 404 page through
	// the same route table, and a renderer that returned an empty body instead
	// of a 404 would ship a blank error page.
	ctx := vrender_request('/no-such-page')
	assert ctx.res.status_code == int(http.Status.not_found)
	assert ctx.res.body != ''
}

fn test_render_request_does_not_open_a_connection() {
	// The load-bearing assertion of this file. `Context.conn` stays nil and
	// `takeover_mode` stays `.none`, so `send_response_to_client` has nothing to
	// write to - which is what makes this usable without a port, and what a
	// future change that "helpfully" started a listener would break.
	ctx := vrender_request('/')
	assert ctx.conn == unsafe { nil }
	assert ctx.takeover_mode == .none
	assert ctx.done
}

fn test_render_request_is_repeatable() {
	// The route table is generated per call, so the second render does not see
	// state left by the first. A static export renders every route in sequence
	// through one app, which is the shape that would leak if it did not.
	mut app := VrenderApp{}
	req := http.Request{
		method: .get
		url:    '/'
	}
	first := render_request[VrenderApp, VrenderContext](mut app, req)
	second := render_request[VrenderApp, VrenderContext](mut app, req)
	assert first.res.body == second.res.body
}
