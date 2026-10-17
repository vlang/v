// method_not_allowed_test.v - a wrong verb is a 405 with `Allow`, not a 404.
//
// select_route skips a route whose method does not match and falls through to
// not_found(), so before this change every method mismatch looked like a path
// that does not exist. That loses the one header a client can act on: `Allow`
// tells a caller which methods the route accepts, which is the difference
// between "you have the wrong URL" and "you have the wrong verb".
module veb

import net.http

struct MethodApp {}

struct MethodContext {
	Context
}

@['/only-get']
fn (app &MethodApp) only_get(mut ctx MethodContext) Result {
	return ctx.text('got it')
}

@['/two-verbs'; get; delete]
fn (app &MethodApp) two_verbs(mut ctx MethodContext) Result {
	return ctx.text('either is fine')
}

@['/param/:id'; put]
fn (app &MethodApp) param_put(mut ctx MethodContext, id int) Result {
	return ctx.text('put ${id}')
}

// method_request renders one method and URL through the dispatcher.
fn method_request(method http.Method, url string) &Context {
	mut app := MethodApp{}
	return render_request[MethodApp, MethodContext](mut app, http.Request{
		method: method
		url:    url
	}) or { panic(err) }
}

fn test_a_matching_method_still_answers_200() {
	ctx := method_request(.get, '/only-get')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'got it'
}

fn test_a_wrong_method_answers_405() {
	// The behaviour change itself: this was `404 Not Found`.
	ctx := method_request(.post, '/only-get')
	assert ctx.res.status_code == int(http.Status.method_not_allowed)
}

fn test_a_405_carries_an_allow_header_naming_the_accepted_methods() {
	ctx := method_request(.post, '/only-get')
	assert ctx.res.header.get_custom('Allow') or { '' } == 'GET'
}

fn test_405_lists_every_method_a_multi_verb_route_accepts() {
	ctx := method_request(.patch, '/two-verbs')
	assert ctx.res.status_code == int(http.Status.method_not_allowed)
	assert ctx.res.header.get_custom('Allow') or { '' } == 'GET,DELETE'
}

fn test_a_parameterised_route_also_reports_405() {
	// A parameterised route must not be mistaken for an unknown path; the client
	// asked for /param/1 with the wrong verb, not for a path that does not exist.
	ctx := method_request(.post, '/param/1')
	assert ctx.res.status_code == int(http.Status.method_not_allowed)
	assert ctx.res.header.get_custom('Allow') or { '' } == 'PUT'
}

fn test_an_unknown_path_is_still_a_404() {
	// The companion assertion: fixing 405 must not swallow 404. An unknown path
	// must not answer 405, because no route claimed it.
	ctx := method_request(.get, '/no-such-route')
	assert ctx.res.status_code == int(http.Status.not_found)
}
