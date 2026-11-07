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

fn (app &MethodApp) index(mut ctx MethodContext) Result {
	return ctx.text('index')
}

@['/'; post]
fn (app &MethodApp) root_post(mut ctx MethodContext) Result {
	return ctx.text('root post')
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

@['/param/:item'; post]
fn (app &MethodApp) param_post(mut ctx MethodContext, item int) Result {
	return ctx.text('post ${item}')
}

@['/same'; get; delete]
fn (app &MethodApp) same_get(mut ctx MethodContext) Result {
	return ctx.text('same get')
}

@['/same'; post; delete]
fn (app &MethodApp) same_post(mut ctx MethodContext) Result {
	return ctx.text('same post')
}

@['/host-only'; get; host: 'example.test']
fn (app &MethodApp) host_get(mut ctx MethodContext) Result {
	return ctx.text('host get')
}

@['/host-only'; post; host: 'example.test']
fn (app &MethodApp) host_post(mut ctx MethodContext) Result {
	return ctx.text('host post')
}

@['/host-only'; put; host: 'other.test']
fn (app &MethodApp) other_host_put(mut ctx MethodContext) Result {
	return ctx.text('other host put')
}

// method_request renders one method and URL through the dispatcher.
fn method_request(method http.Method, url string) &Context {
	return method_host_request(method, url, 'localhost')
}

fn method_host_request(method http.Method, url string, host string) &Context {
	mut app := MethodApp{}
	return render_request[MethodApp, MethodContext](mut app, http.Request{
		method: method
		url:    url
		header: http.new_header_from_map({
			.host: host
		})
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
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'post 1'
	wrong_method := method_request(.patch, '/param/1')
	assert wrong_method.res.status_code == int(http.Status.method_not_allowed)
	assert wrong_method.res.header.get_custom('Allow') or { '' } == 'PUT,POST'
}

fn test_405_combines_separate_handlers_and_deduplicates_methods() {
	ctx := method_request(.put, '/same')
	assert ctx.res.status_code == int(http.Status.method_not_allowed)
	assert ctx.res.header.get_custom('Allow') or { '' } == 'GET,DELETE,POST'
	assert method_request(.get, '/same').res.body == 'same get'
	assert method_request(.post, '/same').res.body == 'same post'
}

fn test_405_combines_root_handlers_and_the_index_alias() {
	root := method_request(.put, '/')
	assert root.res.status_code == int(http.Status.method_not_allowed)
	assert root.res.header.get_custom('Allow') or { '' } == 'GET,POST'
	ctx := method_request(.put, '/index')
	assert ctx.res.status_code == int(http.Status.method_not_allowed)
	assert ctx.res.header.get_custom('Allow') or { '' } == 'GET'
	// Empty paths can reach the selector, but are not valid HTTP request URIs.
	app := MethodApp{}
	routes := generate_routes[MethodApp, MethodContext](&app) or { panic(err) }
	selected := select_route_ignoring_method(&routes, '', 'localhost') or { panic('missing route') }
	assert allowed_methods_for_route(&routes, selected, '', 'localhost') == [
		http.Method.get,
		.post,
	]
}

fn test_405_combines_only_handlers_for_the_request_host() {
	ctx := method_host_request(.delete, '/host-only', 'example.test')
	assert ctx.res.status_code == int(http.Status.method_not_allowed)
	assert ctx.res.header.get_custom('Allow') or { '' } == 'GET,POST'
	other := method_host_request(.delete, '/host-only', 'other.test')
	assert other.res.status_code == int(http.Status.method_not_allowed)
	assert other.res.header.get_custom('Allow') or { '' } == 'PUT'
	unknown := method_host_request(.delete, '/host-only', 'unknown.test')
	assert unknown.res.status_code == int(http.Status.not_found)
	assert unknown.res.header.get_custom('Allow') or { '' } == ''
}

@[unsafe]
fn test_405_keeps_first_matching_resource_and_host_fallbacks() {
	mut routes := map[string]Route{}
	insert_route_metadata(mut routes, 'parameter', ['/items/:id', 'get', 'host: example.test'], [], [], unsafe { nil }) or { panic(err) }
	insert_route_metadata(mut routes, 'literal', ['/items/fixed', 'put'], [], [], unsafe { nil }) or { panic(err) }
	insert_route_metadata(mut routes, 'other_parameter', ['/items/:name', 'post', 'host: example.test'], [], [], unsafe { nil }) or { panic(err) }
	insert_route_metadata(mut routes, 'fallback', ['/items/:value', 'delete'], [], [], unsafe { nil }) or { panic(err) }
	selected := select_route_ignoring_method(&routes, '/items/fixed', 'example.test') or { panic('missing route') }
	assert selected.name == 'parameter'
	assert allowed_methods_for_route(&routes, selected, '/items/fixed', 'example.test') == [
		http.Method.get,
		.post,
		.delete,
	]
	other := select_route_ignoring_method(&routes, '/items/fixed', 'other.test') or { panic('missing route') }
	assert other.name == 'literal'
	assert allowed_methods_for_route(&routes, other, '/items/fixed', 'other.test') == [http.Method.put]
}

@[unsafe]
fn test_405_keeps_variadic_specificity_and_non_variadic_precedence() {
	mut routes := map[string]Route{}
	insert_route_metadata(mut routes, 'catch_all', ['/:path...', 'delete'], [], [], unsafe { nil }) or { panic(err) }
	insert_route_metadata(mut routes, 'api', ['/api/:path...', 'get'], [], [], unsafe { nil }) or { panic(err) }
	insert_route_metadata(mut routes, 'api_post', ['/api/:other...', 'post'], [], [], unsafe { nil }) or { panic(err) }
	insert_route_metadata(mut routes, 'item', ['/api/items/:id', 'patch'], [], [], unsafe { nil }) or { panic(err) }
	selected := select_route_ignoring_method(&routes, '/api/other/path', '') or { panic('missing route') }
	assert selected.name == 'api'
	assert allowed_methods_for_route(&routes, selected, '/api/other/path', '') == [
		http.Method.get,
		.post,
	]
	item := select_route_ignoring_method(&routes, '/api/items/1', '') or { panic('missing route') }
	assert item.name == 'item'
	assert allowed_methods_for_route(&routes, item, '/api/items/1', '') == [http.Method.patch]
}

fn test_an_unknown_path_is_still_a_404() {
	// The companion assertion: fixing 405 must not swallow 404. An unknown path
	// must not answer 405, because no route claimed it.
	ctx := method_request(.get, '/no-such-route')
	assert ctx.res.status_code == int(http.Status.not_found)
	assert ctx.res.header.get_custom('Allow') or { '' } == ''
}
