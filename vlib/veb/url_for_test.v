// url_for_test.v - URLs built from handler names, not stringly-typed paths.
//
// Every redirect, every link in a template and every `Location` header is a
// route path written by hand somewhere else than the route, and renames rot
// them silently. `url_for` reads the same attributes the router reads, so the
// name and the parameters are checked where they are used. It is pure — no
// server, no request — which is what makes it fully testable here.
module veb

struct UrlApp {}

struct UrlContext {
	Context
}

@['/']
fn (app &UrlApp) index(mut ctx UrlContext) Result {
	return ctx.text('home')
}

@['/user/:id']
fn (app &UrlApp) user(mut ctx UrlContext, id int) Result {
	return ctx.text('user ${id}')
}

@['/items/:category/:id']
fn (app &UrlApp) item(mut ctx UrlContext, category string, id int) Result {
	return ctx.text('${category}/${id}')
}

@['/files/:path...']
fn (app &UrlApp) files(mut ctx UrlContext, path string) Result {
	return ctx.text(path)
}

fn (app &UrlApp) auto_mapped(mut ctx UrlContext) Result {
	return ctx.text('auto')
}

fn test_url_for_builds_a_static_route() {
	assert url_for[UrlApp]('index') or { '' } == '/'
}

fn test_url_for_substitutes_one_parameter() {
	assert url_for[UrlApp]('user', '42') or { '' } == '/user/42'
}

fn test_url_for_substitutes_parameters_in_route_order() {
	assert url_for[UrlApp]('item', 'books', '7') or { '' } == '/items/books/7'
}

fn test_url_for_joins_a_variadic_tail() {
	assert url_for[UrlApp]('files', 'a/b/c') or { '' } == '/files/a/b/c'
}

fn test_url_for_resolves_an_auto_mapped_route() {
	// No attribute means `GET /<method name>`, and the builder reads the same
	// rule the router does.
	assert url_for[UrlApp]('auto_mapped') or { '' } == '/auto_mapped'
}

fn test_url_for_rejects_an_unknown_handler() {
	if _ := url_for[UrlApp]('no_such_handler') {
		assert false, 'an unknown handler must be none, not a path'
	}
}

fn test_url_for_rejects_a_missing_parameter() {
	if _ := url_for[UrlApp]('user') {
		assert false, 'a route with an unfilled parameter must be none'
	}
}

fn test_url_for_rejects_an_extra_parameter() {
	// Extra values have nowhere to go; silently dropping them would hide a
	// caller that drifted from the route.
	if _ := url_for[UrlApp]('user', '42', 'spare') {
		assert false, 'surplus parameters must be none, not a longer path'
	}
}
