// param_validation_test.v - a value that cannot fill its parameter is a 400.
//
// A route parameter used to be coerced from its string silently: a missing or
// unparsable value for an `int` parameter became 0, and for a `bool` it became
// false, while the handler answered 200. `?count=abc` was indistinguishable
// from `?count=0`. The dispatcher now checks each value against the declared
// kind of the parameter it will fill, before the handler runs, and a value
// that cannot fill an `int` or a `bool` is a 400 naming the parameter.
module veb

import net.http

struct ParamValidationContext {
	Context
}

struct ParamValidationApp {}

@['/search'; get]
fn (mut app ParamValidationApp) search(mut ctx ParamValidationContext, query string, count int) Result {
	return ctx.text('${query}:${count}')
}

@['/toggle'; post]
fn (mut app ParamValidationApp) toggle(mut ctx ParamValidationContext, enabled bool) Result {
	return ctx.text(enabled.str())
}

@['/items/:id'; get]
fn (mut app ParamValidationApp) item(mut ctx ParamValidationContext, id int) Result {
	return ctx.text('item:${id}')
}

fn param_validation_request(method http.Method, url string, data string) &Context {
	mut app := ParamValidationApp{}
	routes := generate_routes[ParamValidationApp, ParamValidationContext](app) or { panic(err) }
	params := RequestParams{
		routes: &routes
	}
	req := http.Request{
		method: method
		url:    url
		data:   data
		header: http.new_header_from_map({
			http.CommonHeader.host: 'localhost'
			.content_type:          'application/x-www-form-urlencoded'
			.content_length:        '${data.len}'
		})
	}
	return handle_request_and_route[ParamValidationApp, ParamValidationContext](mut app, req,
		0, params)
}

fn test_a_parseable_int_reaches_the_handler() {
	ctx := param_validation_request(.get, '/search?query=x&count=3', '')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'x:3'
}

fn test_a_missing_int_is_a_400() {
	// Before, this answered 200 with the body `:0`.
	ctx := param_validation_request(.get, '/search?query=x', '')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('count')
}

fn test_an_unparsable_int_is_a_400() {
	// Before, `?count=abc` was indistinguishable from `?count=0`.
	ctx := param_validation_request(.get, '/search?query=x&count=abc', '')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('count')
}

fn test_a_missing_string_stays_an_empty_string() {
	// Strings have no invalid shape, so a missing one is still `''` with a 200.
	ctx := param_validation_request(.get, '/search?count=3', '')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == ':3'
}

fn test_a_parseable_bool_reaches_the_handler() {
	ctx := param_validation_request(.post, '/toggle', 'enabled=true')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'true'
}

fn test_a_missing_bool_is_a_400() {
	// Before, this answered 200 with the body `false`.
	ctx := param_validation_request(.post, '/toggle', '')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('enabled')
}

fn test_an_unparsable_bool_is_a_400() {
	ctx := param_validation_request(.post, '/toggle', 'enabled=yes')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('enabled')
}

fn test_a_parseable_path_int_reaches_the_handler() {
	ctx := param_validation_request(.get, '/items/42', '')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'item:42'
}

fn test_an_unparsable_path_int_is_a_400() {
	// Before, `/items/abc` answered 200 with the body `item:0`.
	ctx := param_validation_request(.get, '/items/abc', '')
	assert ctx.res.status_code == int(http.Status.bad_request)
	assert ctx.res.body.contains('id')
}
