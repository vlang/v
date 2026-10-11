// vtest vflags: -new-compiler
module veb

import net.http

struct RouteArgumentsContext {
	Context
}

struct RouteArgumentsApp {}

@['/index'; get; post]
fn (mut app RouteArgumentsApp) index(mut ctx RouteArgumentsContext, name string, count int) Result {
	return ctx.text('${name}:${count}')
}

@['/forms'; post]
fn (mut app RouteArgumentsApp) forms(mut ctx RouteArgumentsContext, value string) Result {
	return ctx.text(value)
}

@['/query'; get]
fn (mut app RouteArgumentsApp) query(mut ctx RouteArgumentsContext, value string, count int) Result {
	return ctx.text('${value}:${count}')
}

@['/items/:id'; get; post; delete]
fn (mut app RouteArgumentsApp) item(mut ctx RouteArgumentsContext, id int) Result {
	return ctx.text('item:${id}')
}

@['/bools'; get; post]
fn (mut app RouteArgumentsApp) bools(mut ctx RouteArgumentsContext, enabled bool) Result {
	return ctx.text(enabled.str())
}

@['/ping'; get; post; put; patch; delete; head; options]
fn (mut app RouteArgumentsApp) ping(mut ctx RouteArgumentsContext) Result {
	return ctx.text(ctx.req.method.str())
}

fn route_arguments_request(method http.Method, url string, data string) &Context {
	mut app := RouteArgumentsApp{}
	routes := generate_routes[RouteArgumentsApp, RouteArgumentsContext](app) or { panic(err) }
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
	return handle_request_and_route[RouteArgumentsApp, RouteArgumentsContext](mut app, req, 0,
		params)
}

fn test_route_arguments_parameterized_index() {
	root := route_arguments_request(.get, '/?name=root+query&count=3', '')
	assert root.res.status_code == int(http.Status.ok)
	assert root.res.body == 'root query:3'

	index := route_arguments_request(.get, '/index?count=4&name=index+query', '')
	assert index.res.status_code == int(http.Status.ok)
	assert index.res.body == 'index query:4'

	form := route_arguments_request(.post, '/', 'count=5&name=root+form')
	assert form.res.status_code == int(http.Status.ok)
	assert form.res.body == 'root form:5'
}

fn test_route_arguments_post_form() {
	ctx := route_arguments_request(.post, '/forms?value=query', 'value=form+%26+value')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'form & value'
}

fn test_route_arguments_get_query() {
	ctx := route_arguments_request(.get, '/query?count=42&value=query+%26+value&ignored=other', '')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'query & value:42'
}

fn test_route_arguments_missing_values() {
	query := route_arguments_request(.get, '/query', '')
	assert query.res.status_code == int(http.Status.ok)
	assert query.res.body == ':0'

	form := route_arguments_request(.post, '/forms', '')
	assert form.res.status_code == int(http.Status.ok)
	assert form.res.body == ''

	index := route_arguments_request(.get, '/', '')
	assert index.res.status_code == int(http.Status.ok)
	assert index.res.body == ':0'
}

fn test_route_arguments_path_parameter_on_delete() {
	ctx := route_arguments_request(.delete, '/items/42?id=99', '')
	assert ctx.res.status_code == int(http.Status.ok)
	assert ctx.res.body == 'item:42'
}

fn test_route_arguments_path_parameters_take_precedence_over_query_and_form() {
	query := route_arguments_request(.get, '/items/42?id=99', '')
	assert query.res.body == 'item:42'
	form := route_arguments_request(.post, '/items/43?id=99', 'id=100')
	assert form.res.body == 'item:43'
}

fn test_route_arguments_boolean_conversion_and_missing_value() {
	assert route_arguments_request(.get, '/bools?enabled=true', '').res.body == 'true'
	assert route_arguments_request(.post, '/bools?enabled=false', 'enabled=true').res.body == 'true'
	assert route_arguments_request(.get, '/bools', '').res.body == 'false'
}

fn test_route_arguments_context_only_handlers() {
	for method in [http.Method.get, .post, .put, .patch, .delete, .head, .options] {
		ctx := route_arguments_request(method, '/ping', '')
		assert ctx.res.status_code == int(http.Status.ok)
		assert ctx.res.body == method.str()
	}
}
