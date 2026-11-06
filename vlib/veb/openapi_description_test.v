module veb

import json2
import net.http

struct DescribedApiContext {
	Context
}

struct DescribedApiApp {
	Controller
}

struct DescribedApiChild {}

@[description: 'The root operation.']
fn (app &DescribedApiApp) index(mut ctx DescribedApiContext) Result {
	return ctx.text('root')
}

@['/plain'; get]
fn (app &DescribedApiApp) plain(mut ctx DescribedApiContext) Result {
	return ctx.text('plain')
}

@[description: r'C:\items']
@['/raw'; get]
fn (app &DescribedApiApp) raw_description(mut ctx DescribedApiContext) Result {
	return ctx.text('raw')
}

@[description: '"Create" an item: first line.\nSecond line.']
@['/items'; get; post; host: 'api.example.com'; OpenApi('items_metadata')]
fn (app &DescribedApiChild) items(mut ctx DescribedApiContext) !Result {
	return ctx.text('items')
}

fn (app &DescribedApiChild) items_metadata(mut handler OpenApiHandler) ! {
	handler.response[string]()!
}

fn described_api() !&DescribedApiApp {
	mut app := &DescribedApiApp{}
	mut child := &DescribedApiChild{}
	app.register_controller[DescribedApiChild, DescribedApiContext]('/v1', mut child)!
	return app
}

fn description_operation(doc OpenApiDocument, path string, method string) map[string]json2.Any {
	return doc.paths[path][method]
}

fn test_openapi_handler_descriptions_follow_mounts_and_index_aliases() {
	mut app := described_api()!
	routes := route_info[DescribedApiApp, DescribedApiContext](app)!
	assert routes.len == 5
	for route in routes {
		match route.path {
			'/', '/index' {
				assert route.description == 'The root operation.'
			}
			'/plain' {
				assert route.description == ''
			}
			'/raw' {
				assert route.description == r'C:\items'
			}
			'/v1/items' {
				assert route.description == '"Create" an item: first line.\nSecond line.'
				assert route.methods == [http.Method.get, .post]
				assert route.host == 'api.example.com'
			}
			else {
				assert false, 'unexpected route ${route.path}'
			}
		}
	}
	mut doc := new_openapi(title: 'Handler descriptions')!
	doc.add_app[DescribedApiApp, DescribedApiContext](app)!
	for path in ['/', '/index'] {
		assert description_operation(doc, path, 'get')['description']!.str() == 'The root operation.'
	}
	for method in ['get', 'post'] {
		operation := description_operation(doc, '/v1/items', method)
		assert operation['description']!.str() == '"Create" an item: first line.\nSecond line.'
		assert operation['x-veb-host']!.str() == 'api.example.com'
		response := operation['responses']!.as_map()['200']!.as_map()
		schema := response['content']!.as_map()['application/json']!.as_map()['schema']!.as_map()
		assert schema['type']!.str() == 'string'
	}
	assert 'description' !in description_operation(doc, '/plain', 'get')
	assert description_operation(doc, '/raw', 'get')['description']!.str() == r'C:\items'
	raw := json2.decode[json2.Any](doc.json(false)!)!
	paths := raw.as_map()['paths']!.as_map()
	get := paths['/v1/items']!.as_map()['get']!.as_map()
	assert get['description']!.str() == '"Create" an item: first line.\nSecond line.'
	for method in [http.Method.get, .post] {
		response := render_request[DescribedApiApp, DescribedApiContext](mut app, http.Request{
			method: method
			url:    '/v1/items'
			header: http.new_header(key: .host, value: 'api.example.com')
		})!
		assert response.res.body == 'items'
	}
}

fn test_openapi_explicit_description_overrides_one_operation() {
	app := described_api()!
	mut doc := new_openapi(title: 'Description overrides')!
	doc.add_app[DescribedApiApp, DescribedApiContext](app)!
	doc.operation('/v1/items', .post, description: 'An explicit description.')!
	assert description_operation(doc, '/v1/items', 'post')['description']!.str() == 'An explicit description.'
	assert description_operation(doc, '/v1/items', 'get')['description']!.str() == '"Create" an item: first line.\nSecond line.'
	doc.operation('/v1/items', .get, summary: 'List items')!
	assert description_operation(doc, '/v1/items', 'get')['description']!.str() == '"Create" an item: first line.\nSecond line.'
}

fn test_route_description_preserves_routing_attributes_and_quoted_text() {
	attrs := ['description: \'"quoted"\'', '/items', 'post', 'host: Example.COM', 'unsafe']
	methods, path, host := parse_attrs('items', attrs)!
	assert methods == [http.Method.post]
	assert path == '/items'
	assert host == 'example.com'
	assert route_description(attrs) == '"quoted"'
	assert route_description(['description: "A description"']) == 'A description'
	assert route_description(["description: r'C:\\items'"]) == r'C:\items'
	assert route_description(['description: ']) == ''
	assert route_description([]) == ''
}
