module veb

import json2
import net.http
import os
import time

struct TypedApiContext {
	Context
}

struct TypedApiRoot {
	Controller
}

struct TypedApiGroup {
	Controller
}

struct TypedApiLeaf {}

@['/'; get]
fn (app &TypedApiRoot) index(mut ctx TypedApiContext) Result {
	return ctx.text('root')
}

@['/items/:id'; get; post; put; patch; delete; options; head]
fn (app &TypedApiLeaf) item(mut ctx TypedApiContext, id int) Result {
	return ctx.text(id.str())
}

@['/files/:path...'; get]
fn (app &TypedApiLeaf) files(mut ctx TypedApiContext, path string) Result {
	return ctx.text(path)
}

$if openapi_extra ? {
	@['/extra'; get]
	fn (app &TypedApiRoot) extra(mut ctx TypedApiContext) Result {
		return ctx.text('extra')
	}
}

struct TypedApiDto {
	name       string @[json: 'display_name'; description: 'A display name'; example: '"V"']
	optional   ?string
	maybe_node ?TypedApiNode
	count      u32
	created    time.Time
	matrix     [][]f32
	labels     map[string]i64
	anything   json2.Any
	hidden     string @[json: '-']
}

struct TypedApiNode {
	value    string
	children []TypedApiNode
}

enum TypedApiState {
	ready
	done
}

struct TypedApiExamples {
	object  string       @[example: '{"ok":true}']
	array   string       @[example: '[1,2]']
	number  int          @[example: '42']
	boolean bool         @[example: 'true']
	text    string       @[example: 'plain text']
	nested  TypedApiNode @[description: 'A recursive node']
}

fn typed_api() !&TypedApiRoot {
	mut app := &TypedApiRoot{}
	mut group := &TypedApiGroup{}
	mut leaf := &TypedApiLeaf{}
	group.register_controller[TypedApiLeaf, TypedApiContext]('/nested', mut leaf)!
	app.register_host_controller[TypedApiGroup, TypedApiContext]('api.example.com', '/v1', mut group)!
	return app
}

fn api_at(value json2.Any, keys ...string) json2.Any {
	mut result := value
	for key in keys { result = result.as_map()[key] or { json2.Any(json2.Null{}) } }
	return result
}

fn typed_document() !OpenApiDocument {
	app := typed_api()!
	mut doc := new_openapi(
		title:               'Typed "API"\nМосква'
		version:             '2.0'
		validation_response: true
		security_schemes:    {
			'token': json2.Any({
				'type':         json2.Any('http')
				'scheme':       json2.Any('bearer')
				'bearerFormat': json2.Any('JWT')
			})
		}
		security:            [{
			'token': []string{}
		}]
		success_field:       'data'
	)!
	doc.add_app[TypedApiRoot, TypedApiContext](app)!
	doc.operation('/v1/nested/items/:id', .post,
		summary:      'Create item'
		description:  'First line\nSecond line'
		tags:         ['items', 'write']
		path_schemas: {
			'id': OpenApiSchema({
				'type':   json2.Any('integer')
				'format': json2.Any('int64')
			})
		}
	)!
	doc.request[TypedApiDto]('/v1/nested/items/:id', .post)!
	doc.request[TypedApiDto]('/v1/nested/items/:id', .get)!
	doc.response[[]TypedApiDto]('/v1/nested/items/:id', .post, status: 201)!
	doc.response[TypedApiDto]('/v1/nested/items/:id', .post,
		status:      422
		description: 'Invalid input'
	)!
	doc.operation('/', .get, security: []map[string][]string{})!
	return doc
}

fn test_typed_openapi_matches_dispatcher_and_mounts() {
	mut app := typed_api()!
	routes := route_info[TypedApiRoot, TypedApiContext](app)!
	mut doc := typed_document()!
	raw := json2.decode[json2.Any](doc.json(false)!)!
	paths := api_at(raw, 'paths').as_map()
	mut operations := 0
	for route in routes {
		path := openapi_path_of(route.path)
		for method in route.methods {
			operations++
			assert method.str().to_lower() in (paths[path] or { json2.Any(json2.null) }).as_map()
		}
	}
	mut emitted := 0
	for _, item in paths { emitted += item.as_map().len }
	assert emitted == operations
	assert api_at(raw, 'paths', '/v1/nested/items/{id}', 'patch', 'x-veb-host').str() == 'api.example.com'
	assert api_at(raw, 'paths', '/v1/nested/items/{id}').as_map().len == 7
	params := api_at(raw, 'paths', '/v1/nested/files/{path}', 'get', 'parameters').as_array()
	assert api_at(params[0], 'name').str() == 'path'
	assert api_at(params[0], 'x-veb-variadic').bool()
	$if openapi_extra ? {
		assert '/extra' in paths
	} $else {
		assert '/extra' !in paths
	}
	response := render_request[TypedApiRoot, TypedApiContext](mut app, http.Request{
		url:    '/v1/nested/items/42'
		header: http.new_header(
			key:   .host
			value: 'api.example.com'
		)
	})!
	assert response.res.body == '42'
}

fn test_typed_openapi_schema_requests_responses_and_security() {
	mut doc := typed_document()!
	_ = doc.schema[TypedApiNode]()
	raw := json2.decode[json2.Any](doc.json(false)!)!
	assert api_at(raw, 'openapi').str() == '3.0.3'
	assert api_at(raw, 'info', 'title').str() == 'Typed "API"\nМосква'
	post := api_at(raw, 'paths', '/v1/nested/items/{id}', 'post')
	assert api_at(post, 'description').str() == 'First line\nSecond line'
	assert api_at(post, 'responses', '201', 'description').str() == 'Created'
	assert '200' !in api_at(post, 'responses').as_map()
	for code in ['400', '500', '401', '403', '422'] {
		assert code in api_at(post, 'responses').as_map()
	}
	assert api_at(post, 'responses', '201', 'content', 'application/json', 'schema', 'properties', 'data', 'type').str() == 'array'
	assert api_at(post, 'responses', '422', 'content', 'application/json', 'schema', '$ref').str().ends_with('TypedApiDto')
	assert api_at(post, 'requestBody', 'required').bool()
	assert api_at(post, 'requestBody', 'content', 'application/json', 'schema', '$ref').str().ends_with('TypedApiDto')
	assert api_at(raw, 'paths', '/', 'get', 'security').as_array().len == 0
	assert '401' !in api_at(raw, 'paths', '/', 'get', 'responses').as_map()
	name := doc.schema[TypedApiDto]()
	dto := doc.resolve_schema(name)
	properties := api_at(json2.Any(dto), 'properties')
	assert api_at(properties, 'display_name', 'example').str() == 'V'
	assert api_at(properties, 'display_name', 'description').str() == 'A display name'
	assert api_at(properties, 'count', 'format').str() == 'int64'
	assert api_at(properties, 'created', 'format').str() == 'date-time'
	assert api_at(properties, 'matrix', 'items', 'items', 'format').str() == 'float'
	assert api_at(properties, 'labels', 'additionalProperties', 'format').str() == 'int64'
	assert 'hidden' !in properties.as_map()
	assert 'nullable' !in api_at(properties, 'optional').as_map()
	assert 'optional' !in api_at(json2.Any(dto), 'required').as_array().map(it.str())
	query := api_at(raw, 'paths', '/v1/nested/items/{id}', 'get', 'parameters').as_array()
	assert query.any(api_at(it, 'name').str() == 'display_name' && api_at(it, 'in').str() == 'query')
	assert api_at(raw, 'components', 'schemas', typeof(TypedApiNode{}).name, 'properties', 'children', 'items', '$ref').str().ends_with('TypedApiNode')
	assert doc.json(false)! == doc.json(false)!
	assert json2.decode[json2.Any](doc.json(true)!)! == raw
	path := os.join_path(os.temp_dir(), 'veb-openapi-${os.getpid()}.json')
	defer { os.rm(path) or {} }
	doc.write(path, true)!
	assert os.read_file(path)! == doc.json(true)!
	if output := os.getenv_opt('V_OPENAPI_TEST_OUTPUT') { doc.write(output, true)! }
}

fn test_typed_openapi_overrides_errors_and_validation() {
	app := typed_api()!
	mut doc := new_openapi(
		title:            'Custom'
		nullable_options: true
		default_errors:   false
		schema_names:     {
			typeof(TypedApiDto{}).name: 'Payload'
		}
	)!
	doc.add_app[TypedApiRoot, TypedApiContext](app)!
	doc.request[TypedApiDto]('/v1/nested/items/:id', .patch,
		media_type: 'application/vnd.test+json'
	)!
	assert api_at(json2.Any(doc.schema[TypedApiDto]()), '$ref').str() == '#/components/schemas/Payload'
	dto := doc.resolve_schema(doc.schema[TypedApiDto]())
	assert api_at(json2.Any(dto), 'properties', 'optional', 'nullable').bool()
	assert api_at(json2.Any(dto), 'properties', 'maybe_node', 'anyOf').as_array().len == 2
	if output := os.getenv_opt('V_OPENAPI_TEST_OUTPUT') { doc.write(output + '.nullable', true)! }
	doc.custom_schema('Payload', {
		'type': json2.Any('string')
	})
	assert api_at(json2.Any(doc.resolve_schema(doc.schema[TypedApiDto]())), 'type').str() == 'string'
	doc.add_app[TypedApiRoot, TypedApiContext](app) or {
		assert err.msg().contains('duplicate OpenAPI operation:')
		return
	}
	assert false, 'duplicate registrations must fail'
}

fn test_typed_openapi_scalar_schemas_and_json_examples() {
	mut doc := new_openapi(title: 'Schemas')!
	assert doc.schema[i8]() == OpenApiSchema({
		'type':   json2.Any('integer')
		'format': json2.Any('int32')
	})
	assert doc.schema[u32]()['format']!.str() == 'int64'
	assert doc.schema[f64]()['format']!.str() == 'double'
	assert doc.schema[bool]()['type']!.str() == 'boolean'
	assert doc.schema[[2]int]()['type']!.str() == 'array'
	assert doc.schema[&TypedApiNode]() == doc.schema[TypedApiNode]()
	assert doc.schema[TypedApiState]()['enum']!.as_array().map(it.str()) == ['ready', 'done']
	assert doc.schema[json2.Any]().len == 0
	examples := json2.Any(doc.resolve_schema(doc.schema[TypedApiExamples]()))
	assert api_at(examples, 'properties', 'object', 'example').as_map()['ok']!.bool()
	assert api_at(examples, 'properties', 'array', 'example').as_array().len == 2
	assert api_at(examples, 'properties', 'number', 'example').int() == 42
	assert api_at(examples, 'properties', 'boolean', 'example').bool()
	assert api_at(examples, 'properties', 'text', 'example').str() == 'plain text'
	assert api_at(examples, 'properties', 'nested', 'allOf').as_array().len == 1
	assert api_at(examples, 'properties', 'nested', 'description').str() == 'A recursive node'
}

fn test_typed_openapi_version_security_and_response_overrides() {
	app := typed_api()!
	version_path := os.join_path(os.temp_dir(), 'veb-openapi-${os.getpid()}.mod')
	os.write_file(version_path, "Module { name: 'test', version: '9.8.7' }")!
	defer { os.rm(version_path) or {} }
	mut doc := new_openapi(
		title:                 'Overrides'
		version_file:          version_path
		response_descriptions: {
			'202': 'Queued for processing'
		}
		controller_security:   {
			typeof(TypedApiLeaf{}).name: []map[string][]string{}
		}
		security_schemes:      {
			'key': json2.Any({
				'type': json2.Any('apiKey')
				'in':   json2.Any('header')
				'name': json2.Any('X-Key')
			})
		}
		security:              [{
			'key': []string{}
		}]
	)!
	doc.add_app[TypedApiRoot, TypedApiContext](app)!
	doc.response[string]('/', .get, status: 200)!
	doc.response[string]('/', .get, status: 202)!
	doc.request[TypedApiDto]('/v1/nested/items/:id', .post, query: true)!
	doc.request[TypedApiDto]('/v1/nested/items/:id', .post, query: false)!
	mut ctx := Context{}
	_ = doc.serve(mut ctx, false)!
	assert (ctx.res.header.get(.content_type) or { '' }) == 'application/json'
	raw := json2.decode[json2.Any](ctx.res.body)!
	assert api_at(raw, 'info', 'version').str() == '9.8.7'
	assert '200' in api_at(raw, 'paths', '/', 'get', 'responses').as_map()
	assert '202' in api_at(raw, 'paths', '/', 'get', 'responses').as_map()
	assert api_at(raw, 'paths', '/', 'get', 'responses', '202', 'description').str() == 'Queued for processing'
	assert api_at(raw, 'paths', '/v1/nested/items/{id}', 'post', 'security').as_array().len == 0
	assert '401' !in api_at(raw, 'paths', '/v1/nested/items/{id}', 'post', 'responses').as_map()
	assert api_at(raw, 'paths', '/v1/nested/items/{id}', 'post', 'parameters').as_array().len > 1
	assert 'requestBody' in api_at(raw, 'paths', '/v1/nested/items/{id}', 'post').as_map()
	doc.operation('/', .get,
		security: [{
			'unknown': []string{}
		}]
	)!
	doc.json(false) or {
		assert err.msg() == 'undefined OpenAPI security scheme: unknown'
		return
	}
	assert false, 'undefined security must fail'
}

fn test_typed_openapi_duplicate_ids_and_invalid_components() {
	app := typed_api()!
	mut doc := new_openapi(title: 'Validation')!
	doc.add_app[TypedApiRoot, TypedApiContext](app)!
	doc.operation('/', .get, operation_id: 'same')!
	doc.operation('/v1/nested/items/:id', .get, operation_id: 'same')!
	if _ := doc.json(false) {
		assert false
	} else {
		assert err.msg() == 'duplicate OpenAPI operationId: same'
	}
	doc.custom_schema('invalid/name', {})
	if _ := doc.json(false) {
		assert false
	} else {
		assert err.msg().contains('invalid OpenAPI component name:')
	}
	if _ := new_openapi(openapi_version: '3.1.0') {
		assert false
	}
	if _ := new_openapi(openapi_version: '3.0.invalid') {
		assert false
	}
}
