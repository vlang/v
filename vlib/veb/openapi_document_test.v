module veb

import json2
import net.http
import os
import time
import veb.tests.openapi_left
import veb.tests.openapi_right

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
	object           string       @[example: '{"ok":true}']
	array            string       @[example: '[1,2]']
	number           int          @[example: '42']
	boolean          bool         @[example: 'true']
	text             string       @[example: 'plain text']
	nested           TypedApiNode @[description: 'A recursive node']
	string_boolean   string       @[example: '"true"']
	string_number    string       @[example: '"42"']
	string_null      string       @[example: '"null"']
	omitted          string       @[omitempty]
	required_omitted string       @[required; omitempty]
	optional_omitted ?string      @[omitempty]
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
	assert query.any(api_at(it, 'name').str() == 'name' && api_at(it, 'in').str() == 'query')
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
	assert doc.schema[&TypedApiNode]()['nullable']!.bool()
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
	for field in ['string_boolean', 'string_number', 'string_null'] {
		assert api_at(examples, 'properties', field, 'example') is string
	}
	assert api_at(examples, 'properties', 'string_boolean', 'example').str() == 'true'
	assert api_at(examples, 'properties', 'string_number', 'example').str() == '42'
	assert api_at(examples, 'properties', 'string_null', 'example').str() == 'null'
	required := api_at(examples, 'required').as_array().map(it.str())
	assert 'omitted' !in required
	assert 'required_omitted' !in required
	assert 'optional_omitted' !in required
}

fn test_typed_openapi_generated_ids_distinguish_mount_paths() {
	mounts := ['/a-b', '/a_b', '/a_2db', '/a/b', '/Москва', '/東京']
	mut app := &TypedApiRoot{}
	mut leaf := &TypedApiLeaf{}
	for mount in mounts {
		app.register_controller[TypedApiLeaf, TypedApiContext](mount, mut leaf)!
	}
	mut doc := new_openapi(title: 'Distinct mount paths')!
	doc.add_app[TypedApiRoot, TypedApiContext](app)!
	raw := json2.decode[json2.Any](doc.json(false)!)!
	mut ids := []string{}
	for _, path in api_at(raw, 'paths').as_map() {
		for _, operation in path.as_map() {
			id := api_at(operation, 'operationId').str()
			assert id !in ids
			ids << id
		}
	}
	mut reversed := &TypedApiRoot{}
	for mount in mounts.reverse() {
		reversed.register_controller[TypedApiLeaf, TypedApiContext](mount, mut leaf)!
	}
	mut reordered := new_openapi(title: 'Distinct mount paths')!
	reordered.add_app[TypedApiRoot, TypedApiContext](reversed)!
	assert reordered.json(false)! == doc.json(false)!
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

struct ImplicitIndexApi {
	Controller
}

fn (app &ImplicitIndexApi) index(mut ctx TypedApiContext) Result {
	return ctx.text('implicit index')
}

fn test_typed_openapi_implicit_index_aliases_match_dispatch() {
	mut app := &ImplicitIndexApi{}
	mut child := &ImplicitIndexApi{}
	app.register_controller[ImplicitIndexApi, TypedApiContext]('/users', mut child)!
	mut doc := new_openapi(title: 'Index aliases')!
	doc.add_app[ImplicitIndexApi, TypedApiContext](app)!
	paths := api_at(json2.decode[json2.Any](doc.json(false)!)!, 'paths').as_map()
	for path in ['/', '/index', '/users', '/users/index'] {
		response := render_request[ImplicitIndexApi, TypedApiContext](mut app, http.Request{ url: path })!
		assert response.res.body == 'implicit index'
		assert path in paths
	}
}

struct WireEmbedded {
	id    int
	label string
}

struct WireDto {
	WireEmbedded
	id     string
	token  ?string @[required]
	hidden string  @[skip]
}

enum WireState {
	ready  @[json: 'READY!']
	done
}

@[json_as_number]
enum WireCode {
	ok     = 4
	failed = 9
}

struct WireLeft {
	name string
}

struct WireRight {
	name string
}

type WireChoice = WireLeft | WireRight

struct WireOptional {
	choice ?WireChoice
}

struct WireQuery {
	name    string @[json: 'display_name'; required]
	limit   int
	enabled bool
}

fn test_typed_openapi_embeds_required_and_skipped_match_json2() {
	mut doc := new_openapi(title: 'Wire fields', nullable_options: true)!
	schema := json2.Any(doc.resolve_schema(doc.schema[WireDto]()))
	wire := json2.decode[json2.Any](json2.encode(WireDto{ token: 'present' }))!.as_map()
	mut keys := api_at(schema, 'properties').as_map().keys()
	keys.sort()
	mut wire_keys := wire.keys()
	wire_keys.sort()
	assert keys == wire_keys
	assert 'WireEmbedded.id' in wire
	assert 'hidden' !in keys
	required := api_at(schema, 'required').as_array().map(it.str())
	assert 'token' in required
	assert 'hidden' !in required
	assert api_at(schema, 'properties', 'token', 'nullable').bool()
	if _ := json2.decode[WireDto]('{"id":"x","label":"l"}') {
		assert false, 'required optional field must be present'
	}
	assert json2.decode[WireDto]('{"id":"x","label":"l","token":null}')!.token == none
}

fn test_typed_openapi_enum_values_match_json2() {
	mut doc := new_openapi(title: 'Enums')!
	assert doc.schema[WireState]()['enum']!.as_array() == [
		json2.decode[json2.Any](json2.encode(WireState.ready))!,
		json2.decode[json2.Any](json2.encode(WireState.done))!,
	]
	assert doc.schema[WireCode]()['type']!.str() == 'integer'
	values := doc.schema[WireCode]()['enum']!.as_array()
	assert values.map(it.int()) == [4, 9]
	assert json2.encode(WireCode.ok) == values[0].str()
	assert json2.encode(WireCode.failed) == values[1].str()
}

fn test_typed_openapi_query_names_and_required_match_bind() {
	app := typed_api()!
	mut doc := new_openapi(title: 'Query')!
	doc.add_app[TypedApiRoot, TypedApiContext](app)!
	doc.request[WireQuery]('/v1/nested/items/:id', .get)!
	raw := json2.decode[json2.Any](doc.json(false)!)!
	parameters := api_at(raw, 'paths', '/v1/nested/items/{id}', 'get', 'parameters').as_array()
	mut ctx := Context{}
	ctx.query = {
		'name':    'V'
		'limit':   '7'
		'enabled': 'true'
	}
	bound := ctx.bind[WireQuery]()!
	assert bound.name == 'V' && bound.limit == 7 && bound.enabled
	for parameter in parameters {
		if api_at(parameter, 'in').str() == 'query' {
			name := api_at(parameter, 'name').str()
			assert name in ctx.query
			assert api_at(parameter, 'required').bool() == (name == 'name')
		}
	}
	assert parameters.any(api_at(it, 'name').str() == 'name')
}

fn test_typed_openapi_sum_variants_and_nullable_contract() {
	mut doc := new_openapi(title: 'Sums', nullable_options: true)!
	schema := json2.Any(doc.resolve_schema(doc.schema[WireChoice]()))
	assert api_at(schema, 'discriminator', 'propertyName').str() == '_type'
	choices := api_at(schema, 'oneOf').as_array()
	assert choices.len == 2
	mut tags := []string{}
	for choice in choices {
		assert '_type' in api_at(choice, 'required').as_array().map(it.str())
		tags << api_at(choice, 'properties', '_type', 'enum').as_array()[0].str()
	}
	assert tags == [
		json2.decode[map[string]json2.Any](json2.encode(WireChoice(WireLeft{})))!['_type']!.str(),
		json2.decode[map[string]json2.Any](json2.encode(WireChoice(WireRight{})))!['_type']!.str(),
	]
	optional := api_at(json2.Any(doc.resolve_schema(doc.schema[WireOptional]())), 'properties', 'choice')
	assert api_at(optional, 'anyOf').as_array().len == 2
	assert api_at(optional, 'anyOf').as_array()[0].as_map()['enum']!.as_array()[0] is json2.Null
	if output := os.getenv_opt('V_OPENAPI_TEST_OUTPUT') {
		app := typed_api()!
		doc.add_app[TypedApiRoot, TypedApiContext](app)!
		doc.response[WireOptional]('/', .get)!
		doc.write(output + '.sum', true)!
	}
}

struct ConventionalApi {}

@['/search'; get]
fn (app &ConventionalApi) search(mut ctx TypedApiContext, query string, page int, active bool) Result {
	return ctx.text('${query}:${page}:${active}')
}

@['/people/:user_id/:is_active'; get]
fn (app &ConventionalApi) person(mut ctx TypedApiContext, id int, active bool) Result {
	return ctx.text('${id}:${active}')
}

fn test_typed_openapi_conventional_query_and_positional_path_parameters() {
	mut app := &ConventionalApi{}
	mut doc := new_openapi(title: 'Conventional parameters')!
	doc.add_app[ConventionalApi, TypedApiContext](app)!
	raw := json2.decode[json2.Any](doc.json(false)!)!
	query := api_at(raw, 'paths', '/search', 'get', 'parameters').as_array()
	assert query.map(api_at(it, 'name').str()) == ['query', 'page', 'active']
	assert query.map(api_at(it, 'in').str()) == ['query', 'query', 'query']
	assert query.map(api_at(it, 'required').bool()) == [false, true, true]
	assert query.map(api_at(it, 'schema', 'type').str()) == ['string', 'integer', 'boolean']
	path := api_at(raw, 'paths', '/people/{user_id}/{is_active}', 'get', 'parameters').as_array()
	assert path.map(api_at(it, 'schema', 'type').str()) == ['integer', 'boolean']
	response := render_request[ConventionalApi, TypedApiContext](mut app, http.Request{
		url:    '/search?query=V&page=2&active=true'
		method: .get
	})!
	assert response.res.body == 'V:2:true'
}

struct UnsupportedApi {}

@['/tunnel'; connect]
fn (app &UnsupportedApi) tunnel(mut ctx TypedApiContext) Result {
	return ctx.text('tunnel')
}

fn test_typed_openapi_rejects_unsupported_methods() {
	mut doc := new_openapi(title: 'Unsupported method')!
	app := &UnsupportedApi{}
	doc.add_app[UnsupportedApi, TypedApiContext](app) or {
		assert err.msg().contains('unsupported OpenAPI method: CONNECT')
		return
	}
	assert false
}

type WireTimeChoice = int | time.Time
type WireOptionalChoice = ?int | string
type WireNumericChoice = int | i64 | f64
type WireRecursiveChoice = int | []WireRecursiveChoice | map[string]WireRecursiveChoice
type WireArrayChoice = int | []WireLeft

fn test_typed_openapi_sum_context_and_recursive_schemas() {
	mut doc := new_openapi(title: 'Sum contexts')!
	times := api_at(json2.Any(doc.resolve_schema(doc.schema[WireTimeChoice]())), 'anyOf').as_array()
	time_choice := times.filter(api_at(it, 'type').str() == 'object')[0]
	assert api_at(time_choice, 'properties', '_type', 'enum').as_array()[0].str() == 'Time'
	assert api_at(time_choice, 'properties', 'value', 'type').str() == 'integer'
	assert api_at(time_choice, 'properties', 'value', 'format').str() == 'int64'
	timestamp := time.now()
	encoded_time := json2.Any(json2.decode[map[string]json2.Any](json2.encode(WireTimeChoice(timestamp)))!)
	assert api_at(encoded_time, '_type').str() == 'Time', encoded_time.str()
	assert api_at(encoded_time, 'value').i64() == timestamp.unix()
	options := api_at(json2.Any(doc.resolve_schema(doc.schema[WireOptionalChoice]())), 'anyOf').as_array()
	optional_choice := options.filter('anyOf' in it.as_map())[0]
	assert api_at(optional_choice, 'anyOf').as_array().len == 2
	assert api_at(api_at(optional_choice, 'anyOf').as_array()[0], 'maxProperties').int() == 0
	missing := ?int(none)
	assert json2.encode(WireOptionalChoice(missing)) == '{}'
	assert api_at(json2.Any(doc.resolve_schema(doc.schema[WireNumericChoice]())), 'anyOf').as_array().len == 3
	recursive := json2.Any(doc.resolve_schema(doc.schema[WireRecursiveChoice]()))
	choices := api_at(recursive, 'anyOf').as_array()
	array := choices.filter(api_at(it, 'type').str() == 'array')[0]
	map_schema := choices.filter(api_at(it, 'type').str() == 'object')[0]
	assert api_at(array, 'items', '$ref').str().ends_with('.WireRecursiveChoice')
	assert api_at(map_schema, 'additionalProperties', '$ref').str().ends_with('.WireRecursiveChoice')
	arrays := api_at(json2.Any(doc.resolve_schema(doc.schema[WireArrayChoice]())), 'anyOf').as_array()
	array_choice := arrays.filter(api_at(it, 'type').str() == 'array')[0]
	assert '_type' in api_at(array_choice, 'items', 'required').as_array().map(it.str())
}

type WireDuplicateTagChoice = openapi_left.Item | openapi_right.Item

fn test_typed_openapi_duplicate_sum_tags_allow_overlapping_variants() {
	mut doc := new_openapi(title: 'Duplicate sum tags')!
	schema := doc.resolve_schema(doc.schema[WireDuplicateTagChoice]())
	assert 'discriminator' !in schema
	assert schema['anyOf'] or { json2.Any([]json2.Any{}) }.as_array().len == 2
	for choice in schema['anyOf']!.as_array() {
		assert api_at(choice, 'properties', '_type', 'enum').as_array()[0].str() == 'Item'
	}
}

struct WireOmittedFields {
	empty    int  @[omitempty]
	forced   int  @[required; omitempty]
	optional ?int @[required; omitempty]
	pointer  &WireLeft = unsafe { nil }
	retained int
}

fn test_typed_openapi_required_fields_match_json2_omissions() {
	mut doc := new_openapi(title: 'Omitted fields')!
	schema := doc.resolve_schema(doc.schema[WireOmittedFields]())
	required := api_at(json2.Any(schema), 'required').as_array().map(it.str())
	assert required == ['retained']
	encoded := json2.decode[map[string]json2.Any](json2.encode(WireOmittedFields{ forced: 0, optional: none }))!
	assert encoded.keys() == ['retained']
}

struct WireRecursiveNode {
	children []WireNodeChoice
}

type WireNodeChoice = WireRecursiveNode | int

fn test_typed_openapi_recursive_struct_sum_preserves_component_reference() {
	mut doc := new_openapi(title: 'Recursive struct sum')!
	_ := doc.schema[WireRecursiveNode]()
	schema := doc.schemas[typeof(WireNodeChoice{}).name] or { panic('missing recursive sum schema') }
	choices := api_at(json2.Any(schema), 'anyOf').as_array()
	node := choices.filter('allOf' in it.as_map())[0]
	assert api_at(node, 'allOf').as_array()[0].as_map()['$ref']!.str().ends_with('.WireRecursiveNode')
}

@['/forms'; post]
fn (app &ConventionalApi) forms(mut ctx TypedApiContext, value string, count int) Result {
	return ctx.text('${value}:${count}')
}

fn test_typed_openapi_conventional_post_documents_dispatched_body() {
	mut app := &ConventionalApi{}
	mut doc := new_openapi(title: 'POST inputs')!
	doc.add_app[ConventionalApi, TypedApiContext](app)!
	raw := json2.decode[json2.Any](doc.json(false)!)!
	body := api_at(raw, 'paths', '/forms', 'post', 'requestBody')
	for media in ['application/x-www-form-urlencoded', 'multipart/form-data', 'application/json'] {
		schema := api_at(body, 'content', media, 'schema')
		assert api_at(schema, 'type').str() == 'object'
		assert api_at(schema, 'properties', 'value', 'type').str() == 'string'
		assert api_at(schema, 'properties', 'count', 'type').str() == 'integer'
		assert api_at(schema, 'required').as_array().map(it.str()) == ['count']
	}
	for media, data in {
		'application/x-www-form-urlencoded': 'value=V&count=3'
		'application/json':                  '{"value":"V","count":3}'
	} {
		response := render_request[ConventionalApi, TypedApiContext](mut app, http.Request{
			url:    '/forms'
			method: .post
			header: http.new_header(key: .content_type, value: media)
			data:   data
		})!
		assert response.res.body == 'V:3'
	}
	missing := render_request[ConventionalApi, TypedApiContext](mut app, http.Request{
		url:    '/forms'
		method: .post
		data:   'value=V'
	})!
	assert missing.res.status_code == 400
}

struct WideIntegerApi {}

@['/wide/:id/:small'; get]
fn (app &WideIntegerApi) wide(mut ctx TypedApiContext, id i64, small u32) Result {
	return ctx.text('${id}:${small}')
}

@['/wide-query'; get; post]
fn (app &WideIntegerApi) query(mut ctx TypedApiContext, id i64, small u32, full u64) Result {
	return ctx.text('${id}:${small}:${full}')
}

fn test_typed_openapi_integer_handler_schemas_keep_width() {
	app := &WideIntegerApi{}
	mut doc := new_openapi(title: 'Integer inputs')!
	doc.add_app[WideIntegerApi, TypedApiContext](app)!
	for route in route_info[WideIntegerApi, TypedApiContext](app)! {
		assert route.parameter_types.all(it == 'integer')
		assert route.parameter_required.all(!it)
	}
	raw := json2.decode[json2.Any](doc.json(false)!)!
	for path in ['/wide/{id}/{small}', '/wide-query'] {
		params := api_at(raw, 'paths', path, 'get', 'parameters').as_array()
		assert params.all(api_at(it, 'schema', 'type').str() == 'integer')
		assert api_at(params[0], 'schema', 'format').str() == 'int64'
		assert api_at(params[1], 'schema', 'format').str() == 'int64'
	}
	body := api_at(raw, 'paths', '/wide-query', 'post', 'requestBody')
	assert !api_at(body, 'required').bool()
	properties := api_at(body, 'content', 'application/json', 'schema', 'properties')
	assert api_at(properties, 'id', 'format').str() == 'int64'
	assert api_at(properties, 'small', 'format').str() == 'int64'
	assert api_at(properties, 'full', 'type').str() == 'integer'
	assert 'format' !in api_at(properties, 'full').as_map()
}

struct WireCustomNumber {
	internal []u32
}

fn (_ WireCustomNumber) to_json() string {
	return '9007199254740993'
}

struct WireLegacyString {
	internal bool
}

fn (_ WireLegacyString) json_str() string {
	return '"custom"'
}

type WireCustomChoice = WireCustomNumber | WireLeft

fn test_typed_openapi_custom_encoder_precedes_reflection_and_accepts_overrides() {
	mut doc := new_openapi(title: 'Custom encoders')!
	assert doc.resolve_schema(doc.schema[WireCustomNumber]()).len == 0
	assert doc.resolve_schema(doc.schema[WireLegacyString]()).len == 0
	assert json2.encode(WireCustomNumber{}) == '9007199254740993'
	assert json2.encode(WireLegacyString{}) == '"custom"'
	assert json2.encode(WireCustomChoice(WireCustomNumber{})) == '{"internal":[],"_type":"WireCustomNumber"}'
	choice := doc.resolve_schema(doc.schema[WireCustomChoice]())
	assert 'discriminator' !in choice
	variants := choice['anyOf'] or { json2.Any([]json2.Any{}) }.as_array()
	assert variants.len == 2
	custom_variant := variants[0]
	assert api_at(custom_variant, 'properties', 'internal', 'type').str() == 'array'
	assert api_at(custom_variant, 'properties', '_type', 'enum').as_array()[0].str() == 'WireCustomNumber'
	name := typeof(WireCustomNumber{}).name
	doc.custom_schema(name, {
		'type': json2.Any('integer')
	})
	assert doc.resolve_schema(doc.schema[WireCustomNumber]())['type']!.str() == 'integer'
	assert doc.resolve_schema(doc.schema[WireCustomNumber]())['properties'] or { json2.Any(json2.null) } is json2.Null
}

struct WireJsonNull {
	value ?string        @[json_null]
	node  ?TypedApiNode  @[json_null]
	state ?TypedApiState @[json_null]
}

fn test_typed_openapi_json_null_attribute_allows_null_without_global_option() {
	mut doc := new_openapi(title: 'Forced null')!
	schema := json2.Any(doc.resolve_schema(doc.schema[WireJsonNull]()))
	assert api_at(schema, 'required').as_array().map(it.str()) == ['value', 'node', 'state']
	for field in ['value', 'node', 'state'] {
		property := api_at(schema, 'properties', field)
		assert api_at(property, 'nullable').bool()
	}
	encoded := json2.decode[map[string]json2.Any](json2.encode(WireJsonNull{}))!
	assert encoded['value']! is json2.Null
	assert encoded['node']! is json2.Null
	assert encoded['state']! is json2.Null
}

fn test_typed_openapi_pointer_nullability_covers_containers_and_responses() {
	mut doc := new_openapi(title: 'Nil pointers')!
	pointer := &int(unsafe { nil })
	assert json2.encode(pointer) == 'null'
	assert json2.encode([pointer]) == '[null]'
	assert json2.encode({
		'key': pointer
	}) == '{"key":null}'
	assert api_at(json2.Any(doc.schema[&int]()), 'nullable').bool()
	assert api_at(json2.Any(doc.schema[[]&int]()), 'items', 'nullable').bool()
	assert api_at(json2.Any(doc.schema[map[string]&int]()), 'additionalProperties', 'nullable').bool()
	assert api_at(json2.Any(doc.schema[&TypedApiNode]()), 'nullable').bool()
	app := &ConventionalApi{}
	doc.add_app[ConventionalApi, TypedApiContext](app)!
	doc.response[&int]('/search', .get)!
	raw := json2.decode[json2.Any](doc.json(false)!)!
	assert api_at(raw, 'paths', '/search', 'get', 'responses', '200', 'content', 'application/json', 'schema', 'nullable').bool()
}

fn test_typed_openapi_sum_first_reference_observes_later_override() {
	app := &ConventionalApi{}
	mut doc := new_openapi(title: 'Sum references')!
	doc.add_app[ConventionalApi, TypedApiContext](app)!
	doc.response[WireNumericChoice]('/search', .get)!
	first := doc.schema[WireNumericChoice]()
	assert '$ref' in first
	assert doc.schema[WireNumericChoice]() == first
	doc.custom_schema(typeof(WireNumericChoice{}).name, {
		'type': json2.Any('string')
	})
	assert doc.resolve_schema(first)['type']!.str() == 'string'
	raw := json2.decode[json2.Any](doc.json(false)!)!
	response := api_at(raw, 'paths', '/search', 'get', 'responses', '200', 'content', 'application/json', 'schema')
	assert response.as_map() == first
}

fn test_typed_openapi_unsigned_64_bit_schema_accepts_full_range() {
	mut doc := new_openapi(title: 'Unsigned values')!
	assert json2.encode(u64(18446744073709551615)) == '18446744073709551615'
	for schema in [doc.schema[u64](), doc.schema[usize]()] {
		assert schema['type']!.str() == 'integer'
		assert api_at(json2.Any(schema), 'minimum').str() == '0'
		if sizeof(usize) == 8 {
			assert 'format' !in schema
		}
	}
}
