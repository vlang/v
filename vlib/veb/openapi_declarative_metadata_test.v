module veb

import json2

struct DeclarativeTagContext {
	Context
}

struct DeclarativeTagRoot {
	Controller
}

struct DeclarativeTagDomain {
	Controller
}

@[OpenApiTag('base.currency', 'Currencies', 'base')]
struct DeclarativeCurrency {}

struct DeclarativeLanguage {}

@[OpenApiTag('shared', '', '')]
struct DeclarativeSharedTags {}

struct DeclarativeUnannotated {}

@[OpenApiTag('ignored', '', '')]
struct DeclarativeUnregistered {}

@['/currencies'; get]
fn (app &DeclarativeCurrency) currencies(mut ctx DeclarativeTagContext) Result {
	return ctx.text('currencies')
}

$if declarative_currency_extra ? {
	@['/currencies/extra'; get]
	fn (app &DeclarativeCurrency) extra(mut ctx DeclarativeTagContext) Result {
		return ctx.text('extra')
	}
}

@['/languages'; get]
fn (app &DeclarativeLanguage) languages(mut ctx DeclarativeTagContext) Result {
	return ctx.text('languages')
}

@[OpenApiTags]
fn (app &DeclarativeLanguage) openapi_tags() []OpenApiTagConfig {
	return [
		OpenApiTagConfig{
			name:    'base.language'
			summary: 'Languages'
			parent:  'base'
		},
		OpenApiTagConfig{
			name: 'shared'
		},
	]
}

@['/shared'; get]
fn (app &DeclarativeSharedTags) shared(mut ctx DeclarativeTagContext) Result {
	return ctx.text('shared')
}

@['/plain'; get]
fn (app &DeclarativeUnannotated) plain(mut ctx DeclarativeTagContext) Result {
	return ctx.text('plain')
}

@['/ignored'; get]
fn (app &DeclarativeUnregistered) ignored(mut ctx DeclarativeTagContext) Result {
	return ctx.text('ignored')
}

fn declarative_tag_app() !&DeclarativeTagRoot {
	mut app := &DeclarativeTagRoot{}
	mut domain := &DeclarativeTagDomain{}
	domain.openapi_tag_group(OpenApiTagGroup{
		name:        'base'
		summary:     'Base data'
		description: 'Currencies and languages.'
	})
	app.openapi_tag_group(OpenApiTagGroup{
		name:    'empty'
		summary: 'Reserved for future controllers'
	})
	app.openapi_tag_group(OpenApiTagGroup{
		name:    'empty'
		summary: 'Reserved for future controllers'
	})
	mut currency_one := &DeclarativeCurrency{}
	mut currency_two := &DeclarativeCurrency{}
	mut languages := &DeclarativeLanguage{}
	mut shared := &DeclarativeSharedTags{}
	domain.register_controller[DeclarativeCurrency, DeclarativeTagContext]('/v1', mut currency_one)!
	domain.register_controller[DeclarativeCurrency, DeclarativeTagContext]('/v2', mut currency_two)!
	domain.register_controller[DeclarativeLanguage, DeclarativeTagContext]('/languages', mut languages)!
	domain.register_controller[DeclarativeSharedTags, DeclarativeTagContext]('/shared', mut shared)!
	app.register_controller[DeclarativeTagDomain, DeclarativeTagContext]('/base', mut domain)!
	mut plain := &DeclarativeUnannotated{}
	app.register_controller[DeclarativeUnannotated, DeclarativeTagContext]('/plain', mut plain)!
	return app
}

fn declarative_api_at(value json2.Any, keys ...string) json2.Any {
	mut result := value
	for key in keys {
		result = result.as_map()[key] or { json2.Any(json2.Null{}) }
	}
	return result
}

fn declarative_tag_names(raw json2.Any) []string {
	return declarative_api_at(raw, 'tags').as_array().map(declarative_api_at(it, 'name').str())
}

fn test_openapi_collects_route_groups_and_controller_tag_metadata() {
	app := declarative_tag_app()!
	mut doc := new_openapi(
		openapi_version: '3.2.0'
		tag_definitions: [
			OpenApiTagConfig{
				name:        'audit'
				summary:     'Audit'
				controllers: [typeof(DeclarativeLanguage{}).name]
			},
		]
	)!
	doc.add_app[DeclarativeTagRoot, DeclarativeTagContext](app)!
	raw := json2.decode[json2.Any](doc.json(false)!)!
	names := declarative_tag_names(raw)
	assert names.contains('base')
	assert names.contains('empty')
	assert names.contains('base.currency')
	assert names.contains('base.language')
	assert names.contains('shared')
	assert names.contains('audit')
	assert 'ignored' !in names
	assert names.index('base') < names.index('base.currency')
	assert names.index('base') < names.index('base.language')

	for prefix in ['/base/v1', '/base/v2'] {
		assert declarative_api_at(raw, 'paths', prefix + '/currencies', 'get', 'tags').as_array().map(it.str()) == [
			'base.currency',
		]
	}
	assert declarative_api_at(raw, 'paths', '/base/languages/languages', 'get', 'tags').as_array().map(it.str()) == [
		'audit',
		'base.language',
		'shared',
	]
	for path in ['/base/shared/shared', '/plain/plain'] {
		assert declarative_api_at(raw, 'paths', path, 'get', 'tags').as_array().map(it.str()) == if path != '/plain/plain' {
			['shared']
		} else {
			[typeof(DeclarativeUnannotated{}).name]
		}
	}
	assert '/ignored/ignored' !in declarative_api_at(raw, 'paths').as_map()
	for name in ['base', 'base.currency', 'base.language', 'empty'] {
		tag := declarative_api_at(raw, 'tags').as_array().filter(declarative_api_at(it, 'name').str() == name)[0]
		if name == 'base.currency' {
			assert declarative_api_at(tag, 'parent').str() == 'base'
			assert declarative_api_at(tag, 'summary').str() == 'Currencies'
		} else if name == 'base.language' {
			assert declarative_api_at(tag, 'parent').str() == 'base'
			assert declarative_api_at(tag, 'summary').str() == 'Languages'
		} else {
			assert declarative_api_at(tag, 'name').str() == name
		}
	}
	shared_tag := declarative_api_at(raw, 'tags').as_array().filter(declarative_api_at(it, 'name').str() == 'shared')[0]
	assert declarative_api_at(shared_tag, 'name').str() == 'shared'
	assert 'parent' !in shared_tag.as_map()
	currency_tag := doc.automatic_tags['base.currency'] or { panic('missing currency tag') }
	assert currency_tag.config.controllers == [typeof(DeclarativeCurrency{}).name]
	shared_controllers := (doc.automatic_tags['shared'] or { panic('missing shared tag') }).config.controllers
	assert shared_controllers.len == 2
	assert doc.json(false)! == doc.json(false)!
}

struct DeclarativeConflictRoot {
	Controller
}

@[OpenApiTag('same', 'First', '')]
struct DeclarativeConflictFirst {}

@[OpenApiTag('same', 'Second', '')]
struct DeclarativeConflictSecond {}

@['/first'; get]
fn (app &DeclarativeConflictFirst) first(mut ctx DeclarativeTagContext) Result {
	return ctx.text('first')
}

@['/second'; get]
fn (app &DeclarativeConflictSecond) second(mut ctx DeclarativeTagContext) Result {
	return ctx.text('second')
}

fn test_openapi_automatic_tag_validation() {
	mut conflict := &DeclarativeConflictRoot{}
	mut first := &DeclarativeConflictFirst{}
	mut second := &DeclarativeConflictSecond{}
	conflict.register_controller[DeclarativeConflictFirst, DeclarativeTagContext]('/first',
		mut first)!
	conflict.register_controller[DeclarativeConflictSecond, DeclarativeTagContext]('/second',
		mut second)!
	mut doc := new_openapi(openapi_version: '3.2.0')!
	if _ := doc.add_app[DeclarativeConflictRoot, DeclarativeTagContext](conflict) {
		assert false, 'conflicting automatic tag definitions must be rejected'
	}

	mut missing_parent := &DeclarativeTagRoot{}
	missing_parent.openapi_tag_group(OpenApiTagGroup{
		name:   'child'
		parent: 'missing'
	})
	mut missing_doc := new_openapi(openapi_version: '3.2.0')!
	if _ := missing_doc.add_app[DeclarativeTagRoot, DeclarativeTagContext](missing_parent) {
		assert false, 'a missing automatic tag parent must be rejected'
	}

	mut cycle := &DeclarativeTagRoot{}
	cycle.openapi_tag_group(OpenApiTagGroup{
		name:   'a'
		parent: 'b'
	})
	cycle.openapi_tag_group(OpenApiTagGroup{
		name:   'b'
		parent: 'a'
	})
	mut cycle_doc := new_openapi(openapi_version: '3.2.0')!
	if _ := cycle_doc.add_app[DeclarativeTagRoot, DeclarativeTagContext](cycle) {
		assert false, 'automatic tag parent cycles must be rejected'
	}

	mut version_doc := new_openapi()!
	if _ := version_doc.add_app[DeclarativeTagRoot, DeclarativeTagContext](declarative_tag_app()!) {
		assert false, 'automatic hierarchical tags require OpenAPI 3.2'
	}
}

struct DeclarativeMetadataRequest {
	name string
}

struct DeclarativeMetadataResponse {
	id int
}

struct DeclarativeMetadataRoot {
	Controller
}

struct DeclarativeMetadataController {}

@[OpenApiMetadata]
fn (app &DeclarativeMetadataController) openapi_metadata(mut operation OpenApiHandler) ! {
	match operation.handler {
		'create_item' {
			operation.request[DeclarativeMetadataRequest]()!
			operation.response[DeclarativeMetadataResponse](status: 201)!
		}
		'list_items' {
			operation.operation(
				summary:      'List items'
				operation_id: 'listItems_${operation.path.trim('/').replace('/', '_')}_${operation.method.str().to_lower()}'
			)!
		}
		else {}
	}
}

@['/items'; post; put]
fn (app &DeclarativeMetadataController) create_item(mut ctx DeclarativeTagContext) Result {
	return ctx.text('created')
}

@['/items'; get]
fn (app &DeclarativeMetadataController) list_items(mut ctx DeclarativeTagContext) Result {
	return ctx.text('items')
}

$if declarative_metadata_extra ? {
	@['/extra'; get]
	fn (app &DeclarativeMetadataController) extra(mut ctx DeclarativeTagContext) Result {
		return ctx.text('extra')
	}
}

@[OpenApiSummary('Create named item')]
@['/named'; post]
fn (app &DeclarativeMetadataController) create_named(mut ctx DeclarativeTagContext) Result {
	return ctx.text('named')
}

@['/generated'; get]
fn (app &DeclarativeMetadataController) generated(mut ctx DeclarativeTagContext) Result {
	return ctx.text('generated')
}

fn test_openapi_shared_controller_metadata_follows_routes_and_mounts() {
	mut app := &DeclarativeMetadataRoot{}
	mut first := &DeclarativeMetadataController{}
	mut second := &DeclarativeMetadataController{}
	app.register_controller[DeclarativeMetadataController, DeclarativeTagContext]('/v1', mut first)!
	app.register_controller[DeclarativeMetadataController, DeclarativeTagContext]('/v2', mut second)!
	mut doc := new_openapi(title: 'Declarative metadata')!
	doc.add_app[DeclarativeMetadataRoot, DeclarativeTagContext](app)!
	raw := json2.decode[json2.Any](doc.json(false)!)!
	for prefix in ['/v1', '/v2'] {
		for verb in ['post', 'put'] {
			operation := declarative_api_at(raw, 'paths', prefix + '/items', verb)
			assert declarative_api_at(operation, 'requestBody', 'content', 'application/json',
				'schema', '$ref').str().ends_with('DeclarativeMetadataRequest')
			assert declarative_api_at(operation, 'responses', '201', 'content',
				'application/json', 'schema', '$ref').str().ends_with('DeclarativeMetadataResponse')
		}
		items := declarative_api_at(raw, 'paths', prefix + '/items', 'get')
		assert declarative_api_at(items, 'summary').str() == 'List items'
		assert declarative_api_at(items, 'operationId').str() == 'listItems_${prefix.trim('/').replace('/', '_')}_items_get'
		named := declarative_api_at(raw, 'paths', prefix + '/named', 'post')
		assert declarative_api_at(named, 'summary').str() == 'Create named item'
		assert declarative_api_at(named, 'operationId').str().contains('5fcreate_5fnamed_5fpost')
		generated := declarative_api_at(raw, 'paths', prefix + '/generated', 'get')
		assert declarative_api_at(generated, 'summary').str() == 'generated'
		assert declarative_api_at(generated, 'operationId').str().contains('5fgenerated_5fget')
		$if declarative_metadata_extra ? {
			assert prefix + '/extra' in declarative_api_at(raw, 'paths').as_map()
		} $else {
			assert prefix + '/extra' !in declarative_api_at(raw, 'paths').as_map()
		}
	}
	// A central override still runs after add_app and wins for both mounts.
	doc.operation('/v1/named', .post, summary: 'Central override', operation_id: 'centralNamed')!
	doc.operation('/v2/named', .post, summary: 'Central override', operation_id: 'centralNamed2')!
	overridden := json2.decode[json2.Any](doc.json(false)!)!
	assert declarative_api_at(overridden, 'paths', '/v1/named', 'post', 'summary').str() == 'Central override'
	assert declarative_api_at(overridden, 'paths', '/v1/named', 'post', 'operationId').str() == 'centralNamed'
}

struct DeclarativeNamedMetadataRoot {
	Controller
}

struct DeclarativeNamedMetadataController {}

@[OpenApiSummary('Custom summary'); OpenApiOperationId('customOperation')]
@['/custom'; post]
fn (app &DeclarativeNamedMetadataController) custom(mut ctx DeclarativeTagContext) Result {
	return ctx.text('custom')
}

@[OpenApiSummary(''); OpenApiOperationId('')]
@['/empty'; post]
fn (app &DeclarativeNamedMetadataController) empty(mut ctx DeclarativeTagContext) Result {
	return ctx.text('empty')
}

fn test_openapi_declarative_operation_metadata_overrides_generated_defaults() {
	mut app := &DeclarativeNamedMetadataRoot{}
	mut controller := &DeclarativeNamedMetadataController{}
	app.register_controller[DeclarativeNamedMetadataController, DeclarativeTagContext]('/v1',
		mut controller)!
	mut doc := new_openapi()!
	doc.add_app[DeclarativeNamedMetadataRoot, DeclarativeTagContext](app)!
	raw := json2.decode[json2.Any](doc.json(false)!)!
	custom := declarative_api_at(raw, 'paths', '/v1/custom', 'post')
	assert declarative_api_at(custom, 'summary').str() == 'Custom summary'
	assert declarative_api_at(custom, 'operationId').str() == 'customOperation'
	empty := declarative_api_at(raw, 'paths', '/v1/empty', 'post')
	assert declarative_api_at(empty, 'summary').str() == 'empty'
	assert declarative_api_at(empty, 'operationId').str().contains('5fempty_5fpost')
}

struct DeclarativeDuplicateIdRoot {
	Controller
}

struct DeclarativeDuplicateIdController {}

@[OpenApiOperationId('duplicateOperation')]
@['/items'; post; put]
fn (app &DeclarativeDuplicateIdController) duplicate(mut ctx DeclarativeTagContext) Result {
	return ctx.text('duplicate')
}

fn test_openapi_declarative_operation_ids_are_validated_at_render_time() {
	mut app := &DeclarativeDuplicateIdRoot{}
	mut controller := &DeclarativeDuplicateIdController{}
	app.register_controller[DeclarativeDuplicateIdController, DeclarativeTagContext]('/v1',
		mut controller)!
	mut doc := new_openapi()!
	doc.add_app[DeclarativeDuplicateIdRoot, DeclarativeTagContext](app)!
	if _ := doc.json(false) {
		assert false, 'duplicate explicit operation IDs must be rejected while rendering'
	}
}

struct DeclarativeMultipleMetadataController {}

@[OpenApiMetadata]
fn (app &DeclarativeMultipleMetadataController) first_metadata(mut operation OpenApiHandler) ! {
	_ = operation.handler
}

@[OpenApiMetadata]
fn (app &DeclarativeMultipleMetadataController) second_metadata(mut operation OpenApiHandler) ! {
	_ = operation.handler
}

@['/multiple'; get]
fn (app &DeclarativeMultipleMetadataController) multiple(mut ctx DeclarativeTagContext) Result {
	return ctx.text('multiple')
}

fn test_openapi_rejects_multiple_shared_metadata_methods() {
	mut doc := new_openapi()!
	app := &DeclarativeMultipleMetadataController{}
	if _ := doc.add_app[DeclarativeMultipleMetadataController, DeclarativeTagContext](app) {
		assert false, 'multiple shared metadata methods must be rejected'
	}
}
