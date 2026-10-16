// openapi_test.v - the route table prints itself as an OpenAPI document.
//
// API clients are generated from documents, and documents rot because they
// are written separately from the routes they describe. `openapi_json` reads
// the same `generate_routes` output the dispatcher reads, so the two cannot
// disagree: adding a route or a parameter changes the document, deleting one
// removes it. These tests pin the exact bytes.
module veb

import json2

struct OApiApp {}

struct OApiContext {
	Context
}

@['/']
fn (app &OApiApp) index(mut ctx OApiContext) Result {
	return ctx.text('home')
}

@['/user/:id'; get; post]
fn (app &OApiApp) user(mut ctx OApiContext, id int) Result {
	return ctx.text('user ${id}')
}

@['/files/:path...']
fn (app &OApiApp) files(mut ctx OApiContext, path string) Result {
	return ctx.text(path)
}

fn oapi_spec() string {
	app := OApiApp{}
	return openapi_json[OApiApp, OApiContext](app, 'Shop API', '1.0.0') or {
		assert false, 'openapi_json failed: ${err}'
		return ''
	}
}

fn test_openapi_document_has_version_and_info() {
	spec := oapi_spec()
	assert spec.starts_with('{"openapi":"3.0.0","info":{"title":"Shop API","version":"1.0.0"},"paths":{')
	assert spec.ends_with('}}')
}

fn test_openapi_document_lists_a_static_route() {
	spec := oapi_spec()
	assert spec.contains('"/":{"get":{"parameters":[],"responses":{"200":{"description":"OK"}}}}')
}

fn test_openapi_document_lists_each_method_and_the_path_parameter() {
	spec := oapi_spec()
	assert spec.contains('"/user/{id}":{"get":{"parameters":[{"name":"id","in":"path","required":true,"schema":{"type":"string"}}]')
	assert spec.contains('"post":{"parameters":[{"name":"id","in":"path","required":true,"schema":{"type":"string"}}]')
}

fn test_openapi_document_renders_a_variadic_tail_as_a_path_parameter() {
	spec := oapi_spec()
	assert spec.contains('"/files/{path}":{"get":{"parameters":[{"name":"path","in":"path","required":true')
}

struct OApiDocument {
	openapi string
	paths   map[string]map[string]OApiOperation
}

struct OApiOperation {
	responses map[string]OApiResponse
}

struct OApiResponse {
	description string
}

fn test_openapi_document_is_valid_json() {
	spec := oapi_spec()
	doc := json2.decode[OApiDocument](spec) or {
		assert false, 'openapi document does not decode: ${err}'
		return
	}
	assert doc.openapi == '3.0.0'
	assert '/' in doc.paths
	assert '/user/{id}' in doc.paths
	assert '/files/{path}' in doc.paths
	user_ops := doc.paths['/user/{id}']
	assert 'get' in user_ops
	assert 'post' in user_ops
	assert user_ops['get'].responses['200'].description == 'OK'
}

struct OApiMergeApp {}

struct OApiMergeContext {
	Context
}

@['/users'; get]
fn (app &OApiMergeApp) list_users(mut ctx OApiMergeContext) Result {
	return ctx.text('list')
}

@['/users'; post]
fn (app &OApiMergeApp) create_user(mut ctx OApiMergeContext) Result {
	return ctx.text('create')
}

fn test_openapi_document_merges_handlers_sharing_a_path() {
	app := OApiMergeApp{}
	spec := openapi_json[OApiMergeApp, OApiMergeContext](app, 'Shop API', '1.0.0') or {
		assert false, 'openapi_json failed: ${err}'
		return
	}
	doc := json2.decode[OApiDocument](spec) or {
		assert false, 'openapi document does not decode: ${err}'
		return
	}
	assert '/users' in doc.paths
	users_ops := doc.paths['/users']
	assert 'get' in users_ops
	assert 'post' in users_ops
}
