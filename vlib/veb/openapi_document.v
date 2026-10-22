module veb

import json2
import net.http
import os
import v.vmod

// OpenApiConfig configures an additive, typed OpenAPI document.
@[params]
pub struct OpenApiConfig {
pub:
	title               string
	version             string = '1.0.0'
	version_file        string
	description         string
	openapi_version     string = '3.0.3'
	nullable_options    bool
	schema_names        map[string]string
	security_schemes    map[string]json2.Any
	security            []map[string][]string
	controller_security map[string][]map[string][]string
	// Each status maps to a complete Response Object (description, content, headers, ...).
	default_errors        bool = true
	validation_response   bool
	security_errors       bool = true
	error_responses       map[string]json2.Any
	response_descriptions map[string]string
	servers               []map[string]json2.Any
	tags                  []map[string]json2.Any
	// Wrap successful typed responses in an object with this property, if nonempty.
	success_field string
}

// OpenApiOperation configures one handler without parsing source comments.
@[params]
pub struct OpenApiOperation {
pub:
	summary      string
	description  string
	operation_id string
	tags         []string
	// none inherits controller/document security; an empty array explicitly disables it.
	security     ?[]map[string][]string
	path_schemas map[string]OpenApiSchema
	parameters   []map[string]json2.Any
}

// OpenApiRequest configures the location and media type of a typed request DTO.
@[params]
pub struct OpenApiRequest {
pub:
	// none selects query for GET/DELETE/HEAD and a body for other methods.
	query       ?bool
	media_type  string = 'application/json'
	required    bool   = true
	description string
}

// OpenApiResponse configures the status, media type and description of a typed response.
@[params]
pub struct OpenApiResponse {
pub:
	status      int = 200
	description string
	media_type  string = 'application/json'
	// none inherits success_field for 2xx only; an empty string disables wrapping.
	envelope ?string
}

// OpenApiDocument collects dispatcher routes and explicitly configured DTO metadata.
pub struct OpenApiDocument {
	config OpenApiConfig
mut:
	paths             map[string]map[string]map[string]json2.Any
	schemas           map[string]json2.Any
	success_overrides map[string]bool
}

// new_openapi creates a document, optionally reading its API version from a v.mod file.
pub fn new_openapi(config OpenApiConfig) !OpenApiDocument {
	version := if config.version_file != '' {
		vmod.from_file(config.version_file)!.version
	} else {
		config.version
	}
	patch := config.openapi_version.all_after('3.0.')
	if !config.openapi_version.starts_with('3.0.') || patch == '' || patch.bytes().any(!it.is_digit()) {
		return error('OpenAPI version must be 3.0.x')
	}
	return OpenApiDocument{ config: OpenApiConfig{ ...config, version: version } }
}

// add_app includes root routes and all recursively mounted controllers.
pub fn (mut doc OpenApiDocument) add_app[A, X](app &A) ! {
	for route in route_info[A, X](app)! {
		path := openapi_path_of(route.path)
		for method in route.methods {
			verb := method.str().to_lower()
			if verb !in ['get', 'put', 'post', 'delete', 'options', 'head', 'patch', 'trace'] {
				return error('unsupported OpenAPI method: ${method}')
			}
			mut item := doc.paths[path]
			if verb in item {
				return error('duplicate OpenAPI operation: ${verb.to_upper()} ${path}')
			}
			mut params := []json2.Any{}
			mut path_index := 0
			for word in route.path.split('/') {
				if word.starts_with(':') {
					name := word.trim_left(':').trim_right('.')
					index := path_index
					path_index++
					mut param := map[string]json2.Any{
						'name':     json2.Any(name)
						'in':       json2.Any('path')
						'required': json2.Any(true)
						'schema':   json2.Any(route.parameter_schema(index))
					}
					if word.ends_with('...') {
						param['x-veb-variadic'] = true
					}
					params << param
				}
			}
			if path_index == 0 && method == .get {
				for i, name in route.parameter_names {
					params << map[string]json2.Any{
						'name':     json2.Any(name)
						'in':       json2.Any('query')
						'required': json2.Any(route.requires_parameter(i))
						'schema':   json2.Any(route.parameter_schema(i))
					}
				}
			}
			mut responses := map[string]json2.Any{}
			if doc.config.default_errors {
				responses['400'] = {
					'description': json2.Any(doc.response_description(400))
				}
				responses['500'] = {
					'description': json2.Any(doc.response_description(500))
				}
			}
			for code, response in doc.config.error_responses { responses[code] = response }
			responses['200'] = {
				'description': json2.Any(doc.response_description(200))
			}
			mut operation := map[string]json2.Any{
				'summary':     json2.Any(route.name)
				'operationId': json2.Any(openapi_id(route.controller + '_' + route.name + '_' + verb + '_' + path))
				'tags':        json2.Any([json2.Any(route.controller)])
				'parameters':  json2.Any(params)
				'responses':   json2.Any(responses)
			}
			if path_index == 0 && method == .post && route.parameter_names.len > 0 {
				mut properties := map[string]json2.Any{}
				mut required := []json2.Any{}
				for i, name in route.parameter_names {
					properties[name] = json2.Any(route.parameter_schema(i))
					if route.requires_parameter(i) { required << name }
				}
				mut schema := OpenApiSchema({
					'type':       json2.Any('object')
					'properties': json2.Any(properties)
				})
				if required.len > 0 { schema['required'] = required }
				mut content := map[string]json2.Any{}
				for media in ['application/x-www-form-urlencoded', 'multipart/form-data',
					'application/json'] {
					content[media] = {
						'schema': json2.Any(schema)
					}
				}
				operation['requestBody'] = {
					'required': json2.Any(required.len > 0)
					'content':  json2.Any(content)
				}
			}
			if route.host != '' {
				operation['x-veb-host'] = route.host
			}
			if security := doc.config.controller_security[route.controller] {
				operation['security'] = openapi_security(security)
			}
			item[verb] = operation
			doc.paths[path] = item
		}
	}
}

// operation applies structured metadata to an existing operation.
pub fn (mut doc OpenApiDocument) operation(path string, method http.Method, options OpenApiOperation) ! {
	key := openapi_path_of(openapi_join_path('', path))
	verb := method.str().to_lower()
	mut item := (doc.paths[key] or { return error('unknown OpenAPI path: ${key}') }).clone()
	mut operation := (item[verb] or { return error('unknown OpenAPI method: ${verb} ${key}') }).clone()
	if options.summary != '' { operation['summary'] = options.summary }
	if options.description != '' { operation['description'] = options.description }
	if options.operation_id != '' { operation['operationId'] = options.operation_id }
	if options.tags.len > 0 {
		operation['tags'] = options.tags.map(json2.Any(it))
	}
	if security := options.security {
		operation['security'] = openapi_security(security)
	}
	mut parameters := (operation['parameters'] or { json2.Any([]json2.Any{}) }).as_array().clone()
	for i, parameter in parameters {
		mut param := parameter.as_map().clone()
		if schema := options.path_schemas[(param['name'] or { json2.Any(json2.null) }).str()] {
			param['schema'] = schema
			parameters[i] = param
		}
	}
	for parameter in options.parameters { parameters << parameter }
	operation['parameters'] = parameters
	item[verb] = operation
	doc.paths[key] = item
}

// request adds a reflected JSON body or query parameters to an existing operation.
// Call twice with explicit query values to document both a body and query fields.
pub fn (mut doc OpenApiDocument) request[T](path string, method http.Method, options OpenApiRequest) ! {
	key := openapi_path_of(openapi_join_path('', path))
	verb := method.str().to_lower()
	mut item := (doc.paths[key] or { return error('unknown OpenAPI path: ${key}') }).clone()
	mut operation := (item[verb] or { return error('unknown OpenAPI method: ${verb} ${key}') }).clone()
	schema := doc.schema[T]()
	query := options.query or { method in [.get, .delete, .head] }
	if query {
		mut parameters := (operation['parameters'] or { json2.Any([]json2.Any{}) }).as_array().clone()
		$if T is $struct {
			$for field in T.fields {
				$if field.typ is string || field.typ is int || field.typ is bool {
					parameters << map[string]json2.Any{
						'name':     json2.Any(field.name)
						'in':       json2.Any('query')
						'required': json2.Any('required' in field.attrs)
						'schema':   json2.Any(doc.schema_field($zero(field.typ), field.attrs))
					}
				}
			}
		} $else {
			return error('OpenAPI query DTO must be a struct')
		}

		operation['parameters'] = parameters
	} else {
		mut body := map[string]json2.Any{
			'required': json2.Any(options.required)
			'content':  json2.Any({
				options.media_type: json2.Any({
					'schema': json2.Any(schema)
				})
			})
		}
		if options.description != '' { body['description'] = options.description }
		operation['requestBody'] = body
		if doc.config.validation_response {
			mut responses := (operation['responses'] or { json2.Any(json2.null) }).as_map().clone()
			if '422' !in responses {
				responses['422'] = {
					'description': json2.Any(doc.response_description(422))
				}
			}
			operation['responses'] = responses
		}
	}
	item[verb] = operation
	doc.paths[key] = item
}

// response adds a typed response; only successful responses inherit success_field wrapping.
pub fn (mut doc OpenApiDocument) response[T](path string, method http.Method, options OpenApiResponse) ! {
	mut schema := doc.schema[T]()
	field := options.envelope or {
		if options.status >= 200 && options.status < 300 {
			doc.config.success_field
		} else {
			''
		}
	}
	if field != '' {
		schema = {
			'type':       json2.Any('object')
			'properties': json2.Any({
				field: json2.Any(schema)
			})
			'required':   json2.Any([json2.Any(field)])
		}
	}
	description := if options.description != '' {
		options.description
	} else {
		doc.response_description(options.status)
	}
	doc.raw_response(path, method, options.status, {
		'description': json2.Any(description)
		'content':     json2.Any({
			options.media_type: json2.Any({
				'schema': json2.Any(schema)
			})
		})
	})!
}

// raw_response adds a complete Response Object, including custom headers and error envelopes.
pub fn (mut doc OpenApiDocument) raw_response(path string, method http.Method, status int, response map[string]json2.Any) ! {
	if status < 100 || status > 599 { return error('invalid OpenAPI response status: ${status}') }
	if 'description' !in response { return error('OpenAPI responses require a description') }
	key := openapi_path_of(openapi_join_path('', path))
	verb := method.str().to_lower()
	mut item := (doc.paths[key] or { return error('unknown OpenAPI path: ${key}') }).clone()
	mut operation := (item[verb] or { return error('unknown OpenAPI method: ${verb} ${key}') }).clone()
	mut responses := (operation['responses'] or { json2.Any(map[string]json2.Any{}) }).as_map().clone()
	operation_key := key + ' ' + verb
	if status >= 200 && status < 300 && !doc.success_overrides[operation_key] {
		if status != 200 { responses.delete('200') }
		doc.success_overrides[operation_key] = true
	}
	responses[status.str()] = response
	operation['responses'] = responses
	item[verb] = operation
	doc.paths[key] = item
}

// json renders deterministic JSON and rejects duplicate operation IDs or undefined security schemes.
pub fn (doc &OpenApiDocument) json(pretty bool) !string {
	for name, _ in doc.schemas {
		if name == '' || name.bytes().any(!it.is_alnum() && it !in [`_`, `-`, `.`]) {
			return error('invalid OpenAPI component name: ${name}; configure schema_names')
		}
	}
	mut paths := map[string]json2.Any{}
	mut ids := map[string]bool{}
	mut tags := map[string]json2.Any{}
	for tag in doc.config.tags { tags[(tag['name'] or { json2.Any(json2.null) }).str()] = tag }
	for path, methods in doc.paths {
		mut item := map[string]json2.Any{}
		for method, stored_operation in methods {
			mut operation := stored_operation.clone()
			id := (operation['operationId'] or { json2.Any(json2.null) }).str()
			if id in ids { return error('duplicate OpenAPI operationId: ${id}') }
			ids[id] = true
			for tag in (operation['tags'] or { json2.Any(json2.null) }).as_array() {
				name := tag.str()
				if name !in tags {
					tags[name] = {
						'name': json2.Any(name)
					}
				}
			}
			security := operation['security'] or { json2.Any(openapi_security(doc.config.security)) }
			doc.check_security(security)!
			if doc.config.security_errors && security.as_array().any(it.as_map().len > 0) {
				mut responses := (operation['responses'] or { json2.Any(json2.null) }).as_map().clone()
				if '401' !in responses {
					responses['401'] = {
						'description': json2.Any(doc.response_description(401))
					}
				}
				if '403' !in responses {
					responses['403'] = {
						'description': json2.Any(doc.response_description(403))
					}
				}
				operation['responses'] = responses
			}
			item[method] = operation
		}
		paths[path] = item
	}
	doc.check_security(openapi_security(doc.config.security))!
	mut info := map[string]json2.Any{
		'title':   json2.Any(doc.config.title)
		'version': json2.Any(doc.config.version)
	}
	if doc.config.description != '' { info['description'] = doc.config.description }
	mut components := map[string]json2.Any{
		'schemas': json2.Any(doc.schemas)
	}
	if doc.config.security_schemes.len > 0 {
		components['securitySchemes'] = doc.config.security_schemes
	}
	mut names := tags.keys()
	names.sort()
	mut sorted_tags := []json2.Any{}
	for name in names { sorted_tags << (tags[name] or { json2.Any(json2.null) }) }
	mut result := map[string]json2.Any{
		'openapi':    json2.Any(doc.config.openapi_version)
		'info':       json2.Any(info)
		'paths':      json2.Any(paths)
		'components': json2.Any(components)
		'tags':       json2.Any(sorted_tags)
	}
	if doc.config.security.len > 0 { result['security'] = openapi_security(doc.config.security) }
	if doc.config.servers.len > 0 { result['servers'] = doc.config.servers.map(json2.Any(it)) }
	return json2.encode(result, prettify: pretty, legacy_layout: pretty)
}

// write saves a rendered document to disk.
pub fn (doc &OpenApiDocument) write(path string, pretty bool) ! {
	os.write_file(path, doc.json(pretty)!)!
}

fn (doc &OpenApiDocument) check_security(security json2.Any) ! {
	for group in security.as_array() {
		for name, _ in group.as_map() {
			if name !in doc.config.security_schemes {
				return error('undefined OpenAPI security scheme: ${name}')
			}
		}
	}
}

fn (doc &OpenApiDocument) resolve_schema(schema OpenApiSchema) OpenApiSchema {
	if reference := schema['$ref'] {
		name := reference.str().all_after('#/components/schemas/').replace('~1', '/').replace('~0', '~')
		return (doc.schemas[name] or { json2.Any(map[string]json2.Any{}) }).as_map()
	}
	return schema
}

fn openapi_security(security []map[string][]string) []json2.Any {
	mut groups := []json2.Any{}
	for group in security {
		mut object := map[string]json2.Any{}
		for name, scopes in group { object[name] = scopes.map(json2.Any(it)) }
		groups << object
	}
	return groups
}

fn openapi_id(value string) string {
	mut result := ''
	for c in value.bytes() {
		// Escape underscores as well so a literal escape sequence cannot collide.
		result += if c.is_alnum() { c.ascii_str() } else { '_' + c.hex() }
	}
	return result
}

// serve sends the specification from a veb handler with the JSON content type.
pub fn (doc &OpenApiDocument) serve(mut ctx Context, pretty bool) !Result {
	return ctx.send_response_to_client('application/json', doc.json(pretty)!)
}

fn (doc &OpenApiDocument) response_description(status int) string {
	return doc.config.response_descriptions[status.str()] or { http.status_from_int(status).str() }
}
