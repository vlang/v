# Typed OpenAPI documents

`openapi_json[A, X](app, title, version)!` keeps its existing minimal output. For DTOs,
controllers and authentication contracts, build an `OpenApiDocument` instead:

```v ignore
mut doc := veb.new_openapi(title: 'Users', version: '1.0.0')!
doc.add_app[App, Context](app)!
doc.operation('/users', .post, summary: 'Create a user', tags: ['users'])!
doc.request[CreateUser]('/users', .post)!
doc.response[User]('/users', .post, status: 201)!
doc.write('openapi.json', true)!
```

See [the complete application](../../examples/veb/openapi/main.v). Run it with `--write`
to export a document, or without arguments to serve `/openapi.json` and the sample API.

## Routes and metadata

`route_info[A, X](app)!` exposes the dispatcher metadata, including recursively mounted
controllers, selected compile-time routes, methods, typed parameters and host restrictions.
Controller metadata is captured when registered, just like dispatcher routes. Register all
nested controllers before registering their parent. Paths are normalized and `:id` and
`:path...` become OpenAPI templates. Variadic parameters retain `x-veb-variadic`; host
restrictions appear as `x-veb-host`. OpenAPI cannot represent two operations with the same
path and method on different hosts; duplicate operations return an error.

Use `operation(path, method, ...)` for summaries, multiline descriptions, tags, explicit
operation IDs, path schemas and additional parameter objects. Generated IDs include the
controller, handler, method and path, escaping punctuation as hexadecimal bytes so distinct
mount paths retain distinct IDs. Output rejects duplicate IDs. Structured metadata
replaces application-specific source-comment parsing: migrate each `@summary`,
`@description`, `@tag` and `@security` into `OpenApiOperation`, and each `@response` into
`response[T]` or `raw_response`. Source comments and decorative dividers are never parsed.

## Requests and schemas

`request[T]` documents a required JSON body for POST, PUT and PATCH, and query parameters
for GET, DELETE and HEAD. Set `query`, `required`, `description` or `media_type` explicitly
to override this. Call it for both locations when a handler binds a query and a body.

`schema[T]()` registers referenced component schemas recursively. Reflection handles
primitives, integer widths, floats, timestamps, options, arrays, maps, enums, sum types
and named structs. `json2.Any` permits any JSON value. Recursive structs use references.
JSON field renames and `json: '-'` are honored. Nonoptional fields are required, including
fields marked `omitempty`. Optional fields are omitted from `required`; set `nullable_options`
to also mark their schemas nullable.

Field attributes `description: '...'` and `example: '...'` supply schema metadata. Examples
are decoded as JSON, including objects and arrays; invalid JSON becomes a string example.
Use `@[example: '"true"']` for the string `true`, or `@[example: 'true']` for the boolean.
Default component names use the reflected module-qualified type name. Map these names
with `schema_names` (names allow letters, digits, `.`, `_` and `-`), or call
`custom_schema(name, schema)` to replace a component with a
project-specific schema. `OpenApiSchema` is a map of `json2.Any` values, so validation
constraints and project-specific envelopes do not require new generator APIs.

## Responses and security

`response[T]` accepts a status, description, media type and optional envelope field.
A non-200 success replaces the generated default 200 response. `success_field` wraps only
2xx responses; errors remain direct by default. Set `envelope: ''` to disable a wrapper.
For envelopes with additional fields, headers, or empty 204 responses, supply a complete
Response Object through `raw_response`. `error_responses` configures shared response
objects; default 400 and 500 descriptions can be disabled with `default_errors: false`.
`response_descriptions` overrides generated descriptions by status code.
`validation_response: true` adds a default 422 when a request body is documented.

Configure `security_schemes` using OpenAPI Security Scheme Objects. For a bearer token,
use `type: http`, `scheme: bearer`, and optionally `bearerFormat: JWT`. Requirement groups
are maps from scheme names to scope arrays: entries in one map are ANDed, separate maps
are alternatives. `security` sets the document default; `controller_security` overrides it
by reflected controller name. An operation with `security: []map[string][]string{}`
explicitly allows anonymous access. Secured operations receive default 401 and 403
responses unless `security_errors: false`. Undefined scheme names return an error.

## Rendering and serving

The default format is OpenAPI 3.0.3; `openapi_version` accepts other 3.0.x versions.
Configure `title`, `description`, `version`, `tags` and `servers`, or set `version_file`
to read a v.mod manifest. JSON keys are sorted, strings escaped and operation IDs checked.
`json(pretty)!` renders, `write(path, pretty)!` saves, and `serve(mut ctx, pretty)!` sends
an `application/json` response from a veb handler. Viewer integrations can consume the
same endpoint without coupling the generator to a particular UI.
