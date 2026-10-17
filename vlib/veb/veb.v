// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module veb

import io
import net
import net.http
import net.urllib
import os
import strconv
import strings
import time

// A type which doesn't get filtered inside templates
pub type RawHtml = string

// A dummy structure that returns from routes to indicate that you actually sent something to a user
@[noinit]
pub struct Result {}

// no_result does nothing, but returns `veb.Result`. Only use it when you are sure
// a response will be send over the connection, or in combination with `Context.takeover_conn`
pub fn no_result() Result {
	return Result{}
}

struct Route {
	methods         []http.Method
	path            string
	words           []string
	host            string
	parameter_names []string
mut:
	middlewares       []RouteMiddleware
	after_middlewares []RouteMiddleware
}

fn route_path_words(path string) []string {
	if path.len == 0 || path == '/' {
		return []string{}
	}
	return path.split('/').filter(it != '')
}

// Parse and assemble route metadata once, outside controller specializations.
fn insert_route_metadata(mut routes map[string]Route, name string, attrs []string, parameter_names []string) ! {
	http_methods, route_path, host := parse_attrs(name, attrs) or {
		return error('error parsing method attributes: ${err}')
	}
	routes[name] = Route{
		methods: http_methods
		path: route_path
		words: route_path_words(route_path)
		host: host
		parameter_names: parameter_names
	}
}

// Generate route structs for an app
fn generate_routes[A, X](app &A) !map[string]Route {
	mut routes := map[string]Route{}
	$for method in A.methods {
		$if method.return_type is Result {
			mut parameter_names := []string{}
			$if method.args.len > 1 {
				for param in method.args[1..] {
					parameter_names << param.name
				}
			}
			insert_route_metadata(mut routes, method.name, method.attrs, parameter_names)!
		} $else {
			// If we have route attributes, but the wrong return type, return an error
			if has_route_attributes(method.attrs) {
				return error('method `${method.name}` at `${method.location}` has route attributes but invalid return type. Handler methods must return `veb.Result`, not `!veb.Result` or other types')
			}
		}
	}
	$if A is MiddlewareApp {
		for name, initial in routes {
			mut route := initial
			route.middlewares = app_route_handlers(app, route.path)
			route.after_middlewares = app_route_handlers_after(app, route.path)
			routes[name] = route
		}
	}
	return routes
}

// run - start a new veb server, listening to all available addresses, at the specified `port`
pub fn run[A, X](mut global_app A, port int) {
	run_at[A, X](mut global_app, host: '', port: port, family: .ip6) or { panic(err.msg()) }
}

struct SslRequestParams {
	global_app                voidptr
	controllers_sorted        []&ControllerPath
	routes                    &map[string]Route
	benchmark_page_generation bool
	max_request_buffer_size   int
	max_request_body_size     int
}

// render_request dispatches one request without starting a listener and returns
// the completed base context. Read the response from `ctx.res`; fields added to
// the application's context type are only available inside its handlers.
// Static file responses retain `return_type` and `return_file`, as on the server.
// X must embed veb.Context. Context, route, and controller configuration errors
// are returned to the caller.
pub fn render_request[A, X](mut app A, req http.Request) !&Context {
	$for field in X.fields {
		$if field.name == 'Context' && field.is_embed {
			routes := generate_routes[A, X](app)!
			controllers_sorted := check_duplicate_routes_in_controllers[A](app, routes)!
			params := RequestParams{
				controllers_sorted: controllers_sorted
				routes:             &routes
			}
			return handle_request_and_route[A, X](mut app, req, -1, params)
		}
	}
	return error('the application context type must embed veb.Context')
}

fn ssl_enabled(params RunParams) bool {
	return params.ssl_config.cert != '' || params.ssl_config.cert_key != ''
}

fn server_protocol(params RunParams) string {
	if ssl_enabled(params) {
		return 'https'
	}
	return 'http'
}

fn startup_host(params RunParams) string {
	if params.host == '' {
		return 'localhost'
	}
	// Bracket a bare IPv6 literal so the startup URL is valid (e.g. `[::1]`).
	if params.host.contains(':') && !params.host.starts_with('[') {
		return '[${params.host}]'
	}
	return params.host
}

fn listen_addr(params RunParams) string {
	if params.host == '' {
		return ':${params.port}'
	}
	return '${params.host}:${params.port}'
}

// request_read_error_response returns the response for a request that
// `read_request_from_buffered_reader` could not read.
fn request_read_error_response(err IError) http.Response {
	if err is http.HeaderLimitError {
		return http_431
	}
	if err is RequestBodyLimitError {
		return http_413_close
	}
	return http_400
}

// RequestBodyLimitError is the error for a request whose body has more than
// `RunParams.max_request_body_size` bytes.
struct RequestBodyLimitError {
	Error
}

// msg returns the description of the error.
fn (err RequestBodyLimitError) msg() string {
	return 'the request body is too large'
}

// read_request_from_buffered_reader reads one request. It returns a `RequestBodyLimitError`
// for a body of more than `max_request_body_size` bytes (0 means no limit), and does not
// read the rest of that body.
fn read_request_from_buffered_reader(mut reader io.BufferedReader, max_request_body_size int) !http.Request {
	mut req := http.parse_request_head(mut reader)!
	length_values := req.header.values(.content_length)
	if length_values.len > 0 && req.header.contains(.transfer_encoding) {
		return error('conflicting request body framing')
	}
	mut content_length_i := 0
	for i, value in length_values {
		length := parse_request_content_length(value)!
		if i > 0 && length != content_length_i {
			return error('conflicting content lengths')
		}
		content_length_i = length
	}
	if transfer_encoding_is_chunked(req.header) {
		req.data = read_chunked_request_body(mut reader, max_request_body_size)!
		return req
	}
	if content_length_i <= 0 {
		return req
	}
	if max_request_body_size > 0 && content_length_i > max_request_body_size {
		return RequestBodyLimitError{}
	}
	mut body := []u8{len: content_length_i}
	read_exact_bytes(mut reader, mut body)!
	req.data = body.bytestr()
	return req
}

// parse_request_content_length rejects prefixes, signs, junk, and integer overflow.
fn parse_request_content_length(value string) !int {
	if value.len == 0 {
		return error('invalid content length')
	}
	mut length := 0
	for byte in value {
		if byte < `0` || byte > `9` {
			return error('invalid content length')
		}
		digit := int(byte - `0`)
		if length > (max_int - digit) / 10 {
			return error('content length too large')
		}
		length = length * 10 + digit
	}
	return length
}

fn transfer_encoding_is_chunked(header http.Header) bool {
	transfer_encoding := header.get(.transfer_encoding) or { return false }
	for word in transfer_encoding.to_lower().split(',') {
		if word.trim_space() == 'chunked' {
			return true
		}
	}
	return false
}

fn read_chunked_request_body(mut reader io.BufferedReader, max_request_body_size int) !string {
	mut sb := strings.new_builder(1024)
	for {
		mut chunk_size_line := reader.read_line()!
		if semicolon_idx := chunk_size_line.index(';') {
			chunk_size_line = chunk_size_line[..semicolon_idx]
		}
		chunk_size_line = chunk_size_line.trim_space()
		if chunk_size_line.len == 0 {
			return error('invalid chunk size line')
		}
		for byte in chunk_size_line {
			if !((byte >= `0` && byte <= `9`) || (byte >= `a` && byte <= `f`)
				|| (byte >= `A` && byte <= `F`)) {
				return error('invalid chunk size line')
			}
		}
		chunk_size_u64 := strconv.parse_uint(chunk_size_line, 16, 64) or {
			return error('invalid chunk size line')
		}
		if chunk_size_u64 > u64(max_int) {
			return error('chunk size too large')
		}
		chunk_size := int(chunk_size_u64)
		if chunk_size == 0 {
			for {
				trailer := reader.read_line()!
				if trailer == '' {
					return sb.str()
				}
			}
		}
		if max_request_body_size > 0 && chunk_size > max_request_body_size - sb.len {
			return RequestBodyLimitError{}
		}
		mut chunk := []u8{len: chunk_size}
		read_exact_bytes(mut reader, mut chunk)!
		sb.write(chunk)!
		mut delimiter := []u8{len: 2}
		read_exact_bytes(mut reader, mut delimiter)!
		if delimiter[0] != `\r` || delimiter[1] != `\n` {
			return error('invalid chunk delimiter')
		}
	}
	return error('invalid chunked body')
}

fn read_exact_bytes(mut reader io.BufferedReader, mut buf []u8) ! {
	mut offset := 0
	for offset < buf.len {
		n := reader.read(mut buf[offset..]) or {
			if err is io.Eof {
				return error('unexpected end of request body')
			}
			return err
		}
		if n <= 0 {
			return error('unexpected end of request body')
		}
		offset += n
	}
}

fn handle_ssl_request[A, X](req http.Request, params &SslRequestParams) ?&Context {
	mut global_app := unsafe { &A(params.global_app) }
	page_gen_start := time.ticks()
	mut url := urllib.parse(req.url) or {
		eprintln('[veb] error parsing path "${req.url}": ${err}')
		return none
	}
	query := parse_query_from_url(url)
	form, files := parse_form_from_request(req) or {
		eprintln('[veb] error parsing form: ${err.msg()}')
		return none
	}
	host_with_port := req.header.get(.host) or { '' }
	host := request_host_name(host_with_port)
	mut ctx := &Context{
		req:            req
		page_gen_start: page_gen_start
		query:          query
		form:           form
		files:          files
	}
	ctx.client_wants_to_close = request_has_connection_close(req)
	mut user_context := X{
		Context: ctx
	}
	$if A is StaticApp {
		ctx.custom_mime_types_ref = unsafe { &global_app.static_mime_types }
		if serve_if_static[X](static_handler_config(global_app.static_files,
			global_app.static_mime_types, global_app.static_hosts, global_app.static_prefixes,
			global_app.enable_static_gzip, global_app.enable_static_zstd,
			global_app.enable_static_compression, global_app.static_compression_max_size,
			global_app.static_compression_mime_types, global_app.enable_markdown_negotiation), mut
			user_context, url, host)
		{
			// Preserve the handled context on the heap before the stack-local user context goes away.
			ctx.preserve_for_response_writer(user_context.Context)
			return ctx
		}
	}
	$if A is ControllerInterface {
		if completed_context := handle_controllers[X](params.controllers_sorted, ctx, mut url, host) {
			return completed_context
		}
	}
	handle_route[A, X](mut global_app, mut user_context, url, host, params.routes)
	// Preserve the handled context on the heap before the stack-local user context goes away.
	ctx.preserve_for_response_writer(user_context.Context)
	return ctx
}

fn request_has_connection_close(req http.Request) bool {
	conn := req.header.get(.connection) or { return false }
	return ascii_eq_ignore_case(conn, 'close')
}

fn request_host_name(host_with_port string) string {
	if host_with_port.len == 0 {
		return ''
	}
	if host_with_port[0] == `[` {
		end := host_with_port.index_u8(`]`)
		if end > 0 {
			return host_with_port[1..end]
		}
		return host_with_port
	}
	colon := host_with_port.last_index_u8(`:`)
	if colon == -1 {
		return host_with_port
	}
	for i in 0 .. colon {
		if host_with_port[i] == `:` {
			return host_with_port
		}
	}
	return host_with_port[..colon]
}

fn ascii_eq_ignore_case(a string, b string) bool {
	if a.len != b.len {
		return false
	}
	for i in 0 .. a.len {
		mut ca := a[i]
		if ca >= `A` && ca <= `Z` {
			ca += 32
		}
		mut cb := b[i]
		if cb >= `A` && cb <= `Z` {
			cb += 32
		}
		if ca != cb {
			return false
		}
	}
	return true
}

fn should_close_connection(req http.Request, resp http.Response, client_wants_to_close bool) bool {
	if client_wants_to_close {
		return true
	}
	if resp_conn := resp.header.get(.connection) {
		if ascii_eq_ignore_case(resp_conn, 'close') {
			return true
		}
		if ascii_eq_ignore_case(resp_conn, 'keep-alive') {
			return false
		}
	}
	if req_conn := req.header.get(.connection) {
		if ascii_eq_ignore_case(req_conn, 'close') {
			return true
		}
		if ascii_eq_ignore_case(req_conn, 'keep-alive') {
			return false
		}
	}
	return req.version != .v1_1
}

interface BeforeAcceptApp {
mut:
	before_accept_loop()
}

interface HasBeforeRequestOnContext {
mut:
	before_request()
}

fn handle_route[A, X](mut app A, mut user_context X, url urllib.URL, host string, routes &map[string]Route) {
	mut route := Route{}
	mut middleware_has_sent_response := false
	mut not_found := false

	defer {
		// A panicking handler must not take the server down with it. Nothing between
		// here and the accept loop recovers, so without this the process exits.
		// `recover()` only stops the panic when called directly in a defer block, and
		// V does not allow `return` in one, so the recovered case answers 500 here and
		// skips the after-middleware through the else branch.
		if msg := recover() {
			eprintln('[veb] handler for route "${url.path}" panicked: ${msg}')
			user_context.server_error('Internal Server Error')
		} else {
			// execute middleware functions after veb is done and before the response is send
			mut was_done := true
			$if A is MiddlewareApp {
				if !not_found && !middleware_has_sent_response {
					// if the middleware doesn't send an alternate response, but only changes the
					// response object we only have to check if the `done` was previously set to true
					was_done = user_context.Context.done
					// reset `done` so the middleware functions can return a different response
					// 1 time only, since the `done` guard is still present in
					// `Context.send_response_to_client`
					user_context.Context.done = false

					// no need to check the result of `validate_middleware`, since a response has to be sent
					// anyhow. This function makes sure no further middleware is executed.
					validate_middleware[X](mut user_context, app_global_handlers_after(app))
					// skip route-specific after-middleware if global already sent a response
					if !user_context.Context.done {
						validate_middleware[X](mut user_context, get_handlers_for_method(route.after_middlewares,
							user_context.Context.req.method))
						// Preserve an after-middleware response, or restore the handler response state.
						user_context.Context.done = user_context.Context.done || was_done
					}
				}
			}
			// send only the headers, because if the response body is too big, TcpConn code will
			// actually block, because it has to wait for the socket to become ready to write. veb
			// will handle this case.
			if !was_done && !user_context.Context.done && user_context.Context.takeover_mode == .none {
				eprintln('[veb] handler for route "${url.path}" does not send any data!')
				// send response anyway so the connection won't block
				// fast_send_resp_header(mut user_context.conn, user_context.res) or {}
			} else if user_context.Context.takeover_mode == .none {
				// fast_send_resp_header(mut user_context.conn, user_context.res) or {}
			}
			// Context.takeover_mode is set, so the user must send a response.
		}
		// Free the handler slots a throttle took for this request on every way
		// out, including the 404 and refusing-middleware paths that skip the
		// after-middleware above.
		if user_context.Context.throttle_leases.len > 0 {
			user_context.Context.release_throttle_leases()
		}
	}

	is_root_path := url.path.len == 0 || url.path == '/'

	$if veb_livereload ? {
		if url.path.starts_with('/veb_livereload/') {
			if url.path.ends_with('/events') {
				user_context.handle_veb_livereload_events()
				return
			}
			$if veb_livereload_polling ? {
				if url.path.ends_with('current') {
					user_context.handle_veb_livereload_current()
					return
				}
			}
			if url.path.ends_with('script.js') {
				user_context.handle_veb_livereload_script()
				return
			}
		}
	}

	// first execute before_request
	$if X is HasBeforeRequestOnContext {
		user_context.before_request()
	}
	// user_context.before_request()
	if user_context.Context.done {
		return
	}
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'veb before_request done') }
	}

	// then execute global middleware functions
	$if A is MiddlewareApp {
		if validate_middleware[X](mut user_context, app_global_handlers(app)) == false {
			middleware_has_sent_response = true
			return
		}
	}

	$if A is StaticApp {
		should_check_static := !is_root_path || app.enable_markdown_negotiation
			|| app.static_files['/'] != '' || app.static_files['/index.html'] != ''
			|| app.static_files['/index.htm'] != ''
		if should_check_static {
			if serve_if_static[X](static_handler_config(app.static_files, app.static_mime_types,
				app.static_hosts, app.static_prefixes, app.enable_static_gzip,
				app.enable_static_zstd, app.enable_static_compression,
				app.static_compression_max_size, app.static_compression_mime_types,
				app.enable_markdown_negotiation), mut user_context, url, host)
			{
				// successfully served a static file
				return
			}
		}
	}
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'veb route static checked') }
	}
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'veb before route match') }
	}

	if matched := select_route(routes, url.path, host, user_context.Context.req.method) {
		route = matched.route
		$if A is MiddlewareApp {
			if !validate_middleware[X](mut user_context, get_handlers_for_method(route.middlewares,
				user_context.Context.req.method)) {
				middleware_has_sent_response = true
				return
			}
		}
		mut args := matched.params
		if matched.from_data {
			data := if user_context.Context.req.method == .get {
				user_context.Context.query
			} else {
				user_context.Context.form
			}
			for name in route.parameter_names {
				args << data[name]
			}
		}
		$for method in A.methods {
			$if method.return_type is Result {
				if method.name == matched.name {
					$if method.args.len > 1 {
						if !matched.from_data || user_context.Context.req.method in [
							.get,
							.post,
						] {
							if !matched.from_data && args.len + 1 != method.args.len {
								eprintln('[veb] warning: uneven parameters count (${method.args.len}) in `${method.name}`, compared to the veb route `${method.attrs}` (${args.len})')
							}
							$if trace_prealloc ? {
								unsafe { prealloc_scope_checkpoint(c'veb before route handler') }
							}
							app.$method(mut user_context, ...args)
							$if trace_prealloc ? {
								unsafe { prealloc_scope_checkpoint(c'veb after route handler') }
							}
						}
					} $else {
						$if trace_prealloc ? {
							unsafe { prealloc_scope_checkpoint(c'veb before route handler') }
						}
						app.$method(mut user_context)
						$if trace_prealloc ? {
							unsafe { prealloc_scope_checkpoint(c'veb after route handler') }
						}
					}
					return
				}
			}
		}
	}
	// return 404
	user_context.not_found()
	not_found = true
	return
}

// RouteMatch holds runtime routing work shared by all controller types. Only the
// selected handler's typed invocation remains in handle_route's method loop.
struct RouteMatch {
	name      string
	route     Route
	params    []string
	from_data bool
}

fn select_route(routes &map[string]Route, path string, host string, method http.Method) ?RouteMatch {
	is_root := path.len == 0 || path == '/'
	if is_root {
		if index_route := (*routes)['index'] {
			if method in index_route.methods && (index_route.host == '' || index_route.host == host)
				&& index_route.path in ['/', '/index'] {
				return RouteMatch{ name: 'index', route: index_route, from_data: true }
			}
		}
	}
	url_words := route_path_words(path)
	mut variadic_match := RouteMatch{}
	for name, route in *routes {
		if method !in route.methods || (route.host != '' && route.host != host) {
			continue
		}
		if (!route.path.contains('/:') && url_words == route.words)
			|| (url_words.len == 0 && route.words.len == 1 && route.words[0] == 'index' && name == 'index') {
			return RouteMatch{ name: name, route: route, from_data: true }
		}
		if params := route_matches(url_words, route.words) {
			if !route_is_variadic(route.words) {
				return RouteMatch{ name: name, route: route, params: params }
			}
			if should_prefer_variadic_route(route.words, variadic_match.route.words) {
				variadic_match = RouteMatch{ name: name, route: route, params: params }
			}
		}
	}
	if variadic_match.name != '' {
		return variadic_match
	}
	return none
}

fn route_is_variadic(route_words []string) bool {
	return route_words.len > 0 && route_words[route_words.len - 1].ends_with('...')
}

fn should_prefer_variadic_route(candidate []string, current []string) bool {
	if current.len == 0 {
		return true
	}
	if candidate.len != current.len {
		return candidate.len > current.len
	}
	return variadic_route_static_parts(candidate) > variadic_route_static_parts(current)
}

fn variadic_route_static_parts(route_words []string) int {
	mut static_parts := 0
	for route_word in route_words[..route_words.len - 1] {
		if !route_word.starts_with(':') {
			static_parts++
		}
	}
	return static_parts
}

fn route_matches(url_words []string, route_words []string) ?[]string {
	// URL path should be at least as long as the route path
	// except for the catchall route (`/:path...`)
	if route_words.len == 1 && route_words[0].starts_with(':') && route_words[0].ends_with('...') {
		return ['/' + url_words.join('/')]
	}
	if url_words.len < route_words.len {
		return none
	}
	mut params := []string{cap: url_words.len}
	if url_words.len == route_words.len {
		for i in 0 .. url_words.len {
			if route_words[i].starts_with(':') {
				// We found a path parameter
				params << url_words[i]
			} else if route_words[i] != url_words[i] {
				// This url does not match the route
				return none
			}
		}
		return params
	}

	// The last route can end with ... indicating an array
	if route_words.len == 0 || !route_words[route_words.len - 1].ends_with('...') {
		return none
	}

	for i in 0 .. route_words.len - 1 {
		if route_words[i].starts_with(':') {
			// We found a path parameter
			params << url_words[i]
		} else if route_words[i] != url_words[i] {
			// This url does not match the route
			return none
		}
	}
	params << url_words[route_words.len - 1..url_words.len].join('/')
	return params
}

// check if request is for a static file and serves it
// returns true if we served a static file, false otherwise
fn serve_if_static[X](app StaticHandler, mut user_context X, url urllib.URL, host string) bool {
	// TODO: handle url parameters properly - for now, ignore them
	mut asked_path := url.path
	static_handler := app
	if !static_handler.enable_markdown_negotiation && asked_path == '/'
		&& static_handler.static_files['/'] == ''
		&& static_handler.static_files['/index.html'] == ''
		&& static_handler.static_files['/index.htm'] == '' {
		return false
	}
	if !static_handler.enable_markdown_negotiation
		&& !static_handler.may_contain_static_path(asked_path) {
		return false
	}

	// Content negotiation for markdown files (if enabled)
	if static_handler.enable_markdown_negotiation {
		accept_header := user_context.req.header.get(.accept) or { '' }
		if accept_header.contains('text/markdown') {
			// Try markdown variants in order of priority
			markdown_variants := [
				asked_path + '.md',
				asked_path + '.html.md',
				asked_path + '/index.html.md',
			]

			for variant in markdown_variants {
				if static_handler.static_files[variant] != '' {
					asked_path = variant
					break
				}
			}
		}
	}

	base_path := os.base(asked_path)
	if !base_path.contains('.') && !asked_path.ends_with('/') {
		asked_path += '/'
	}

	if asked_path.ends_with('/') {
		// Check for markdown index first if Accept header requests it and feature is enabled
		if static_handler.enable_markdown_negotiation {
			accept_header := user_context.req.header.get(.accept) or { '' }
			if accept_header.contains('text/markdown')
				&& static_handler.static_files[asked_path + 'index.html.md'] != '' {
				asked_path += 'index.html.md'
			} else if static_handler.static_files[asked_path + 'index.html'] != '' {
				asked_path += 'index.html'
			} else if static_handler.static_files[asked_path + 'index.htm'] != '' {
				asked_path += 'index.htm'
			}
		} else if static_handler.static_files[asked_path + 'index.html'] != '' {
			asked_path += 'index.html'
		} else if static_handler.static_files[asked_path + 'index.htm'] != '' {
			asked_path += 'index.htm'
		}
	}
	static_file := static_handler.static_files[asked_path] or { return false }

	// StaticHandler ensures that the mime type exists on either the App or in veb
	ext := os.file_ext(static_file).to_lower()
	mut mime_type := static_handler.static_mime_types[ext] or { mime_types[ext] }

	static_host := static_handler.static_hosts[asked_path] or { '' }
	if static_file == '' || mime_type == '' {
		return false
	}
	if static_host != '' && static_host != host {
		return false
	}

	// Configure static file compression settings
	user_context.set_static_compression_config(static_handler.enable_static_gzip,
		static_handler.enable_static_zstd, static_handler.enable_static_compression, if static_handler.static_compression_max_size >= 0 {
			static_handler.static_compression_max_size
		} else {
			1048576 // Default: 1MB
		}, static_handler.static_compression_mime_types)

	user_context.send_file(mime_type, static_file)
	return true
}

fn (sh StaticHandler) may_contain_static_path(path string) bool {
	if sh.static_prefixes.len == 0 || path == '/' {
		return true
	}
	for prefix in sh.static_prefixes {
		if prefix.ends_with('/') {
			if path.starts_with(prefix) {
				return true
			}
		} else if path == prefix {
			return true
		}
	}
	return false
}

fn static_handler_config(static_files map[string]string, static_mime_types map[string]string, static_hosts map[string]string, static_prefixes []string, enable_static_gzip bool, enable_static_zstd bool, enable_static_compression bool, static_compression_max_size int, static_compression_mime_types []string, enable_markdown_negotiation bool) StaticHandler {
	return StaticHandler{
		static_files:                  static_files
		static_mime_types:             static_mime_types
		static_hosts:                  static_hosts
		static_prefixes:               static_prefixes
		enable_static_gzip:            enable_static_gzip
		enable_static_zstd:            enable_static_zstd
		enable_static_compression:     enable_static_compression
		static_compression_max_size:   static_compression_max_size
		static_compression_mime_types: static_compression_mime_types
		enable_markdown_negotiation:   enable_markdown_negotiation
	}
}

// send a string over `conn`
fn send_string(mut conn net.TcpConn, s string) ! {
	$if trace_send_string_conn ? {
		eprintln('> send_string: conn: ${ptr_str(conn)}')
	}
	$if trace_response ? {
		eprintln('> send_string:\n${s}\n')
	}
	if voidptr(conn) == unsafe { nil } {
		return error('connection was closed before send_string')
	}
	conn.write_string(s)!
}

// Set s to the form error
pub fn (mut ctx Context) error(s string) {
	eprintln('[veb] Context.error: ${s}')
	ctx.form_error = s
}

fn fast_send_resp_header(mut conn net.TcpConn, resp http.Response) ! {
	mut sb := strings.new_builder(resp.body.len + 200)
	sb.write_string('HTTP/')
	sb.write_string(resp.http_version)
	sb.write_string(' ')
	sb.write_decimal(resp.status_code)
	sb.write_string(' ')
	sb.write_string(resp.status_msg)
	sb.write_string('\r\n')

	resp.header.render_into_sb(mut sb,
		version: resp.version()
	)
	sb.write_string('\r\n')
	send_string(mut conn, sb.str())!
}

// Formats resp to a string suitable for HTTP response transmission
// A fast version of `resp.bytestr()` used with
// `send_string(mut ctx.conn, resp.bytestr())`
fn fast_send_resp(mut conn net.TcpConn, resp http.Response) ! {
	fast_send_resp_header(mut conn, resp)!
	send_string(mut conn, resp.body)!
}
