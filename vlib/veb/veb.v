// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module veb

import io
import net
import net.http
import net.mbedtls
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
	methods []http.Method
	path    string
	host    string
mut:
	middlewares       []RouteMiddleware
	after_middlewares []RouteMiddleware
}

// Generate route structs for an app
fn generate_routes[A, X](app &A) !map[string]Route {
	// Parsing methods attributes
	mut routes := map[string]Route{}
	$for method in A.methods {
		$if method.return_type is Result {
			http_methods, route_path, host := parse_attrs(method.name, method.attrs) or {
				return error('error parsing method attributes: ${err}')
			}

			mut route := Route{
				methods: http_methods
				path:    route_path
				host:    host
			}

			$if A is MiddlewareApp {
				route.middlewares = app.Middleware.get_handlers_for_route[X](route_path)
				route.after_middlewares = app.Middleware.get_handlers_for_route_after[X](route_path)
			}

			routes[method.name] = route
		} $else {
			// If we have route attributes, but the wrong return type, return an error
			if has_route_attributes(method.attrs) {
				return error('method `${method.name}` at `${method.location}` has route attributes but invalid return type. Handler methods must return `veb.Result`, not `!veb.Result` or other types')
			}
		}
	}
	return routes
}

// run - start a new veb server, listening to all available addresses, at the specified `port`
pub fn run[A, X](mut global_app A, port int) {
	run_at[A, X](mut global_app, host: '', port: port, family: .ip6) or { panic(err.msg()) }
}

@[params]
pub struct RunParams {
pub:
	// use `family: .ip, host: 'localhost'` when you want it to bind only to 127.0.0.1
	family                    net.AddrFamily = .ip6
	host                      string
	port                      int  = default_port
	show_startup_message      bool = true
	timeout_in_seconds        int  = 30
	max_request_buffer_size   int  = 8192
	benchmark_page_generation bool // for the "page rendered in X ms"
	ssl_config                mbedtls.SSLConnectConfig
}

struct SslRequestParams {
	global_app                voidptr
	controllers_sorted        []&ControllerPath
	routes                    &map[string]Route
	benchmark_page_generation bool
	max_request_buffer_size   int
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
	return params.host
}

fn listen_addr(params RunParams) string {
	if params.host == '' {
		return ':${params.port}'
	}
	return '${params.host}:${params.port}'
}

fn run_at_with_ssl[A, X](mut global_app A, params RunParams) ! {
	routes := generate_routes[A, X](global_app)!
	controllers_sorted := check_duplicate_routes_in_controllers[A](global_app, routes)!
	if params.show_startup_message {
		println('[veb] Running app on https://${startup_host(params)}:${params.port}/')
	}
	flush_stdout()
	mut ssl_listener := mbedtls.new_ssl_listener(listen_addr(params), params.ssl_config)!
	defer {
		ssl_listener.shutdown() or {}
	}
	ssl_params := &SslRequestParams{
		global_app:                unsafe { global_app }
		controllers_sorted:        controllers_sorted
		routes:                    &routes
		benchmark_page_generation: params.benchmark_page_generation
		max_request_buffer_size:   if params.max_request_buffer_size > 0 {
			params.max_request_buffer_size
		} else {
			max_read
		}
	}
	$if A is BeforeAcceptApp {
		global_app.before_accept_loop()
	}
	for {
		mut ssl_conn := ssl_listener.accept() or {
			eprintln('[veb] accept() failed, reason: ${err}; skipping')
			continue
		}
		ssl_conn.duration = params.timeout_in_seconds * time.second
		spawn handle_ssl_connection[A, X](mut ssl_conn, ssl_params)
	}
}

fn handle_ssl_connection[A, X](mut ssl_conn mbedtls.SSLConn, params &SslRequestParams) {
	defer {
		ssl_conn.shutdown() or {}
	}
	mut reader := io.new_buffered_reader(
		reader: ssl_conn
		cap:    params.max_request_buffer_size
	)
	defer {
		unsafe {
			reader.free()
		}
	}
	for {
		req := read_request_from_buffered_reader(mut reader) or {
			if err !is io.Eof {
				write_ssl_response(mut ssl_conn, http_400) or {}
			}
			return
		}
		completed_context := handle_ssl_request[A, X](req, params) or {
			write_ssl_response(mut ssl_conn, http_400) or {}
			return
		}
		if completed_context.takeover {
			eprintln('[veb] HTTPS connections do not support `ctx.takeover_conn()` yet; closing the connection after this response.')
		}
		write_ssl_context_response(mut ssl_conn, completed_context) or {
			eprintln('[veb] error sending HTTPS response: ${err}')
			return
		}
		if completed_context.takeover
			|| should_close_ssl_connection(completed_context.req, completed_context.res, completed_context.client_wants_to_close) {
			return
		}
	}
}

fn read_request_from_buffered_reader(mut reader io.BufferedReader) !http.Request {
	mut req := http.parse_request_head(mut reader)!
	if transfer_encoding_is_chunked(req.header) {
		req.data = read_chunked_request_body(mut reader)!
		return req
	}
	content_length := req.header.get(.content_length) or { '0' }
	content_length_i := content_length.int()
	if content_length_i <= 0 {
		return req
	}
	mut body := []u8{len: content_length_i}
	read_exact_bytes(mut reader, mut body)!
	req.data = body.bytestr()
	return req
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

fn read_chunked_request_body(mut reader io.BufferedReader) !string {
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
		offset += reader.read(mut buf[offset..])!
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
	host, _ := urllib.split_host_port(host_with_port)
	mut ctx := &Context{
		req:            req
		page_gen_start: page_gen_start
		query:          query
		form:           form
		files:          files
	}
	if connection_header := req.header.get(.connection) {
		if connection_header.to_lower() == 'close' {
			ctx.client_wants_to_close = true
		}
	}
	$if A is StaticApp {
		ctx.custom_mime_types = global_app.static_mime_types.clone()
	}
	$if A is ControllerInterface {
		if completed_context := handle_controllers[X](params.controllers_sorted, ctx, mut
			url, host)
		{
			return completed_context
		}
	}
	mut user_context := X{}
	user_context.Context = ctx
	handle_route[A, X](mut global_app, mut user_context, url, host, params.routes)
	return &user_context.Context
}

fn write_ssl_context_response(mut ssl_conn mbedtls.SSLConn, completed_context &Context) ! {
	if !completed_context.done && completed_context.return_type == .normal {
		return error('context did not send a response')
	}
	match completed_context.return_type {
		.normal {
			write_ssl_response(mut ssl_conn, completed_context.res)!
		}
		.file {
			write_ssl_response(mut ssl_conn, completed_context.res)!
			if completed_context.return_file == '' {
				return error('missing file response path')
			}
			mut file := os.open(completed_context.return_file)!
			defer {
				file.close()
			}
			mut buf := []u8{len: max_read}
			for {
				n := file.read(mut buf) or {
					if err is io.Eof {
						break
					}
					return err
				}
				if n <= 0 {
					break
				}
				ssl_conn.write(buf[..n])!
			}
		}
	}
}

fn write_ssl_response(mut ssl_conn mbedtls.SSLConn, resp http.Response) ! {
	ssl_conn.write(resp.bytes())!
}

fn should_close_ssl_connection(req http.Request, resp http.Response, client_wants_to_close bool) bool {
	if client_wants_to_close {
		return true
	}
	resp_conn := (resp.header.get(.connection) or { '' }).to_lower()
	if resp_conn == 'close' {
		return true
	}
	if resp_conn == 'keep-alive' {
		return false
	}
	req_conn := (req.header.get(.connection) or { '' }).to_lower()
	if req_conn == 'close' {
		return true
	}
	if req_conn == 'keep-alive' {
		return false
	}
	return req.version != .v1_1
}

struct FileResponse {
pub mut:
	open              bool
	file              os.File
	total             i64
	pos               i64
	should_close_conn bool
}

// close the open file and reset the struct to its default values
pub fn (mut fr FileResponse) done() {
	fr.open = false
	fr.file.close()
	fr.total = 0
	fr.pos = 0
	fr.should_close_conn = false
}

struct StringResponse {
pub mut:
	open              bool
	str               string
	pos               i64
	should_close_conn bool
}

// free the current string and reset the struct to its default values
@[manualfree]
pub fn (mut sr StringResponse) done() {
	sr.open = false
	sr.pos = 0
	sr.should_close_conn = false
	unsafe { sr.str.free() }
}

$if !new_veb ? {
	// EV context
	struct RequestParams {
		global_app         voidptr
		controllers        []&ControllerPath
		routes             &map[string]Route
		timeout_in_seconds int
	mut:
		// request body buffer
		buf &u8 = unsafe { nil }
		// idx keeps track of how much of the request body has been read
		// for each incomplete request, see `handle_conn`
		idx                 []int
		incomplete_requests []http.Request
		file_responses      []FileResponse
		string_responses    []StringResponse
	}

	// reset request parameters for `fd`:
	// reset content-length index and the http request
	@[manualfree]
	pub fn (mut params RequestParams) request_done(fd int) {
		mut request := &params.incomplete_requests[fd]
		request.reset()
		params.idx[fd] = 0
		$if trace_handle_read ? {
			eprintln('>>>>> fd: ${fd} | request_done.')
		}
	}
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
				validate_middleware[X](mut user_context, app.Middleware.get_global_handlers_after[X]())
				// skip route-specific after-middleware if global already sent a response
				if !user_context.Context.done {
					validate_middleware[X](mut user_context, get_handlers_for_method(route.after_middlewares,
						user_context.Context.req.method))
				}
			}
		}
		// send only the headers, because if the response body is too big, TcpConn code will
		// actually block, because it has to wait for the socket to become ready to write. veb
		// will handle this case.
		if !was_done && !user_context.Context.done && !user_context.Context.takeover {
			eprintln('[veb] handler for route "${url.path}" does not send any data!')
			// send response anyway so the connection won't block
			// fast_send_resp_header(mut user_context.conn, user_context.res) or {}
		} else if !user_context.Context.takeover {
			// fast_send_resp_header(mut user_context.conn, user_context.res) or {}
		}
		// Context.takeover is set to true, so the user must close the connection and sent a response.
	}

	url_words := url.path.split('/').filter(it != '')

	$if veb_livereload ? {
		if url.path.starts_with('/veb_livereload/') {
			if url.path.ends_with('current') {
				user_context.handle_veb_livereload_current()
				return
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

	// then execute global middleware functions
	$if A is MiddlewareApp {
		if validate_middleware[X](mut user_context, app.Middleware.get_global_handlers[X]()) == false {
			middleware_has_sent_response = true
			return
		}
	}

	$if A is StaticApp {
		if serve_if_static[A, X](app, mut user_context, url, host) {
			// successfully served a static file
			return
		}
	}

	// Route matching and match route specific middleware as last step
	$for method in A.methods {
		$if method.return_type is Result {
			route = (*routes)[method.name] or {
				eprintln('[veb] parsed attributes for the `${method.name}` are not found, skipping...')
				Route{}
			}

			// Skip if the HTTP request method does not match the attributes
			if user_context.Context.req.method in route.methods {
				// Used for route matching
				route_words := route.path.split('/').filter(it != '')

				// Skip if the host does not match or is empty
				if route.host == '' || route.host == host {
					can_have_data_args := user_context.Context.req.method == .post
						|| user_context.Context.req.method == .get
					// Route immediate matches first
					// For example URL `/register` matches route `/:user`, but `fn register()`
					// should be called first.
					if !route.path.contains('/:') && url_words == route_words {
						// We found a match
						$if A is MiddlewareApp {
							if validate_middleware[X](mut user_context, get_handlers_for_method(route.middlewares,
								user_context.Context.req.method)) == false {
								middleware_has_sent_response = true
								return
							}
						}
						if method.args.len > 1 && can_have_data_args {
							// Populate method args with form or query values
							mut args := []string{cap: method.args.len + 1}
							data := if user_context.Context.req.method == .get {
								user_context.Context.query
							} else {
								user_context.Context.form
							}
							for param in method.args[1..] {
								args << data[param.name]
							}
							app.$method(mut user_context, args)
						} else {
							app.$method(mut user_context)
						}
						return
					}

					if url_words.len == 0 && route_words == ['index'] && method.name == 'index' {
						$if A is MiddlewareApp {
							if validate_middleware[X](mut user_context, get_handlers_for_method(route.middlewares,
								user_context.Context.req.method)) == false {
								middleware_has_sent_response = true
								return
							}
						}

						if method.args.len > 1 && can_have_data_args {
							// Populate method args with form or query values
							mut args := []string{cap: method.args.len + 1}
							data := if user_context.Context.req.method == .get {
								user_context.Context.query
							} else {
								user_context.Context.form
							}
							for param in method.args[1..] {
								args << data[param.name]
							}
							app.$method(mut user_context, args)
						} else {
							app.$method(mut user_context)
						}
						return
					}

					if params := route_matches(url_words, route_words) {
						$if A is MiddlewareApp {
							if validate_middleware[X](mut user_context, get_handlers_for_method(route.middlewares,
								user_context.Context.req.method)) == false {
								middleware_has_sent_response = true
								return
							}
						}
						method_args := params.clone()
						if method_args.len + 1 != method.args.len {
							eprintln('[veb] warning: uneven parameters count (${method.args.len}) in `${method.name}`, compared to the veb route `${method.attrs}` (${method_args.len})')
						}
						app.$method(mut user_context, method_args)
						return
					}
				}
			}
		}
	}
	// return 404
	user_context.not_found()
	not_found = true
	return
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
@[manualfree]
fn serve_if_static[A, X](app &A, mut user_context X, url urllib.URL, host string) bool {
	// TODO: handle url parameters properly - for now, ignore them
	mut asked_path := url.path

	// Content negotiation for markdown files (if enabled)
	if app.enable_markdown_negotiation {
		accept_header := user_context.req.header.get(.accept) or { '' }
		if accept_header.contains('text/markdown') {
			// Try markdown variants in order of priority
			markdown_variants := [
				asked_path + '.md',
				asked_path + '.html.md',
				asked_path + '/index.html.md',
			]

			for variant in markdown_variants {
				if app.static_files[variant] != '' {
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
		if app.enable_markdown_negotiation {
			accept_header := user_context.req.header.get(.accept) or { '' }
			if accept_header.contains('text/markdown')
				&& app.static_files[asked_path + 'index.html.md'] != '' {
				asked_path += 'index.html.md'
			} else if app.static_files[asked_path + 'index.html'] != '' {
				asked_path += 'index.html'
			} else if app.static_files[asked_path + 'index.htm'] != '' {
				asked_path += 'index.htm'
			}
		} else if app.static_files[asked_path + 'index.html'] != '' {
			asked_path += 'index.html'
		} else if app.static_files[asked_path + 'index.htm'] != '' {
			asked_path += 'index.htm'
		}
	}
	static_file := app.static_files[asked_path] or { return false }

	// StaticHandler ensures that the mime type exists on either the App or in veb
	ext := os.file_ext(static_file).to_lower()
	mut mime_type := app.static_mime_types[ext] or { mime_types[ext] }

	static_host := app.static_hosts[asked_path] or { '' }
	if static_file == '' || mime_type == '' {
		return false
	}
	if static_host != '' && static_host != host {
		return false
	}

	// Configure static file compression settings
	user_context.enable_static_gzip = app.enable_static_gzip
	user_context.enable_static_zstd = app.enable_static_zstd
	user_context.enable_static_compression = app.enable_static_compression
	user_context.static_compression_max_size = if app.static_compression_max_size >= 0 {
		app.static_compression_max_size
	} else {
		1048576 // Default: 1MB
	}
	user_context.static_compression_mime_types = app.static_compression_mime_types.clone()

	user_context.send_file(mime_type, static_file)
	return true
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

// send a string ptr over `conn`
fn send_string_ptr(mut conn net.TcpConn, ptr &u8, len int) !int {
	$if trace_send_string_conn ? {
		eprintln('> send_string: conn: ${ptr_str(conn)}')
	}
	if voidptr(conn) == unsafe { nil } {
		return error('connection was closed before send_string')
	}
	return conn.write_ptr(ptr, len)
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
