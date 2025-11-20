// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module veb

import net
import net.http
import net.urllib
import os
import strings

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
	middlewares       []voidptr
	after_middlewares []voidptr
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
	family               net.AddrFamily = .ip6
	host                 string
	port                 int  = default_port
	show_startup_message bool = true
	timeout_in_seconds   int  = 30
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
	pub fn (mut params RequestParams) request_done(fd int) {
		params.incomplete_requests[fd] = http.Request{}
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

fn handle_route[A, X](mut app A, mut user_context X, url urllib.URL, host string, routes &map[string]Route) {
	// println('\n\nhandle_route() url=${url} routes=${routes}')
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
				validate_middleware[X](mut user_context, route.after_middlewares)
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
	$if A is HasBeforeRequest {
		app.before_request()
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

	// defer {
	// println('USER CONTEXT at end of handle_route')
	// println(user_context)
	//}

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
							if validate_middleware[X](mut user_context, route.middlewares) == false {
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

							// println('m1')
							app.$method(mut user_context, args)
						} else {
							// println('m2')
							app.$method(mut user_context)
						}
						return
					}

					// println('route_words=${route_words} method=${method}')
					if url_words.len == 0 && route_words == ['index'] && method.name == 'index' {
						$if A is MiddlewareApp {
							if validate_middleware[X](mut user_context, route.middlewares) == false {
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

							// println('m3')
							app.$method(mut user_context, args)
						} else {
							// println('m4')
							app.$method(mut user_context)
						}
						return
					}

					if params := route_matches(url_words, route_words) {
						$if A is MiddlewareApp {
							if validate_middleware[X](mut user_context, route.middlewares) == false {
								middleware_has_sent_response = true
								return
							}
						}

						method_args := params.clone()
						if method_args.len + 1 != method.args.len {
							eprintln('[veb] warning: uneven parameters count (${method.args.len}) in `${method.name}`, compared to the veb route `${method.attrs}` (${method_args.len})')
						}
						// println('m5')
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
	// println('route_matches(url_words:${url_words} route_words:${route_words}')
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

	// Configure static file gzip compression settings
	user_context.enable_static_gzip = app.enable_static_gzip
	user_context.static_gzip_max_size = if app.static_gzip_max_size >= 0 {
		app.static_gzip_max_size
	} else {
		1048576 // Default: 1MB
	}

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
	// $if trace_response ? {
	// 	eprintln('> send_string:\n${s}\n')
	// }
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
