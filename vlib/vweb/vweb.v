// Copyright (c) 2019-2023 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module vweb

import os
import io
import runtime
import net
import net.http
import net.urllib
import time
import json
import encoding.html
import db.pg
import db.sqlite

// A type which don't get filtered inside templates
pub type RawHtml = string

// A dummy structure that returns from routes to indicate that you actually sent something to a user
[noinit]
pub struct Result {}

pub const (
	methods_with_form = [http.Method.post, .put, .patch]
	headers_close     = http.new_custom_header_from_map({
		'Server':                           'VWeb'
		http.CommonHeader.connection.str(): 'close'
	}) or { panic('should never fail') }

	http_302          = http.new_response(
		status: .found
		body: '302 Found'
		header: headers_close
	)
	http_400          = http.new_response(
		status: .bad_request
		body: '400 Bad Request'
		header: http.new_header(
			key: .content_type
			value: 'text/plain'
		).join(headers_close)
	)
	http_404          = http.new_response(
		status: .not_found
		body: '404 Not Found'
		header: http.new_header(
			key: .content_type
			value: 'text/plain'
		).join(headers_close)
	)
	http_500          = http.new_response(
		status: .internal_server_error
		body: '500 Internal Server Error'
		header: http.new_header(
			key: .content_type
			value: 'text/plain'
		).join(headers_close)
	)
	mime_types        = {
		'.aac':    'audio/aac'
		'.abw':    'application/x-abiword'
		'.arc':    'application/x-freearc'
		'.avi':    'video/x-msvideo'
		'.azw':    'application/vnd.amazon.ebook'
		'.bin':    'application/octet-stream'
		'.bmp':    'image/bmp'
		'.bz':     'application/x-bzip'
		'.bz2':    'application/x-bzip2'
		'.cda':    'application/x-cdf'
		'.csh':    'application/x-csh'
		'.css':    'text/css'
		'.csv':    'text/csv'
		'.doc':    'application/msword'
		'.docx':   'application/vnd.openxmlformats-officedocument.wordprocessingml.document'
		'.eot':    'application/vnd.ms-fontobject'
		'.epub':   'application/epub+zip'
		'.gz':     'application/gzip'
		'.gif':    'image/gif'
		'.htm':    'text/html'
		'.html':   'text/html'
		'.ico':    'image/vnd.microsoft.icon'
		'.ics':    'text/calendar'
		'.jar':    'application/java-archive'
		'.jpeg':   'image/jpeg'
		'.jpg':    'image/jpeg'
		'.js':     'text/javascript'
		'.json':   'application/json'
		'.jsonld': 'application/ld+json'
		'.mid':    'audio/midi audio/x-midi'
		'.midi':   'audio/midi audio/x-midi'
		'.mjs':    'text/javascript'
		'.mp3':    'audio/mpeg'
		'.mp4':    'video/mp4'
		'.mpeg':   'video/mpeg'
		'.mpkg':   'application/vnd.apple.installer+xml'
		'.odp':    'application/vnd.oasis.opendocument.presentation'
		'.ods':    'application/vnd.oasis.opendocument.spreadsheet'
		'.odt':    'application/vnd.oasis.opendocument.text'
		'.oga':    'audio/ogg'
		'.ogv':    'video/ogg'
		'.ogx':    'application/ogg'
		'.opus':   'audio/opus'
		'.otf':    'font/otf'
		'.png':    'image/png'
		'.pdf':    'application/pdf'
		'.php':    'application/x-httpd-php'
		'.ppt':    'application/vnd.ms-powerpoint'
		'.pptx':   'application/vnd.openxmlformats-officedocument.presentationml.presentation'
		'.rar':    'application/vnd.rar'
		'.rtf':    'application/rtf'
		'.sh':     'application/x-sh'
		'.svg':    'image/svg+xml'
		'.swf':    'application/x-shockwave-flash'
		'.tar':    'application/x-tar'
		'.tif':    'image/tiff'
		'.tiff':   'image/tiff'
		'.ts':     'video/mp2t'
		'.ttf':    'font/ttf'
		'.txt':    'text/plain'
		'.vsd':    'application/vnd.visio'
		'.wav':    'audio/wav'
		'.weba':   'audio/webm'
		'.webm':   'video/webm'
		'.webp':   'image/webp'
		'.woff':   'font/woff'
		'.woff2':  'font/woff2'
		'.xhtml':  'application/xhtml+xml'
		'.xls':    'application/vnd.ms-excel'
		'.xlsx':   'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
		'.xml':    'application/xml'
		'.xul':    'application/vnd.mozilla.xul+xml'
		'.zip':    'application/zip'
		'.3gp':    'video/3gpp'
		'.3g2':    'video/3gpp2'
		'.7z':     'application/x-7z-compressed'
	}
	max_http_post_size = 1024 * 1024
	default_port       = 8080
)

// The Context struct represents the Context which hold the HTTP request and response.
// It has fields for the query, form, files.
pub struct Context {
mut:
	content_type string = 'text/plain'
	status       string = '200 OK'
pub:
	// HTTP Request
	req http.Request
	// TODO Response
pub mut:
	done bool
	// time.ticks() from start of vweb connection handle.
	// You can use it to determine how much time is spent on your request.
	page_gen_start i64
	// TCP connection to client.
	// But beware, do not store it for further use, after request processing vweb will close connection.
	conn              &net.TcpConn = unsafe { nil }
	static_files      map[string]string
	static_mime_types map[string]string
	// Map containing query params for the route.
	// http://localhost:3000/index?q=vpm&order_by=desc => { 'q': 'vpm', 'order_by': 'desc' }
	query map[string]string
	// Multipart-form fields.
	form map[string]string
	// Files from multipart-form.
	files map[string][]http.FileData

	header http.Header // response headers
	// ? It doesn't seem to be used anywhere
	form_error                  string
	livereload_poll_interval_ms int = 250
}

struct FileData {
pub:
	filename     string
	content_type string
	data         string
}

struct Route {
	methods    []http.Method
	path       string
	middleware string
}

// Defining this method is optional.
// This method called at server start.
// You can use it for initializing globals.
pub fn (ctx Context) init_server() {
	eprintln('init_server() has been deprecated, please init your web app in `fn main()`')
}

// Defining this method is optional.
// This method is called before every request (aka middleware).
// You can use it for checking user session cookies or to add headers.
pub fn (ctx Context) before_request() {}

// TODO - test
// vweb intern function
[manualfree]
pub fn (mut ctx Context) send_response_to_client(mimetype string, res string) bool {
	if ctx.done {
		return false
	}
	ctx.done = true
	//
	mut resp := http.Response{
		body: res
	}
	$if vweb_livereload ? {
		if mimetype == 'text/html' {
			resp.body = res.replace('</html>', '<script src="/vweb_livereload/${vweb_livereload_server_start}/script.js"></script>\n</html>')
		}
	}
	// build the header after the potential modification of resp.body from above
	header := http.new_header_from_map({
		http.CommonHeader.content_type:   mimetype
		http.CommonHeader.content_length: resp.body.len.str()
	}).join(ctx.header)
	resp.header = header.join(vweb.headers_close)
	//
	resp.set_version(.v1_1)
	resp.set_status(http.status_from_int(ctx.status.int()))
	send_string(mut ctx.conn, resp.bytestr()) or { return false }
	return true
}

// Response HTTP_OK with s as payload with content-type `text/html`
pub fn (mut ctx Context) html(s string) Result {
	ctx.send_response_to_client('text/html', s)
	return Result{}
}

// Response HTTP_OK with s as payload with content-type `text/plain`
pub fn (mut ctx Context) text(s string) Result {
	ctx.send_response_to_client('text/plain', s)
	return Result{}
}

// Response HTTP_OK with json_s as payload with content-type `application/json`
pub fn (mut ctx Context) json[T](j T) Result {
	json_s := json.encode(j)
	ctx.send_response_to_client('application/json', json_s)
	return Result{}
}

// Response HTTP_OK with a pretty-printed JSON result
pub fn (mut ctx Context) json_pretty[T](j T) Result {
	json_s := json.encode_pretty(j)
	ctx.send_response_to_client('application/json', json_s)
	return Result{}
}

// TODO - test
// Response HTTP_OK with file as payload
pub fn (mut ctx Context) file(f_path string) Result {
	ext := os.file_ext(f_path)
	data := os.read_file(f_path) or {
		eprint(err.msg())
		ctx.server_error(500)
		return Result{}
	}
	content_type := vweb.mime_types[ext]
	if content_type.len == 0 {
		eprintln('[vweb] no MIME type found for extension ${ext}')
		ctx.server_error(500)
	} else {
		ctx.send_response_to_client(content_type, data)
	}
	return Result{}
}

// Response HTTP_OK with s as payload
pub fn (mut ctx Context) ok(s string) Result {
	ctx.send_response_to_client(ctx.content_type, s)
	return Result{}
}

// TODO - test
// Response a server error
pub fn (mut ctx Context) server_error(ecode int) Result {
	$if debug {
		eprintln('> ctx.server_error ecode: ${ecode}')
	}
	if ctx.done {
		return Result{}
	}
	send_string(mut ctx.conn, vweb.http_500.bytestr()) or {}
	return Result{}
}

// Redirect to an url
pub fn (mut ctx Context) redirect(url string) Result {
	if ctx.done {
		return Result{}
	}
	ctx.done = true
	mut resp := vweb.http_302
	resp.header = resp.header.join(ctx.header)
	resp.header.add(.location, url)
	send_string(mut ctx.conn, resp.bytestr()) or { return Result{} }
	return Result{}
}

// Send an not_found response
pub fn (mut ctx Context) not_found() Result {
	if ctx.done {
		return Result{}
	}
	ctx.done = true
	send_string(mut ctx.conn, vweb.http_404.bytestr()) or {}
	return Result{}
}

// TODO - test
// Sets a cookie
pub fn (mut ctx Context) set_cookie(cookie http.Cookie) {
	cookie_raw := cookie.str()
	if cookie_raw == '' {
		eprintln('[vweb] error setting cookie: name of cookie is invalid')
		return
	}
	ctx.add_header('Set-Cookie', cookie_raw)
}

// Sets the response content type
pub fn (mut ctx Context) set_content_type(typ string) {
	ctx.content_type = typ
}

// TODO - test
// Sets a cookie with a `expire_date`
pub fn (mut ctx Context) set_cookie_with_expire_date(key string, val string, expire_date time.Time) {
	cookie := http.Cookie{
		name: key
		value: val
		expires: expire_date
	}
	ctx.set_cookie(cookie)
}

// Gets a cookie by a key
pub fn (ctx &Context) get_cookie(key string) !string {
	if value := ctx.req.cookies[key] {
		return value
	}
	return error('Cookie not found')
}

// TODO - test
// Sets the response status
pub fn (mut ctx Context) set_status(code int, desc string) {
	if code < 100 || code > 599 {
		ctx.status = '500 Internal Server Error'
	} else {
		ctx.status = '${code} ${desc}'
	}
}

// TODO - test
// Adds an header to the response with key and val
pub fn (mut ctx Context) add_header(key string, val string) {
	ctx.header.add_custom(key, val) or {}
}

// TODO - test
// Returns the header data from the key
pub fn (ctx &Context) get_header(key string) string {
	return ctx.req.header.get_custom(key) or { '' }
}

pub struct Database[T] {
	get_connection fn (tid int) T
mut:
	db T
}

interface DbInterface {
	db voidptr
}

pub type Middleware = fn (mut Context) bool

interface MiddlewareInterface {
	middlewares map[string][]Middleware
}

// Generate route structs for an app
fn generate_routes[T](app &T) !map[string]Route {
	// Parsing methods attributes
	mut routes := map[string]Route{}
	$for method in T.methods {
		http_methods, route_path, middleware := parse_attrs(method.name, method.attrs) or {
			return error('error parsing method attributes: ${err}')
		}

		routes[method.name] = Route{
			methods: http_methods
			path: route_path
			middleware: middleware
		}
	}
	return routes
}

type ControllerHandler = fn (ctx Context, mut url urllib.URL, tid int)

pub struct ControllerPath {
	path    string
	handler ControllerHandler
}

interface ControllerInterface {
	controllers []&ControllerPath
}

pub struct Controller {
mut:
	controllers []&ControllerPath
}

// controller generates a new Controller for the main app
pub fn controller[T](path string, global_app &T) &ControllerPath {
	routes := generate_routes(global_app) or { panic(err.msg()) }

	// generate struct with closure so the generic type is encapsulated in the closure
	// no need to type `ControllerHandler` as generic since it's not needed for closures
	return &ControllerPath{
		path: path
		handler: fn [global_app, path, routes] [T](ctx Context, mut url urllib.URL, tid int) {
			// request_app is freed in `handle_route`
			mut request_app := new_request_app[T](global_app, ctx, tid)
			// transform the url
			url.path = url.path.all_after_first(path)
			handle_route[T](mut request_app, url, &routes, tid)
		}
	}
}

// run - start a new VWeb server, listening to all available addresses, at the specified `port`
pub fn run[T](global_app &T, port int) {
	run_at[T](global_app, host: '', port: port, family: .ip6) or { panic(err.msg()) }
}

[params]
pub struct RunParams {
	family               net.AddrFamily = .ip6 // use `family: .ip, host: 'localhost'` when you want it to bind only to 127.0.0.1
	host                 string
	port                 int  = 8080
	nr_workers           int  = runtime.nr_jobs()
	pool_channel_slots   int  = 1000
	show_startup_message bool = true
}

// run_at - start a new VWeb server, listening only on a specific address `host`, at the specified `port`
// Example: vweb.run_at(new_app(), vweb.RunParams{ host: 'localhost' port: 8099 family: .ip }) or { panic(err) }
[manualfree]
pub fn run_at[T](global_app &T, params RunParams) ! {
	if params.port <= 0 || params.port > 65535 {
		return error('invalid port number `${params.port}`, it should be between 1 and 65535')
	}
	if params.pool_channel_slots < 1 {
		return error('invalid pool_channel_slots `${params.pool_channel_slots}`, it should be above 0, preferably higher than 10 x nr_workers')
	}
	if params.nr_workers < 1 {
		return error('invalid nr_workers `${params.nr_workers}`, it should be above 0')
	}

	mut l := net.listen_tcp(params.family, '${params.host}:${params.port}') or {
		ecode := err.code()
		return error('failed to listen ${ecode} ${err}')
	}

	routes := generate_routes(global_app)!
	// check duplicate routes in controllers
	$if T is ControllerInterface {
		mut paths := []string{}
		for controller in global_app.controllers {
			paths << controller.path
		}
		for method_name, route in routes {
			for controller_path in paths {
				if route.path.starts_with(controller_path) {
					return error('method "${method_name}" with route "${route.path}" should be handled by the Controller of "${controller_path}"')
				}
			}
		}
	}

	host := if params.host == '' { 'localhost' } else { params.host }
	if params.show_startup_message {
		println('[Vweb] Running app on http://${host}:${params.port}/')
	}

	ch := chan &RequestParams{cap: params.pool_channel_slots}
	mut ws := []thread{cap: params.nr_workers}
	for worker_number in 0 .. params.nr_workers {
		ws << new_worker[T](ch, worker_number)
	}
	if params.show_startup_message {
		println('[Vweb] We have ${ws.len} workers')
	}
	flush_stdout()

	// Forever accept every connection that comes, and
	// pass it through the channel, to the thread pool:
	for {
		mut connection := l.accept_only() or {
			// failures should not panic
			eprintln('[vweb] accept() failed with error: ${err.msg()}')
			continue
		}
		ch <- &RequestParams{
			connection: connection
			global_app: unsafe { global_app }
			routes: &routes
		}
	}
}

fn new_request_app[T](global_app &T, ctx Context, tid int) &T {
	// Create a new app object for each connection, copy global data like db connections
	mut request_app := &T{}
	$if T is MiddlewareInterface {
		request_app = &T{
			middlewares: global_app.middlewares.clone()
		}
	}
	$if T is DbInterface {
		// copy a database to a app without multithreading
		request_app.db = global_app.db
	}

	$for field in T.fields {
		// type has to be declared else a RUNTIME error is generated
		// get database connection from the connection pool
		$if field.typ is Database[pg.DB] {
			request_app.db = global_app.$(field.name).get_connection(tid)
		} $else $if field.typ is Database[sqlite.DB] {
			request_app.db = global_app.$(field.name).get_connection(tid)
		}

		if field.is_shared {
			unsafe {
				// TODO: remove this horrible hack, when copying a shared field at comptime works properly!!!
				raptr := &voidptr(&request_app.$(field.name))
				gaptr := &voidptr(&global_app.$(field.name))
				*raptr = *gaptr
				_ = raptr // TODO: v produces a warning that `raptr` is unused otherwise, even though it was on the previous line
			}
		} else {
			if 'vweb_global' in field.attrs {
				request_app.$(field.name) = global_app.$(field.name)
			}
		}
	}
	request_app.Context = ctx // copy request data such as form and query etc

	// copy static files
	request_app.Context.static_files = global_app.static_files.clone()
	request_app.Context.static_mime_types = global_app.static_mime_types.clone()

	return request_app
}

[manualfree]
fn handle_conn[T](mut conn net.TcpConn, global_app &T, routes &map[string]Route, tid int) {
	conn.set_read_timeout(30 * time.second)
	conn.set_write_timeout(30 * time.second)
	defer {
		conn.close() or {}
	}

	conn.set_sock() or {
		eprintln('[vweb] tid: ${tid:03d}, error setting socket')
		return
	}

	mut reader := io.new_buffered_reader(reader: conn)
	defer {
		unsafe {
			reader.free()
		}
	}

	page_gen_start := time.ticks()

	// Request parse
	req := http.parse_request(mut reader) or {
		// Prevents errors from being thrown when BufferedReader is empty
		if '${err}' != 'none' {
			eprintln('[vweb] tid: ${tid:03d}, error parsing request: ${err}')
		}
		return
	}
	$if trace_request ? {
		dump(req)
	}
	$if trace_request_url ? {
		dump(req.url)
	}
	// URL Parse
	mut url := urllib.parse(req.url) or {
		eprintln('[vweb] tid: ${tid:03d}, error parsing path: ${err}')
		return
	}

	// Query parse
	query := parse_query_from_url(url)

	// Form parse
	form, files := parse_form_from_request(req) or {
		// Bad request
		conn.write(vweb.http_400.bytes()) or {}
		return
	}

	// Create Context with request data
	ctx := Context{
		req: req
		page_gen_start: page_gen_start
		conn: conn
		query: query
		form: form
		files: files
	}

	// match controller paths
	$if T is ControllerInterface {
		for controller in global_app.controllers {
			if url.path.len >= controller.path.len && url.path.starts_with(controller.path) {
				// pass route handling to the controller
				controller.handler(ctx, mut url, tid)
				return
			}
		}
	}

	mut request_app := new_request_app(global_app, ctx, tid)
	handle_route(mut request_app, url, routes, tid)
}

[manualfree]
fn handle_route[T](mut app T, url urllib.URL, routes &map[string]Route, tid int) {
	defer {
		unsafe {
			free(app)
		}
	}

	url_words := url.path.split('/').filter(it != '')

	// Calling middleware...
	app.before_request()

	$if vweb_livereload ? {
		if url.path.starts_with('/vweb_livereload/') {
			if url.path.ends_with('current') {
				app.handle_vweb_livereload_current()
				return
			}
			if url.path.ends_with('script.js') {
				app.handle_vweb_livereload_script()
				return
			}
		}
	}

	// Static handling
	if serve_if_static[T](mut app, url) {
		// successfully served a static file
		return
	}

	// Route matching
	$for method in T.methods {
		$if method.return_type is Result {
			route := (*routes)[method.name] or {
				eprintln('[vweb] tid: ${tid:03d}, parsed attributes for the `${method.name}` are not found, skipping...')
				Route{}
			}

			// Skip if the HTTP request method does not match the attributes
			if app.req.method in route.methods {
				// Used for route matching
				route_words := route.path.split('/').filter(it != '')

				// Route immediate matches first
				// For example URL `/register` matches route `/:user`, but `fn register()`
				// should be called first.
				if !route.path.contains('/:') && url_words == route_words {
					// We found a match
					$if T is MiddlewareInterface {
						if validate_middleware(mut app, url.path) == false {
							return
						}
					}

					if app.req.method == .post && method.args.len > 0 {
						// Populate method args with form values
						mut args := []string{cap: method.args.len}
						for param in method.args {
							args << app.form[param.name]
						}

						if route.middleware == '' {
							app.$method(args)
						} else if validate_app_middleware(mut app, route.middleware, method.name) {
							app.$method(args)
						}
					} else {
						if route.middleware == '' {
							app.$method()
						} else if validate_app_middleware(mut app, route.middleware, method.name) {
							app.$method()
						}
					}
					return
				}

				if url_words.len == 0 && route_words == ['index'] && method.name == 'index' {
					$if T is MiddlewareInterface {
						if validate_middleware(mut app, url.path) == false {
							return
						}
					}
					if route.middleware == '' {
						app.$method()
					} else if validate_app_middleware(mut app, route.middleware, method.name) {
						app.$method()
					}
					return
				}

				if params := route_matches(url_words, route_words) {
					method_args := params.clone()
					if method_args.len != method.args.len {
						eprintln('[vweb] tid: ${tid:03d}, warning: uneven parameters count (${method.args.len}) in `${method.name}`, compared to the vweb route `${method.attrs}` (${method_args.len})')
					}

					$if T is MiddlewareInterface {
						if validate_middleware(mut app, url.path) == false {
							return
						}
					}
					if route.middleware == '' {
						app.$method(method_args)
					} else if validate_app_middleware(mut app, route.middleware, method.name) {
						app.$method(method_args)
					}
					return
				}
			}
		}
	}
	// Route not found
	app.not_found()
}

// validate_middleware validates and fires all middlewares that are defined in the global app instance
fn validate_middleware[T](mut app T, full_path string) bool {
	for path, middleware_chain in app.middlewares {
		// only execute middleware if route.path starts with `path`
		if full_path.len >= path.len && full_path.starts_with(path) {
			// there is middleware for this route
			for func in middleware_chain {
				if func(mut app.Context) == false {
					return false
				}
			}
		}
	}
	// passed all middleware checks
	return true
}

// validate_app_middleware validates all middlewares as a method of `app`
fn validate_app_middleware[T](mut app T, middleware string, method_name string) bool {
	// then the middleware that is defined for this route specifically
	valid := fire_app_middleware(mut app, middleware) or {
		eprintln('[vweb] warning: middleware `${middleware}` for the `${method_name}` are not found')
		true
	}
	return valid
}

// fire_app_middleware fires all middlewares that are defined as a method of `app`
fn fire_app_middleware[T](mut app T, method_name string) ?bool {
	$for method in T.methods {
		if method_name == method.name {
			$if method.return_type is bool {
				return app.$method()
			} $else {
				eprintln('[vweb] error in `${method.name}, middleware functions must return bool')
				return none
			}
		}
	}
	// no middleware function found
	return none
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
				// We found a path paramater
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
			// We found a path paramater
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
[manualfree]
fn serve_if_static[T](mut app T, url urllib.URL) bool {
	// TODO: handle url parameters properly - for now, ignore them
	static_file := app.static_files[url.path] or { return false }
	mime_type := app.static_mime_types[url.path] or { return false }
	if static_file == '' || mime_type == '' {
		return false
	}
	data := os.read_file(static_file) or {
		send_string(mut app.conn, vweb.http_404.bytestr()) or {}
		return true
	}
	app.send_response_to_client(mime_type, data)
	unsafe { data.free() }
	return true
}

fn (mut ctx Context) scan_static_directory(directory_path string, mount_path string) {
	files := os.ls(directory_path) or { panic(err) }
	if files.len > 0 {
		for file in files {
			full_path := os.join_path(directory_path, file)
			if os.is_dir(full_path) {
				ctx.scan_static_directory(full_path, mount_path.trim_right('/') + '/' + file)
			} else if file.contains('.') && !file.starts_with('.') && !file.ends_with('.') {
				ext := os.file_ext(file)
				// Rudimentary guard against adding files not in mime_types.
				// Use serve_static directly to add non-standard mime types.
				if ext in vweb.mime_types {
					ctx.serve_static(mount_path.trim_right('/') + '/' + file, full_path)
				}
			}
		}
	}
}

// handle_static is used to mark a folder (relative to the current working folder)
// as one that contains only static resources (css files, images etc).
// If `root` is set the mount path for the dir will be in '/'
// Usage:
// ```v
// os.chdir( os.executable() )?
// app.handle_static('assets', true)
// ```
pub fn (mut ctx Context) handle_static(directory_path string, root bool) bool {
	if ctx.done || !os.exists(directory_path) {
		return false
	}
	dir_path := directory_path.trim_space().trim_right('/')
	mut mount_path := ''
	if dir_path != '.' && os.is_dir(dir_path) && !root {
		// Mount point hygene, "./assets" => "/assets".
		mount_path = '/' + dir_path.trim_left('.').trim('/')
	}
	ctx.scan_static_directory(dir_path, mount_path)
	return true
}

// TODO - test
// mount_static_folder_at - makes all static files in `directory_path` and inside it, available at http://server/mount_path
// For example: suppose you have called .mount_static_folder_at('/var/share/myassets', '/assets'),
// and you have a file /var/share/myassets/main.css .
// => That file will be available at URL: http://server/assets/main.css .
pub fn (mut ctx Context) mount_static_folder_at(directory_path string, mount_path string) bool {
	if ctx.done || mount_path.len < 1 || mount_path[0] != `/` || !os.exists(directory_path) {
		return false
	}
	dir_path := directory_path.trim_right('/')

	trim_mount_path := mount_path.trim_left('/').trim_right('/')
	ctx.scan_static_directory(dir_path, '/${trim_mount_path}')
	return true
}

// TODO - test
// Serves a file static
// `url` is the access path on the site, `file_path` is the real path to the file, `mime_type` is the file type
pub fn (mut ctx Context) serve_static(url string, file_path string) {
	ctx.static_files[url] = file_path
	// ctx.static_mime_types[url] = mime_type
	ext := os.file_ext(file_path)
	ctx.static_mime_types[url] = vweb.mime_types[ext]
}

// Returns the ip address from the current user
pub fn (ctx &Context) ip() string {
	mut ip := ctx.req.header.get(.x_forwarded_for) or { '' }
	if ip == '' {
		ip = ctx.req.header.get_custom('X-Real-Ip') or { '' }
	}

	if ip.contains(',') {
		ip = ip.all_before(',')
	}
	if ip == '' {
		ip = ctx.conn.peer_ip() or { '' }
	}
	return ip
}

// Set s to the form error
pub fn (mut ctx Context) error(s string) {
	eprintln('[vweb] Context.error: ${s}')
	ctx.form_error = s
}

// Returns an empty result
pub fn not_found() Result {
	return Result{}
}

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

// Do not delete.
// It used by `vlib/v/gen/c/str_intp.v:130` for string interpolation inside vweb templates
// TODO: move it to template render
fn filter(s string) string {
	return html.escape(s)
}

// Worker functions for the thread pool:
struct RequestParams {
	global_app voidptr
	routes     &map[string]Route
mut:
	connection &net.TcpConn
}

struct Worker[T] {
	id int
	ch chan &RequestParams
}

fn new_worker[T](ch chan &RequestParams, id int) thread {
	mut w := &Worker[T]{
		id: id
		ch: ch
	}
	return spawn w.process_incomming_requests[T]()
}

fn (mut w Worker[T]) process_incomming_requests() {
	sid := '[vweb] tid: ${w.id:03d} received request'
	for {
		mut params := <-w.ch or { break }
		$if vweb_trace_worker_scan ? {
			eprintln(sid)
		}
		handle_conn[T](mut params.connection, params.global_app, params.routes, w.id)
	}
	$if vweb_trace_worker_scan ? {
		eprintln('[vweb] closing worker ${w.id}.')
	}
}

// database_pool creates a pool of database connections
pub fn database_pool[T](create_connection fn () T, workers int) fn (tid int) T {
	mut connections := []T{}
	// create a database connection for each worker
	for _ in 0 .. workers {
		connections << create_connection()
	}

	return fn [connections] [T](tid int) T {
		$if vweb_trace_worker_scan ? {
			eprintln('[vweb] worker ${w.id} received database connection')
		}
		return connections[tid]
	}
}
