// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
//@[deprecated: '`vweb` is deprecated `veb`. Please use `veb` instead.']
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
import context
import strings

// A type which don't get filtered inside templates
pub type RawHtml = string

// A dummy structure that returns from routes to indicate that you actually sent something to a user
@[noinit]
pub struct Result {}

pub const methods_with_form = [http.Method.post, .put, .patch]
pub const headers_close = http.new_custom_header_from_map({
	'Server':                           'VWeb'
	http.CommonHeader.connection.str(): 'close'
}) or { panic('should never fail') }

pub const http_302 = http.new_response(
	status: .found
	body:   '302 Found'
	header: headers_close
)
pub const http_303 = http.new_response(
	status: .see_other
	body:   '303 See Other'
	header: headers_close
)
pub const http_400 = http.new_response(
	status: .bad_request
	body:   '400 Bad Request'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)
pub const http_404 = http.new_response(
	status: .not_found
	body:   '404 Not Found'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)
pub const http_500 = http.new_response(
	status: .internal_server_error
	body:   '500 Internal Server Error'
	header: http.new_header(
		key:   .content_type
		value: 'text/plain'
	).join(headers_close)
)
pub const mime_types = {
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
	'.md':     'text/markdown'
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
	'.toml':   'application/toml'
	'.tif':    'image/tiff'
	'.tiff':   'image/tiff'
	'.ts':     'video/mp2t'
	'.ttf':    'font/ttf'
	'.txt':    'text/plain'
	'.vsd':    'application/vnd.visio'
	'.wasm':   'application/wasm'
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
	'.m3u8':   'application/vnd.apple.mpegurl'
	'.vsh':    'text/x-vlang'
	'.v':      'text/x-vlang'
}
pub const max_http_post_size = 1024 * 1024
pub const default_port = 8080

// The Context struct represents the Context which hold the HTTP request and response.
// It has fields for the query, form, files.
pub struct Context {
mut:
	content_type string          = 'text/plain'
	status       string          = '200 OK'
	ctx          context.Context = context.EmptyContext{}
pub mut:
	// HTTP Request
	req http.Request
	// TODO: Response
	done bool
	// time.ticks() from start of vweb connection handle.
	// You can use it to determine how much time is spent on your request.
	page_gen_start i64
	// TCP connection to client.
	// But beware, do not store it for further use, after request processing vweb will close connection.
	conn              &net.TcpConn = unsafe { nil }
	static_files      map[string]string
	static_mime_types map[string]string
	static_hosts      map[string]string
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
	path_words []string // precalculated once to avoid split() allocations in handle_conn()
	middleware string
	host       string
}

// Defining this method is optional.
// This method called at server start.
// You can use it for initializing globals.
pub fn (ctx &Context) init_server() {
	eprintln('init_server() has been deprecated, please init your web app in `fn main()`')
}

// before_accept_loop is called once the vweb app is started, and listening, but before the loop that accepts
// incoming request connections.
// It will be called in the main thread, that runs vweb.run/2 or vweb.run_at/2.
// It allows you to be notified about the successful start of your app, and to synchronise your other threads
// with the webserver start, without error prone and slow pooling or time.sleep waiting.
// Defining this method is optional.
pub fn (ctx &Context) before_accept_loop() {
}

// before_request is called once before each request is routed.
// It will be called in one of multiple threads in a pool, serving requests,
// the same one, in which the matching route method will be executed right after it.
// Defining this method is optional.
pub fn (ctx &Context) before_request() {}

// TODO: test
// vweb intern function
@[manualfree]
pub fn (mut ctx Context) send_response_to_client(mimetype string, res string) bool {
	if ctx.done {
		return false
	}
	ctx.done = true

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
		.content_type:   mimetype
		.content_length: resp.body.len.str()
	}).join(ctx.header)
	resp.header = header.join(headers_close)

	resp.set_version(.v1_1)
	resp.set_status(http.status_from_int(ctx.status.int()))
	// send_string(mut ctx.conn, resp.bytestr()) or { return false }
	fast_send_resp(mut ctx.conn, resp) or { return false }
	return true
}

// Response with payload and content-type `text/html`
pub fn (mut ctx Context) html(payload string) Result {
	ctx.send_response_to_client('text/html', payload)
	return Result{}
}

// Response with s as payload and content-type `text/plain`
pub fn (mut ctx Context) text(s string) Result {
	ctx.send_response_to_client('text/plain', s)
	return Result{}
}

// Response with json_s as payload and content-type `application/json`
pub fn (mut ctx Context) json[T](j T) Result {
	json_s := json.encode(j)
	ctx.send_response_to_client('application/json', json_s)
	return Result{}
}

// Response with a pretty-printed JSON result
pub fn (mut ctx Context) json_pretty[T](j T) Result {
	json_s := json.encode_pretty(j)
	ctx.send_response_to_client('application/json', json_s)
	return Result{}
}

// TODO: test
// Response with file as payload
pub fn (mut ctx Context) file(f_path string) Result {
	if !os.exists(f_path) {
		eprintln('[vweb] file ${f_path} does not exist')
		return ctx.not_found()
	}
	ext := os.file_ext(f_path)
	data := os.read_file(f_path) or {
		eprint(err.msg())
		ctx.server_error(500)
		return Result{}
	}
	content_type := mime_types[ext]
	if content_type.len == 0 {
		eprintln('[vweb] no MIME type found for extension ${ext}')
		ctx.server_error(500)
	} else {
		ctx.send_response_to_client(content_type, data)
	}
	return Result{}
}

// Response with s as payload and sets the status code to HTTP_OK
pub fn (mut ctx Context) ok(s string) Result {
	ctx.set_status(200, 'OK')
	ctx.send_response_to_client(ctx.content_type, s)
	return Result{}
}

// TODO: test
// Response a server error
pub fn (mut ctx Context) server_error(ecode int) Result {
	$if debug {
		eprintln('> ctx.server_error ecode: ${ecode}')
	}
	if ctx.done {
		return Result{}
	}
	send_string(mut ctx.conn, http_500.bytestr()) or {}
	return Result{}
}

@[params]
pub struct RedirectParams {
pub:
	status_code int = 302
}

// Redirect to an url
pub fn (mut ctx Context) redirect(url string, params RedirectParams) Result {
	if ctx.done {
		return Result{}
	}
	ctx.done = true
	mut resp := http_302
	if params.status_code == 303 {
		resp = http_303
	}
	resp.header = resp.header.join(ctx.header)
	resp.header.add(.location, url)
	send_string(mut ctx.conn, resp.bytestr()) or { return Result{} }
	return Result{}
}

// Send an not_found response
pub fn (mut ctx Context) not_found() Result {
	// TODO: add a [must_be_returned] attribute, so that the caller is forced to use `return app.not_found()`
	if ctx.done {
		return Result{}
	}
	ctx.done = true
	send_string(mut ctx.conn, http_404.bytestr()) or {}
	return Result{}
}

// TODO: test
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

// TODO: test
// Sets a cookie with a `expire_date`
pub fn (mut ctx Context) set_cookie_with_expire_date(key string, val string, expire_date time.Time) {
	cookie := http.Cookie{
		name:    key
		value:   val
		expires: expire_date
	}
	ctx.set_cookie(cookie)
}

// Gets a cookie by a key
pub fn (ctx &Context) get_cookie(key string) !string {
	c := ctx.req.cookie(key) or { return error('Cookie not found') }
	return c.value
	// if value := ctx.req.cookies[key] {
	// return value
	//}
}

// TODO: test
// Sets the response status
pub fn (mut ctx Context) set_status(code int, desc string) {
	if code < 100 || code > 599 {
		ctx.status = '500 Internal Server Error'
	} else {
		ctx.status = '${code} ${desc}'
	}
}

// TODO: test
// Adds an header to the response with key and val
pub fn (mut ctx Context) add_header(key string, val string) {
	ctx.header.add_custom(key, val) or {}
}

// TODO: test
// Returns the header data from the key
pub fn (ctx &Context) get_header(key string) string {
	return ctx.req.header.get_custom(key) or { '' }
}

// set_value sets a value on the context
pub fn (mut ctx Context) set_value(key context.Key, value context.Any) {
	ctx.ctx = context.with_value(ctx.ctx, key, value)
}

// get_value gets a value from the context
pub fn (ctx &Context) get_value[T](key context.Key) ?T {
	if val := ctx.ctx.value(key) {
		match val {
			T {
				// `context.value()` always returns a reference
				// if we send back `val` the returntype becomes `?&T` and this can be problematic
				// for end users since they won't be able to do something like
				// `app.get_value[string]('a') or { '' }
				// since V expects the value in the or block to be of type `&string`.
				// And if a reference was allowed it would enable mutating the context directly
				return *val
			}
			else {}
		}
	}
	return none
}

pub type DatabasePool[T] = fn (tid int) T

interface DbPoolInterface {
	db_handle voidptr
mut:
	db voidptr
}

interface DbInterface {
mut:
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
		http_methods, route_path, middleware, host := parse_attrs(method.name, method.attrs) or {
			return error('error parsing method attributes: ${err}')
		}

		routes[method.name] = Route{
			methods:    http_methods
			path:       route_path
			path_words: route_path.split('/').filter(it != '')
			middleware: middleware
			host:       host
		}
	}
	return routes
}

type ControllerHandler = fn (ctx Context, mut url urllib.URL, host string, tid int)

pub struct ControllerPath {
pub:
	path    string
	handler ControllerHandler = unsafe { nil }
pub mut:
	host string
}

interface ControllerInterface {
	controllers []&ControllerPath
}

pub struct Controller {
pub mut:
	controllers []&ControllerPath
}

// controller generates a new Controller for the main app
pub fn controller[T](path string, global_app &T) &ControllerPath {
	routes := generate_routes(global_app) or { panic(err.msg()) }

	// generate struct with closure so the generic type is encapsulated in the closure
	// no need to type `ControllerHandler` as generic since it's not needed for closures
	return &ControllerPath{
		path:    path
		handler: fn [global_app, path, routes] [T](ctx Context, mut url urllib.URL, host string, tid int) {
			// request_app is freed in `handle_route`
			mut request_app := new_request_app[T](global_app, ctx, tid)
			// transform the url
			url.path = url.path.all_after_first(path)
			handle_route[T](mut request_app, url, host, &routes, tid)
		}
	}
}

// controller_host generates a controller which only handles incoming requests from the `host` domain
pub fn controller_host[T](host string, path string, global_app &T) &ControllerPath {
	mut ctrl := controller(path, global_app)
	ctrl.host = host
	return ctrl
}

// run - start a new VWeb server, listening to all available addresses, at the specified `port`
pub fn run[T](global_app &T, port int) {
	run_at[T](global_app, host: '', port: port, family: .ip6) or { panic(err.msg()) }
}

@[params]
pub struct RunParams {
pub:
	family               net.AddrFamily = .ip6 // use `family: .ip, host: 'localhost'` when you want it to bind only to 127.0.0.1
	host                 string
	port                 int  = 8080
	nr_workers           int  = runtime.nr_jobs()
	pool_channel_slots   int  = 1000
	show_startup_message bool = true
	startup_message      string
}

// run_at - start a new VWeb server, listening only on a specific address `host`, at the specified `port`
// Example: vweb.run_at(new_app(), vweb.RunParams{ host: 'localhost' port: 8099 family: .ip }) or { panic(err) }
@[manualfree]
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

	routes := generate_routes(global_app)!
	controllers_sorted := check_duplicate_routes_in_controllers[T](global_app, routes)!

	listen_address := '${params.host}:${params.port}'
	mut l := net.listen_tcp(params.family, listen_address) or {
		ecode := err.code()
		return error('failed to listen ${ecode} ${err}')
	}
	$if trace_listen ? {
		eprintln('>> vweb listen_address: `${listen_address}` | params.family: ${params.family} | l.addr: ${l.addr()} | params: ${params}')
	}

	if params.show_startup_message {
		if params.startup_message == '' {
			host := if params.host == '' { 'localhost' } else { params.host }
			println('[Vweb] Running app on http://${host}:${params.port}/')
		} else {
			println(params.startup_message)
		}
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

	unsafe {
		global_app.before_accept_loop()
	}

	// Forever accept every connection that comes, and
	// pass it through the channel, to the thread pool:
	for {
		mut connection := l.accept_only() or {
			// failures should not panic
			eprintln('[vweb] accept() failed with error: ${err.msg()}')
			continue
		}
		ch <- &RequestParams{
			connection:  connection
			global_app:  unsafe { global_app }
			controllers: controllers_sorted
			routes:      &routes
		}
	}
}

fn check_duplicate_routes_in_controllers[T](global_app &T, routes map[string]Route) ![]&ControllerPath {
	mut controllers_sorted := []&ControllerPath{}
	$if T is ControllerInterface {
		mut paths := []string{}
		controllers_sorted = global_app.controllers.clone()
		controllers_sorted.sort(a.path.len > b.path.len)
		for controller in controllers_sorted {
			if controller.host == '' {
				if controller.path in paths {
					return error('conflicting paths: duplicate controller handling the route "${controller.path}"')
				}
				paths << controller.path
			}
		}
		for method_name, route in routes {
			for controller_path in paths {
				if route.path.starts_with(controller_path) {
					return error('conflicting paths: method "${method_name}" with route "${route.path}" should be handled by the Controller of path "${controller_path}"')
				}
			}
		}
	}
	return controllers_sorted
}

fn new_request_app[T](global_app &T, ctx Context, tid int) &T {
	// Create a new app object for each connection, copy global data like db connections
	mut request_app := &T{}
	$if T is MiddlewareInterface {
		request_app = &T{
			middlewares: global_app.middlewares.clone()
		}
	}

	$if T is DbPoolInterface {
		// get database connection from the connection pool
		request_app.db = global_app.db_handle(tid)
	} $else $if T is DbInterface {
		// copy a database to a app without pooling
		request_app.db = global_app.db
	}

	$for field in T.fields {
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
	request_app.Context.static_hosts = global_app.static_hosts.clone()

	return request_app
}

@[manualfree]
fn handle_conn[T](mut conn net.TcpConn, global_app &T, controllers []&ControllerPath, routes &map[string]Route,
	tid int) {
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
		if err !is io.Eof {
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
		conn.write(http_400.bytes()) or {}
		return
	}

	// remove the port from the HTTP Host header
	host_with_port := req.header.get(.host) or { '' }
	host, _ := urllib.split_host_port(host_with_port)

	// Create Context with request data
	ctx := Context{
		ctx:            context.background()
		req:            req
		page_gen_start: page_gen_start
		conn:           conn
		query:          query
		form:           form
		files:          files
	}

	// match controller paths
	$if T is ControllerInterface {
		for controller in controllers {
			// skip controller if the hosts don't match
			if controller.host != '' && host != controller.host {
				continue
			}
			if url.path.len >= controller.path.len && url.path.starts_with(controller.path) {
				// pass route handling to the controller
				controller.handler(ctx, mut url, host, tid)
				return
			}
		}
	}

	mut request_app := new_request_app(global_app, ctx, tid)
	handle_route(mut request_app, url, host, routes, tid)
}

@[manualfree]
fn handle_route[T](mut app T, url urllib.URL, host string, routes &map[string]Route, tid int) {
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
	if serve_if_static[T](mut app, url, host) {
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
				route_words := route.path_words // route.path.split('/').filter(it != '')
				// println('ROUTES ${routes}')
				// println('\nROUTE WORDS')
				// println(route_words)
				// println(route.path_words)

				// Skip if the host does not match or is empty
				if route.host == '' || route.host == host {
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
							} else if validate_app_middleware(mut app, route.middleware,
								method.name)
							{
								app.$method(args)
							}
						} else {
							if route.middleware == '' {
								app.$method()
							} else if validate_app_middleware(mut app, route.middleware,
								method.name)
							{
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
fn serve_if_static[T](mut app T, url urllib.URL, host string) bool {
	// TODO: handle url parameters properly - for now, ignore them
	static_file := app.static_files[url.path] or { return false }
	mime_type := app.static_mime_types[url.path] or { return false }
	static_host := app.static_hosts[url.path] or { '' }
	if static_file == '' || mime_type == '' {
		return false
	}
	if static_host != '' && static_host != host {
		return false
	}
	data := os.read_file(static_file) or {
		send_string(mut app.conn, http_404.bytestr()) or {}
		return true
	}
	app.send_response_to_client(mime_type, data)
	unsafe { data.free() }
	return true
}

fn (mut ctx Context) scan_static_directory(directory_path string, mount_path string, host string) {
	files := os.ls(directory_path) or { panic(err) }
	if files.len > 0 {
		for file in files {
			full_path := os.join_path(directory_path, file)
			if os.is_dir(full_path) {
				ctx.scan_static_directory(full_path, mount_path.trim_right('/') + '/' + file,
					host)
			} else if file.contains('.') && !file.starts_with('.') && !file.ends_with('.') {
				ext := os.file_ext(file)
				// Rudimentary guard against adding files not in mime_types.
				// Use host_serve_static directly to add non-standard mime types.
				if ext in mime_types {
					ctx.host_serve_static(host, mount_path.trim_right('/') + '/' + file,
						full_path)
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
	return ctx.host_handle_static('', directory_path, root)
}

// host_handle_static is used to mark a folder (relative to the current working folder)
// as one that contains only static resources (css files, images etc).
// If `root` is set the mount path for the dir will be in '/'
// Usage:
// ```v
// os.chdir( os.executable() )?
// app.host_handle_static('localhost', 'assets', true)
// ```
pub fn (mut ctx Context) host_handle_static(host string, directory_path string, root bool) bool {
	if ctx.done || !os.exists(directory_path) {
		return false
	}
	dir_path := directory_path.trim_space().trim_right('/')
	mut mount_path := ''
	if dir_path != '.' && os.is_dir(dir_path) && !root {
		// Mount point hygiene, "./assets" => "/assets".
		mount_path = '/' + dir_path.trim_left('.').trim('/')
	}
	ctx.scan_static_directory(dir_path, mount_path, host)
	return true
}

// TODO: test
// mount_static_folder_at - makes all static files in `directory_path` and inside it, available at http://server/mount_path
// For example: suppose you have called .mount_static_folder_at('/var/share/myassets', '/assets'),
// and you have a file /var/share/myassets/main.css .
// => That file will be available at URL: http://server/assets/main.css .
pub fn (mut ctx Context) mount_static_folder_at(directory_path string, mount_path string) bool {
	return ctx.host_mount_static_folder_at('', directory_path, mount_path)
}

// TODO: test
// host_mount_static_folder_at - makes all static files in `directory_path` and inside it, available at http://host/mount_path
// For example: suppose you have called .host_mount_static_folder_at('localhost', '/var/share/myassets', '/assets'),
// and you have a file /var/share/myassets/main.css .
// => That file will be available at URL: http://localhost/assets/main.css .
pub fn (mut ctx Context) host_mount_static_folder_at(host string, directory_path string, mount_path string) bool {
	if ctx.done || mount_path == '' || mount_path[0] != `/` || !os.exists(directory_path) {
		return false
	}
	dir_path := directory_path.trim_right('/')

	trim_mount_path := mount_path.trim_left('/').trim_right('/')
	ctx.scan_static_directory(dir_path, '/${trim_mount_path}', host)
	return true
}

// TODO: test
// Serves a file static
// `url` is the access path on the site, `file_path` is the real path to the file, `mime_type` is the file type
pub fn (mut ctx Context) serve_static(url string, file_path string) {
	ctx.host_serve_static('', url, file_path)
}

// TODO: test
// Serves a file static
// `url` is the access path on the site, `file_path` is the real path to the file
// `mime_type` is the file type, `host` is the host to serve the file from
pub fn (mut ctx Context) host_serve_static(host string, url string, file_path string) {
	ctx.static_files[url] = file_path
	// ctx.static_mime_types[url] = mime_type
	ext := os.file_ext(file_path)
	ctx.static_mime_types[url] = mime_types[ext]
	ctx.static_hosts[url] = host
}

// user_agent returns the user-agent header for the current client
pub fn (ctx &Context) user_agent() string {
	return ctx.req.header.get(.user_agent) or { '' }
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
	// ctx.set_cookie(name: 'veb.error', value: s)
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

// Formats resp to a string suitable for HTTP response transmission
// A fast version of `resp.bytestr()` used with
// `send_string(mut ctx.conn, resp.bytestr())`
fn fast_send_resp(mut conn net.TcpConn, resp http.Response) ! {
	mut sb := strings.new_builder(resp.body.len + 200)
	/*
	send_string(mut conn, 'HTTP/')!
	send_string(mut conn, resp.http_version)!
	send_string(mut conn, ' ')!
	send_string(mut conn, resp.status_code.str())!
	send_string(mut conn, ' ')!
	send_string(mut conn, resp.status_msg)!
	send_string(mut conn, '\r\n')!
	send_string(mut conn, resp.header.render(
		version: resp.version()
	))!
	send_string(mut conn, '\r\n')!
	send_string(mut conn, resp.body)!
	*/
	sb.write_string('HTTP/')
	sb.write_string(resp.http_version)
	sb.write_string(' ')
	sb.write_decimal(resp.status_code)
	sb.write_string(' ')
	sb.write_string(resp.status_msg)
	sb.write_string('\r\n')
	// sb.write_string(resp.header.render_with_sb(
	// version: resp.version()
	//))
	resp.header.render_into_sb(mut sb,
		version: resp.version()
	)
	sb.write_string('\r\n')
	sb.write_string(resp.body)
	send_string(mut conn, sb.str())!
}

// Do not delete.
// It used by `vlib/v/gen/c/str_intp.v:130` for string interpolation inside vweb templates
// TODO: move it to template render
fn filter(s string) string {
	return html.escape(s)
}

// Worker functions for the thread pool:
struct RequestParams {
	global_app  voidptr
	controllers []&ControllerPath
	routes      &map[string]Route
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
	return spawn w.process_incoming_requests[T]()
}

fn (mut w Worker[T]) process_incoming_requests() {
	sid := '[vweb] tid: ${w.id:03d} received request'
	for {
		mut params := <-w.ch or { break }
		$if vweb_trace_worker_scan ? {
			eprintln(sid)
		}
		handle_conn[T](mut params.connection, params.global_app, params.controllers, params.routes,
			w.id)
	}
	$if vweb_trace_worker_scan ? {
		eprintln('[vweb] closing worker ${w.id}.')
	}
}

@[params]
pub struct PoolParams[T] {
pub:
	handler    fn () T = unsafe { nil } @[required]
	nr_workers int     = runtime.nr_jobs()
}

// database_pool creates a pool of database connections
pub fn database_pool[T](params PoolParams[T]) DatabasePool[T] {
	mut connections := []T{}
	// create a database connection for each worker
	for _ in 0 .. params.nr_workers {
		connections << params.handler()
	}

	return fn [connections] [T](tid int) T {
		$if vweb_trace_worker_scan ? {
			eprintln('[vweb] worker ${tid} received database connection')
		}
		return connections[tid]
	}
}
