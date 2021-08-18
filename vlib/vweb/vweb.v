// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module vweb

import os
import io
import net
import net.http
import net.urllib
import time

pub const (
	methods_with_form = [http.Method.post, .put, .patch]
	headers_close     = http.new_custom_header_from_map({
		'Server':          'VWeb'
		http.CommonHeader.connection.str(): 'close'
	}) or { panic('should never fail') }

	http_400          = http.new_response(
		status: .bad_request
		text: '400 Bad Request'
		header: http.new_header(
			key: .content_type
			value: 'text/plain'
		).join(headers_close)
	)
	http_404          = http.new_response(
		status: .not_found
		text: '404 Not Found'
		header: http.new_header(
			key: .content_type
			value: 'text/plain'
		).join(headers_close)
	)
	http_500          = http.new_response(
		status: .internal_server_error
		text: '500 Internal Server Error'
		header: http.new_header(
			key: .content_type
			value: 'text/plain'
		).join(headers_close)
	)
	mime_types        = {
		'.css':  'text/css; charset=utf-8'
		'.gif':  'image/gif'
		'.htm':  'text/html; charset=utf-8'
		'.html': 'text/html; charset=utf-8'
		'.jpg':  'image/jpeg'
		'.js':   'application/javascript'
		'.json': 'application/json'
		'.md':   'text/markdown; charset=utf-8'
		'.pdf':  'application/pdf'
		'.png':  'image/png'
		'.svg':  'image/svg+xml'
		'.txt':  'text/plain; charset=utf-8'
		'.wasm': 'application/wasm'
		'.xml':  'text/xml; charset=utf-8'
		'.ico':  'img/x-icon'
		'.ttf':  'font/ttf'
		'.otf':  'font/opentype'
	}
	max_http_post_size = 1024 * 1024
	default_port       = 8080
)

pub struct Context {
mut:
	content_type string = 'text/plain'
	status       string = '200 OK'
pub:
	req http.Request
	// TODO Response
pub mut:
	conn              &net.TcpConn
	static_files      map[string]string
	static_mime_types map[string]string
	form              map[string]string
	query             map[string]string
	files             map[string][]FileData
	header            http.Header // response headers
	done              bool
	page_gen_start    i64
	form_error        string
}

struct FileData {
pub:
	filename     string
	content_type string
	data         string
}

struct UnexpectedExtraAttributeError {
	msg  string
	code int
}

struct MultiplePathAttributesError {
	msg  string = 'Expected at most one path attribute'
	code int
}

// declaring init_server in your App struct is optional
pub fn (ctx Context) init_server() {
	eprintln('init_server() has been deprecated, please init your web app in `fn main()`')
}

// declaring before_request in your App struct is optional
pub fn (ctx Context) before_request() {}

pub struct Cookie {
	name      string
	value     string
	expires   time.Time
	secure    bool
	http_only bool
}

[noinit]
pub struct Result {
}

// vweb intern function
[manualfree]
pub fn (mut ctx Context) send_response_to_client(mimetype string, res string) bool {
	if ctx.done {
		return false
	}
	ctx.done = true

	// build header
	header := http.new_header_from_map({
		http.CommonHeader.content_type:   mimetype
		http.CommonHeader.content_length: res.len.str()
	}).join(ctx.header)

	mut resp := http.Response{
		header: header.join(vweb.headers_close)
		text: res
	}
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

// Response HTTP_OK with s as payload with content-type `application/json`
pub fn (mut ctx Context) json(s string) Result {
	ctx.send_response_to_client('application/json', s)
	return Result{}
}

// Response HTTP_OK with s as payload
pub fn (mut ctx Context) ok(s string) Result {
	ctx.send_response_to_client(ctx.content_type, s)
	return Result{}
}

// Response a server error
pub fn (mut ctx Context) server_error(ecode int) Result {
	$if debug {
		eprintln('> ctx.server_error ecode: $ecode')
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
	send_string(mut ctx.conn, 'HTTP/1.1 302 Found\r\nLocation: $url$ctx.header\r\n$vweb.headers_close\r\n') or {
		return Result{}
	}
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

// Sets a cookie
pub fn (mut ctx Context) set_cookie(cookie Cookie) {
	mut cookie_data := []string{}
	mut secure := if cookie.secure { 'Secure;' } else { '' }
	secure += if cookie.http_only { ' HttpOnly' } else { ' ' }
	cookie_data << secure
	if cookie.expires.unix > 0 {
		cookie_data << 'expires=$cookie.expires.utc_string()'
	}
	data := cookie_data.join(' ')
	ctx.add_header('Set-Cookie', '$cookie.name=$cookie.value; $data')
}

// Sets the response content type
pub fn (mut ctx Context) set_content_type(typ string) {
	ctx.content_type = typ
}

// Sets a cookie with a `expire_data`
pub fn (mut ctx Context) set_cookie_with_expire_date(key string, val string, expire_date time.Time) {
	ctx.add_header('Set-Cookie', '$key=$val;  Secure; HttpOnly; expires=$expire_date.utc_string()')
}

// Gets a cookie by a key
pub fn (ctx &Context) get_cookie(key string) ?string { // TODO refactor
	mut cookie_header := ctx.get_header('cookie')
	if cookie_header == '' {
		cookie_header = ctx.get_header('Cookie')
	}
	cookie_header = ' ' + cookie_header
	// println('cookie_header="$cookie_header"')
	// println(ctx.req.header)
	cookie := if cookie_header.contains(';') {
		cookie_header.find_between(' $key=', ';')
	} else {
		cookie_header.find_between(' $key=', '\r')
	}
	if cookie != '' {
		return cookie.trim_space()
	}
	return error('Cookie not found')
}

// Sets the response status
pub fn (mut ctx Context) set_status(code int, desc string) {
	if code < 100 || code > 599 {
		ctx.status = '500 Internal Server Error'
	} else {
		ctx.status = '$code $desc'
	}
}

// Adds an header to the response with key and val
pub fn (mut ctx Context) add_header(key string, val string) {
	ctx.header.add_custom(key, val) or {}
}

// Returns the header data from the key
pub fn (ctx &Context) get_header(key string) string {
	return ctx.req.header.get_custom(key) or { '' }
}

/*
pub fn run<T>(port int) {
	mut x := &T{}
	run_app(mut x, port)
}
*/

interface DbInterface {
	db voidptr
}

// run_app
[manualfree]
pub fn run<T>(global_app &T, port int) {
	// mut global_app := &T{}
	// mut app := &T{}
	// run_app<T>(mut app, port)

	mut l := net.listen_tcp(.ip6, ':$port') or { panic('failed to listen $err.code $err') }

	println('[Vweb] Running app on http://localhost:$port')
	// app.Context = Context{
	// conn: 0
	//}
	// app.init_server()
	// unsafe {
	// global_app.init_server()
	//}
	//$for method in T.methods {
	//$if method.return_type is Result {
	// check routes for validity
	//}
	//}
	for {
		// Create a new app object for each connection, copy global data like db connections
		mut request_app := &T{}
		$if T is DbInterface {
			request_app.db = global_app.db
		} $else {
			// println('vweb no db')
		}
		$for field in T.fields {
			if field.is_shared {
				request_app.$(field.name) = global_app.$(field.name)
			}
		}
		request_app.Context = global_app.Context // copy the context ref that contains static files map etc
		// request_app.Context = Context{
		// conn: 0
		//}
		mut conn := l.accept() or {
			// failures should not panic
			eprintln('accept() failed with error: $err.msg')
			continue
		}
		go handle_conn<T>(mut conn, mut request_app)
	}
}

[manualfree]
fn handle_conn<T>(mut conn net.TcpConn, mut app T) {
	conn.set_read_timeout(30 * time.second)
	conn.set_write_timeout(30 * time.second)
	defer {
		conn.close() or {}
		unsafe {
			free(app)
		}
	}
	mut reader := io.new_buffered_reader(reader: conn)
	defer {
		reader.free()
	}
	page_gen_start := time.ticks()
	req := parse_request(mut reader) or {
		// Prevents errors from being thrown when BufferedReader is empty
		if '$err' != 'none' {
			eprintln('error parsing request: $err')
		}
		return
	}
	app.Context = Context{
		req: req
		conn: conn
		form: map[string]string{}
		static_files: app.static_files
		static_mime_types: app.static_mime_types
		page_gen_start: page_gen_start
	}
	if req.method in vweb.methods_with_form {
		ct := req.header.get(.content_type) or { '' }.split(';').map(it.trim_left(' \t'))
		if 'multipart/form-data' in ct {
			boundary := ct.filter(it.starts_with('boundary='))
			if boundary.len != 1 {
				send_string(mut conn, vweb.http_400.bytestr()) or {}
				return
			}
			form, files := parse_multipart_form(req.data, boundary[0][9..])
			for k, v in form {
				app.form[k] = v
			}
			for k, v in files {
				app.files[k] = v
			}
		} else {
			form := parse_form(req.data)
			for k, v in form {
				app.form[k] = v
			}
		}
	}
	// Serve a static file if it is one
	// TODO: get the real path
	url := urllib.parse(app.req.url) or {
		eprintln('error parsing path: $err')
		return
	}
	if serve_if_static<T>(mut app, url) {
		// successfully served a static file
		return
	}

	app.before_request()
	// Call the right action
	$if debug {
		println('route matching...')
	}
	url_words := url.path.split('/').filter(it != '')
	// copy query args to app.query
	for k, v in url.query().data {
		app.query[k] = v.data[0]
	}

	$for method in T.methods {
		$if method.return_type is Result {
			mut method_args := []string{}
			// TODO: move to server start
			http_methods, route_path := parse_attrs(method.name, method.attrs) or {
				eprintln('error parsing method attributes: $err')
				return
			}

			// Used for route matching
			route_words := route_path.split('/').filter(it != '')

			// Skip if the HTTP request method does not match the attributes
			if app.req.method in http_methods {
				// Route immediate matches first
				// For example URL `/register` matches route `/:user`, but `fn register()`
				// should be called first.
				if !route_path.contains('/:') && url_words == route_words {
					// We found a match
					app.$method()
					return
				}

				if url_words.len == 0 && route_words == ['index'] && method.name == 'index' {
					app.$method()
					return
				}

				if params := route_matches(url_words, route_words) {
					method_args = params.clone()
					if method_args.len != method.args.len {
						eprintln('warning: uneven parameters count ($method.args.len) in `$method.name`, compared to the vweb route `$method.attrs` ($method_args.len)')
					}
					app.$method(method_args)
					return
				}
			}
		}
	}
	// site not found
	send_string(mut conn, vweb.http_404.bytestr()) or {}
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

// parse function attribute list for methods and a path
fn parse_attrs(name string, attrs []string) ?([]http.Method, string) {
	if attrs.len == 0 {
		return [http.Method.get], '/$name'
	}

	mut x := attrs.clone()
	mut methods := []http.Method{}
	mut path := ''

	for i := 0; i < x.len; {
		attr := x[i]
		attru := attr.to_upper()
		m := http.method_from_str(attru)
		if attru == 'GET' || m != .get {
			methods << m
			x.delete(i)
			continue
		}
		if attr.starts_with('/') {
			if path != '' {
				return IError(&MultiplePathAttributesError{})
			}
			path = attr
			x.delete(i)
			continue
		}
		i++
	}
	if x.len > 0 {
		return IError(&UnexpectedExtraAttributeError{
			msg: 'Encountered unexpected extra attributes: $x'
		})
	}
	if methods.len == 0 {
		methods = [http.Method.get]
	}
	if path == '' {
		path = '/$name'
	}
	// Make path lowercase for case-insensitive comparisons
	return methods, path.to_lower()
}

// check if request is for a static file and serves it
// returns true if we served a static file, false otherwise
[manualfree]
fn serve_if_static<T>(mut app T, url urllib.URL) bool {
	// TODO: handle url parameters properly - for now, ignore them
	static_file := app.static_files[url.path]
	mime_type := app.static_mime_types[url.path]
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
				ctx.scan_static_directory(full_path, mount_path + '/' + file)
			} else if file.contains('.') && !file.starts_with('.') && !file.ends_with('.') {
				ext := os.file_ext(file)
				// Rudimentary guard against adding files not in mime_types.
				// Use serve_static directly to add non-standard mime types.
				if ext in vweb.mime_types {
					ctx.serve_static(mount_path + '/' + file, full_path)
				}
			}
		}
	}
}

// Handles a directory static
// If `root` is set the mount path for the dir will be in '/'
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

// mount_static_folder_at - makes all static files in `directory_path` and inside it, available at http://server/mount_path
// For example: suppose you have called .mount_static_folder_at('/var/share/myassets', '/assets'),
// and you have a file /var/share/myassets/main.css .
// => That file will be available at URL: http://server/assets/main.css .
pub fn (mut ctx Context) mount_static_folder_at(directory_path string, mount_path string) bool {
	if ctx.done || mount_path.len < 1 || mount_path[0] != `/` || !os.exists(directory_path) {
		return false
	}
	dir_path := directory_path.trim_right('/')
	ctx.scan_static_directory(dir_path, mount_path[1..])
	return true
}

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
	println('vweb error: $s')
	ctx.form_error = s
}

// Returns an empty result
pub fn not_found() Result {
	return Result{}
}

fn filter(s string) string {
	return s.replace_each([
		'<',
		'&lt;',
		'"',
		'&quot;',
		'&',
		'&amp;',
	])
}

// A type which don't get filtered inside templates
pub type RawHtml = string

fn send_string(mut conn net.TcpConn, s string) ? {
	conn.write(s.bytes()) ?
}
