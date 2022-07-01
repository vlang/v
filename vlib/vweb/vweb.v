// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module vweb

import os
import io
import net
import net.http
import net.urllib
import time
import json

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
	conn              &net.TcpConn
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
	form_error string
}

struct FileData {
pub:
	filename     string
	content_type string
	data         string
}

struct Route {
	methods []http.Method
	path    string
}

// Defining this method is optional.
// This method called at server start.
// You can use it for initializing globals.
pub fn (ctx Context) init_server() {
	eprintln('init_server() has been deprecated, please init your web app in `fn main()`')
}

// Defining this method is optional.
// This method called before every request (aka middleware).
// Probably you can use it for check user session cookie or add header.
pub fn (ctx Context) before_request() {}

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
		body: res
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

// Response HTTP_OK with json_s as payload with content-type `application/json`
pub fn (mut ctx Context) json<T>(j T) Result {
	json_s := json.encode(j)
	ctx.send_response_to_client('application/json', json_s)
	return Result{}
}

// Response HTTP_OK with a pretty-printed JSON result
pub fn (mut ctx Context) json_pretty<T>(j T) Result {
	json_s := json.encode_pretty(j)
	ctx.send_response_to_client('application/json', json_s)
	return Result{}
}

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
		eprintln('no MIME type found for extension $ext')
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

// Sets a cookie
pub fn (mut ctx Context) set_cookie(cookie http.Cookie) {
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

interface DbInterface {
	db voidptr
}

// run - start a new VWeb server, listening to all available addresses, at the specified `port`
pub fn run<T>(global_app &T, port int) {
	run_at<T>(global_app, host: '', port: port, family: .ip6) or { panic(err.msg()) }
}

[params]
pub struct RunParams {
	host   string
	port   int = 8080
	family net.AddrFamily = .ip6 // use `family: .ip, host: 'localhost'` when you want it to bind only to 127.0.0.1
}

// run_at - start a new VWeb server, listening only on a specific address `host`, at the specified `port`
// Example: vweb.run_at(app, 'localhost', 8099)
[manualfree]
pub fn run_at<T>(global_app &T, params RunParams) ? {
	mut l := net.listen_tcp(params.family, '$params.host:$params.port') or {
		ecode := err.code()
		return error('failed to listen $ecode $err')
	}

	// Parsing methods attributes
	mut routes := map[string]Route{}
	$for method in T.methods {
		http_methods, route_path := parse_attrs(method.name, method.attrs) or {
			return error('error parsing method attributes: $err')
		}

		routes[method.name] = Route{
			methods: http_methods
			path: route_path
		}
	}
	host := if params.host == '' { 'localhost' } else { params.host }
	println('[Vweb] Running app on http://$host:$params.port/')
	for {
		// Create a new app object for each connection, copy global data like db connections
		mut request_app := &T{}
		$if T is DbInterface {
			request_app.db = global_app.db
		} $else {
			// println('vweb no db')
		}
		$for field in T.fields {
			if 'vweb_global' in field.attrs || field.is_shared {
				request_app.$(field.name) = global_app.$(field.name)
			}
		}
		request_app.Context = global_app.Context // copy the context ref that contains static files map etc
		mut conn := l.accept() or {
			// failures should not panic
			eprintln('accept() failed with error: $err.msg()')
			continue
		}
		go handle_conn<T>(mut conn, mut request_app, routes)
	}
}

[manualfree]
fn handle_conn<T>(mut conn net.TcpConn, mut app T, routes map[string]Route) {
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

	// Request parse
	req := http.parse_request(mut reader) or {
		// Prevents errors from being thrown when BufferedReader is empty
		if '$err' != 'none' {
			eprintln('error parsing request: $err')
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
	url := urllib.parse(req.url) or {
		eprintln('error parsing path: $err')
		return
	}

	// Query parse
	query := parse_query_from_url(url)
	url_words := url.path.split('/').filter(it != '')

	// Form parse
	form, files := parse_form_from_request(req) or {
		// Bad request
		conn.write(vweb.http_400.bytes()) or {}
		return
	}

	app.Context = Context{
		req: req
		page_gen_start: page_gen_start
		conn: conn
		query: query
		form: form
		files: files
		static_files: app.static_files
		static_mime_types: app.static_mime_types
	}

	// Calling middleware...
	app.before_request()

	// Static handling
	if serve_if_static<T>(mut app, url) {
		// successfully served a static file
		return
	}

	// Route matching
	$for method in T.methods {
		$if method.return_type is Result {
			route := routes[method.name] or {
				eprintln('parsed attributes for the `$method.name` are not found, skipping...')
				Route{}
			}

			// Skip if the HTTP request method does not match the attributes
			if req.method in route.methods {
				// Used for route matching
				route_words := route.path.split('/').filter(it != '')

				// Route immediate matches first
				// For example URL `/register` matches route `/:user`, but `fn register()`
				// should be called first.
				if !route.path.contains('/:') && url_words == route_words {
					// We found a match
					if req.method == .post && method.args.len > 0 {
						// Populate method args with form values
						mut args := []string{cap: method.args.len}
						for param in method.args {
							args << form[param.name]
						}
						app.$method(args)
					} else {
						app.$method()
					}
					return
				}

				if url_words.len == 0 && route_words == ['index'] && method.name == 'index' {
					app.$method()
					return
				}

				if params := route_matches(url_words, route_words) {
					method_args := params.clone()
					if method_args.len != method.args.len {
						eprintln('warning: uneven parameters count ($method.args.len) in `$method.name`, compared to the vweb route `$method.attrs` ($method_args.len)')
					}
					app.$method(method_args)
					return
				}
			}
		}
	}
	// Route not found
	conn.write(vweb.http_404.bytes()) or {}
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
				ctx.scan_static_directory(full_path, mount_path + file)
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
	ctx.scan_static_directory(dir_path, '/$trim_mount_path')
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

fn send_string(mut conn net.TcpConn, s string) ? {
	$if trace_response ? {
		eprintln('> send_string:\n$s\n')
	}
	conn.write(s.bytes())?
}

// Do not delete.
// It used by `vlib/v/gen/c/str_intp.v:130` for string interpolation inside vweb templates
// TODO: move it to template render
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
