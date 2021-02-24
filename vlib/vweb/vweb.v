// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module vweb

import os
import io
import net
import net.http
import net.urllib
import strings
import time

pub const (
	methods_with_form       = [http.Method.post, .put, .patch]
	methods_without_first   = ['ost', 'ut', 'et', 'atch', 'ptions', 'elete', 'ead'] // needed for method checking as method parameter
	header_server           = 'Server: VWeb\r\n'
	header_connection_close = 'Connection: close\r\n'
	headers_close           = '$header_server$header_connection_close\r\n'
	http_404                = 'HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\nContent-Length: 13\r\n${headers_close}404 Not Found'
	http_500                = 'HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain\r\n${headers_close}500 Internal Server Error'
	mime_types              = map{
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
	}
	max_http_post_size      = 1024 * 1024
	default_port            = 8080
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
	headers           string // response headers
	done              bool
	page_gen_start    i64
	form_error        string
	chunked_transfer  bool
	max_chunk_len     int = 20
}

struct FileData {
pub:
	filename     string
	content_type string
	data         string
}

// declaring init_once in your App struct is optional
pub fn (ctx Context) init_once() {}

// declaring init in your App struct is optional
pub fn (ctx Context) init() {}

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
pub fn (mut ctx Context) send_response_to_client(mimetype string, res string) bool {
	if ctx.done {
		return false
	}
	ctx.done = true
	mut sb := strings.new_builder(1024)
	defer {
		unsafe { sb.free() }
	}
	sb.write_string('HTTP/1.1 $ctx.status')
	sb.write_string('\r\nContent-Type: $mimetype')
	sb.write_string('\r\nContent-Length: $res.len')
	if ctx.chunked_transfer {
		sb.write_string('\r\nTransfer-Encoding: chunked')
	}
	sb.write_string(ctx.headers)
	sb.write_string('\r\n')
	sb.write_string(vweb.headers_close)
	if ctx.chunked_transfer {
		mut i := 0
		mut len := res.len
		for {
			if len <= 0 {
				break
			}
			mut chunk := ''
			if len > ctx.max_chunk_len {
				chunk = res[i..i + ctx.max_chunk_len]
				i += ctx.max_chunk_len
				len -= ctx.max_chunk_len
			} else {
				chunk = res[i..]
				len = 0
			}
			sb.write_string(chunk.len.hex())
			sb.write_string('\r\n$chunk\r\n')
		}
		sb.write_string('0\r\n\r\n') // End of chunks
	} else {
		sb.write_string(res)
	}
	s := sb.str()
	defer {
		unsafe { s.free() }
	}
	send_string(mut ctx.conn, s) or { return false }
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
	send_string(mut ctx.conn, vweb.http_500) or { }
	return Result{}
}

// Redirect to an url
pub fn (mut ctx Context) redirect(url string) Result {
	if ctx.done {
		return Result{}
	}
	ctx.done = true
	send_string(mut ctx.conn, 'HTTP/1.1 302 Found\r\nLocation: $url$ctx.headers\r\n$vweb.headers_close') or {
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
	send_string(mut ctx.conn, vweb.http_404) or { }
	return Result{}
}

// Enables chunk transfer with max_chunk_len per chunk
pub fn (mut ctx Context) enable_chunked_transfer(max_chunk_len int) {
	ctx.chunked_transfer = true
	ctx.max_chunk_len = max_chunk_len
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

// Old function
[deprecated]
pub fn (mut ctx Context) set_cookie_old(key string, val string) {
	// TODO support directives, escape cookie value (https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie)
	// ctx.add_header('Set-Cookie', '${key}=${val};  Secure; HttpOnly')
	ctx.add_header('Set-Cookie', '$key=$val; HttpOnly')
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
	// println(ctx.req.headers)
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
	// println('add_header($key, $val)')
	ctx.headers = ctx.headers + '\r\n$key: $val'
	// println(ctx.headers)
}

// Returns the header data from the key
pub fn (ctx &Context) get_header(key string) string {
	return ctx.req.lheaders[key.to_lower()]
}

pub fn run<T>(port int) {
	mut app := T{}
	run_app<T>(mut app, port)
}

pub fn run_app<T>(mut app T, port int) {
	mut l := net.listen_tcp(port) or { panic('failed to listen') }
	println('[Vweb] Running app on http://localhost:$port')
	app.Context = Context{
		conn: 0
	}
	app.init_once()
	$for method in T.methods {
		$if method.return_type is Result {
			// check routes for validity
		}
	}
	for {
		mut conn := l.accept() or { panic('accept() failed') }
		// TODO: running handle_conn concurrently results in a race-condition
		handle_conn<T>(mut conn, mut app)
	}
}

fn handle_conn<T>(mut conn net.TcpConn, mut app T) {
	conn.set_read_timeout(30 * time.second)
	conn.set_write_timeout(30 * time.second)
	defer {
		conn.close() or { }
	}
	mut reader := io.new_buffered_reader(reader: io.make_reader(conn))
	page_gen_start := time.ticks()
	req := parse_request(mut reader) or { return }
	app.Context = Context{
		req: req
		conn: conn
		form: map[string]string{}
		static_files: app.static_files
		static_mime_types: app.static_mime_types
		page_gen_start: page_gen_start
	}
	if req.method in vweb.methods_with_form {
		if 'multipart/form-data' in req.lheaders['content-type'].split('; ') {
			boundary := req.lheaders['content-type'].split('; ').filter(it.starts_with('boundary '))
			if boundary.len != 1 {
				// TODO: send 400 error
				return
			}
			app.parse_multipart_form(req.data, boundary[0][9..])
		} else {
			app.parse_form(req.data)
		}
	}
	// Serve a static file if it is one
	// TODO: handle url parameters properly - for now, ignore them
	mut static_file_name := app.req.url
	// TODO: use urllib methods instead of manually parsing
	if static_file_name.contains('?') {
		static_file_name = static_file_name.all_before('?')
	}
	static_file := app.static_files[static_file_name]
	mime_type := app.static_mime_types[static_file_name]
	if static_file != '' && mime_type != '' {
		data := os.read_file(static_file) or {
			send_string(mut conn, vweb.http_404) or { }
			return
		}
		app.send_response_to_client(mime_type, data)
		unsafe { data.free() }
		return
	}
	app.init()
	// Call the right action
	$if debug {
		println('route matching...')
	}
	mut route_words_a := [][]string{}
	// TODO: use urllib methods instead of manually parsing
	mut url_words := req.url.split('/').filter(it != '')
	// Parse URL query
	if url_words.len > 0 && url_words.last().contains('?') {
		words := url_words.last().after('?').split('&')
		tmp_query := words.map(it.split('='))
		url_words[url_words.len - 1] = url_words.last().all_before('?')
		for data in tmp_query {
			if data.len == 2 {
				app.query[data[0]] = data[1]
			}
		}
	}
	mut vars := []string{cap: route_words_a.len}
	mut action := ''
	$for method in T.methods {
		$if method.return_type is Result {
			attrs := method.attrs
			route_words_a = [][]string{}
			// Get methods
			// Get is default
			mut req_method_str := '$req.method'
			if req.method == .post {
				if 'post' in attrs {
					route_words_a = attrs.filter(it.to_lower() != 'post').map(it[1..].split('/'))
				}
			} else if req.method == .put {
				if 'put' in attrs {
					route_words_a = attrs.filter(it.to_lower() != 'put').map(it[1..].split('/'))
				}
			} else if req.method == .patch {
				if 'patch' in attrs {
					route_words_a = attrs.filter(it.to_lower() != 'patch').map(it[1..].split('/'))
				}
			} else if req.method == .delete {
				if 'delete' in attrs {
					route_words_a = attrs.filter(it.to_lower() != 'delete').map(it[1..].split('/'))
				}
			} else if req.method == .head {
				if 'head' in attrs {
					route_words_a = attrs.filter(it.to_lower() != 'head').map(it[1..].split('/'))
				}
			} else if req.method == .options {
				if 'options' in attrs {
					route_words_a = attrs.filter(it.to_lower() != 'options').map(it[1..].split('/'))
				}
			} else {
				route_words_a = attrs.filter(it.to_lower() != 'get').map(it[1..].split('/'))
			}
			if attrs.len == 0 || (attrs.len == 1 && route_words_a.len == 0) {
				if url_words.len > 0 {
					// No routing for this method. If it matches, call it and finish matching
					// since such methods have a priority.
					// For example URL `/register` matches route `/:user`, but `fn register()`
					// should be called first.
					if (req_method_str == '' && url_words[0] == method.name && url_words.len == 1)
						|| (req_method_str == req.method.str() && url_words[0] == method.name
						&& url_words.len == 1) {
						$if debug {
							println('easy match method=$method.name')
						}
						app.$method(vars)

						return
					}
				} else if method.name == 'index' {
					// handle / to .index()
					$if debug {
						println('route to .index()')
					}
					app.$method(vars)

					return
				}
			} else {
				mut req_method := []string{}
				if route_words_a.len > 0 {
					for route_words_ in route_words_a {
						// cannot move to line initialize line because of C error with map(it.filter(it != ''))
						route_words := route_words_.filter(it != '')
						if route_words.len == 1 && route_words[0] in vweb.methods_without_first {
							req_method << route_words[0]
						}
						if url_words.len == route_words.len || (url_words.len >= route_words.len - 1
							&& route_words.len > 0 && route_words.last().ends_with('...')) {
							if req_method.len > 0 {
								if req_method_str.to_lower()[1..] !in req_method {
									continue
								}
							}
							// match `/:user/:repo/tree` to `/vlang/v/tree`
							mut matching := false
							mut unknown := false
							mut variables := []string{cap: route_words.len}
							if route_words.len == 0 && url_words.len == 0 {
								// index route
								matching = true
							}
							for i in 0 .. route_words.len {
								if url_words.len == i {
									variables << ''
									matching = true
									unknown = true
									break
								}
								if url_words[i] == route_words[i] {
									// no parameter
									matching = true
									continue
								} else if route_words[i].starts_with(':') {
									// is parameter
									if i < route_words.len && !route_words[i].ends_with('...') {
										// normal parameter
										variables << url_words[i]
									} else {
										// array parameter only in the end
										variables << url_words[i..].join('/')
									}
									matching = true
									unknown = true
									continue
								} else {
									matching = false
									break
								}
							}
							if matching && !unknown {
								// absolute router words like `/test/site`
								app.$method(vars)

								return
							} else if matching && unknown {
								// router words with paramter like `/:test/site`
								action = method.name
								vars = variables.clone()
							}
							req_method = []string{}
						}
					}
				}
			}
		}
	}
	if action == '' {
		// site not found
		send_string(mut conn, vweb.http_404) or { }
		return
	}
	$for method in T.methods {
		$if method.return_type is Result {
			// search again for method
			if action == method.name && method.attrs.len > 0 {
				// call action method
				if method.args.len == vars.len {
					app.$method(vars)
					return
				} else {
					eprintln('warning: uneven parameters count ($method.args.len) in `$method.name`, compared to the vweb route `$method.attrs` ($vars.len)')
				}
			}
		}
	}
}

// vweb intern function
pub fn (mut ctx Context) parse_form(s string) {
	if ctx.req.method !in vweb.methods_with_form {
		return
	}
	// pos := s.index('\r\n\r\n')
	// if pos > -1 {
	mut str_form := s // [pos..s.len]
	str_form = str_form.replace('+', ' ')
	words := str_form.split('&')
	for word in words {
		$if debug {
			println('parse form keyval="$word"')
		}
		keyval := word.trim_space().split('=')
		if keyval.len != 2 {
			continue
		}
		key := urllib.query_unescape(keyval[0]) or { continue }
		val := urllib.query_unescape(keyval[1]) or { continue }
		$if debug {
			println('http form "$key" => "$val"')
		}
		ctx.form[key] = val
	}
	// }
	// todo: parse form-data and application/json
	// ...
}

// vweb intern function
[manualfree]
pub fn (mut ctx Context) parse_multipart_form(s string, b string) {
	if ctx.req.method !in vweb.methods_with_form {
		return
	}
	a := s.split('$b')[1..]
	fields := a[..a.len - 1]
	for field in fields {
		lines := field.split_into_lines()[1..]
		mut l := 0
		// Parse name
		disposition_data := lines[l].split('; ')[1..]
		l++
		name := disposition_data[0][6..disposition_data[0].len - 1]
		// Parse files
		if disposition_data.len > 1 {
			filename := disposition_data[1][10..disposition_data[1].len - 1]
			ct := lines[l].split(': ')[1]
			l++
			if name !in ctx.files {
				ctx.files[name] = []FileData{}
			}
			mut sb := strings.new_builder(field.len)
			for i in l + 1 .. lines.len - 1 {
				sb.writeln(lines[i])
			}
			ctx.files[name] << FileData{
				filename: filename
				content_type: ct
				data: sb.str()
			}
			unsafe {
				sb.free()
			}
			continue
		}
		mut sb := strings.new_builder(field.len)
		for i in l + 1 .. lines.len - 1 {
			sb.writeln(lines[i])
		}
		ctx.form[name] = sb.str()
		unsafe {
			disposition_data.free()
			name.free()
			sb.free()
		}
	}
}

fn (mut ctx Context) scan_static_directory(directory_path string, mount_path string) {
	files := os.ls(directory_path) or { panic(err.msg) }
	if files.len > 0 {
		for file in files {
			full_path := directory_path + '/' + file
			if os.is_dir(full_path) {
				ctx.scan_static_directory(full_path, mount_path + '/' + file)
			} else if file.contains('.') && !file.starts_with('.') && !file.ends_with('.') {
				ext := os.file_ext(file)
				// Rudimentary guard against adding files not in mime_types.
				// Use serve_static directly to add non-standard mime types.
				if ext in vweb.mime_types {
					ctx.serve_static(mount_path + '/' + file, full_path, vweb.mime_types[ext])
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

// Serves a file static
// `url` is the access path on the site, `file_path` is the real path to the file, `mime_type` is the file type 
pub fn (mut ctx Context) serve_static(url string, file_path string, mime_type string) {
	ctx.static_files[url] = file_path
	ctx.static_mime_types[url] = mime_type
}

// Returns the ip address from the current user
pub fn (ctx &Context) ip() string {
	mut ip := ctx.req.lheaders['x-forwarded-for']
	if ip == '' {
		ip = ctx.req.lheaders['x-real-ip']
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
	ctx.form_error = s
}

fn strip(s string) string {
	// strip('\nabc\r\n') => 'abc'
	return s.trim('\r\n')
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
