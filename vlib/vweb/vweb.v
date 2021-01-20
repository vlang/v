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
	mime_types              = {
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
	headers           string // response headers
	done              bool
	page_gen_start    i64
	form_error        string
	chunked_transfer  bool
	max_chunk_len     int = 20
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

pub fn (mut ctx Context) send_response_to_client(mimetype string, res string) bool {
	if ctx.done {
		return false
	}
	ctx.done = true
	mut sb := strings.new_builder(1024)
	defer {
		sb.free()
	}
	sb.write('HTTP/1.1 $ctx.status')
	sb.write('\r\nContent-Type: $mimetype')
	sb.write('\r\nContent-Length: $res.len')
	if ctx.chunked_transfer {
		sb.write('\r\nTransfer-Encoding: chunked')
	}
	sb.write(ctx.headers)
	sb.write('\r\n')
	sb.write(headers_close)
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
			sb.write(chunk.len.hex())
			sb.write('\r\n$chunk\r\n')
		}
		sb.write('0\r\n\r\n') // End of chunks
	} else {
		sb.write(res)
	}
	s := sb.str()
	defer {
		s.free()
	}
	send_string(mut ctx.conn, s) or { return false }
	return true
}

pub fn (mut ctx Context) html(s string) Result {
	ctx.send_response_to_client('text/html', s)
	return Result{}
}

pub fn (mut ctx Context) text(s string) Result {
	ctx.send_response_to_client('text/plain', s)
	return Result{}
}

pub fn (mut ctx Context) json(s string) Result {
	ctx.send_response_to_client('application/json', s)
	return Result{}
}

pub fn (mut ctx Context) ok(s string) Result {
	ctx.send_response_to_client(ctx.content_type, s)
	return Result{}
}

pub fn (mut ctx Context) redirect(url string) Result {
	if ctx.done {
		return Result{}
	}
	ctx.done = true
	send_string(mut ctx.conn, 'HTTP/1.1 302 Found\r\nLocation: $url$ctx.headers\r\n$headers_close') or {
		return Result{}
	}
	return Result{}
}

pub fn (mut ctx Context) not_found() Result {
	if ctx.done {
		return Result{}
	}
	ctx.done = true
	send_string(mut ctx.conn, http_404) or { }
	return Result{}
}

pub fn (mut ctx Context) enable_chunked_transfer(max_chunk_len int) {
	ctx.chunked_transfer = true
	ctx.max_chunk_len = max_chunk_len
}

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

pub fn (mut ctx Context) set_cookie_old(key string, val string) {
	// TODO support directives, escape cookie value (https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie)
	// ctx.add_header('Set-Cookie', '${key}=${val};  Secure; HttpOnly')
	ctx.add_header('Set-Cookie', '$key=$val; HttpOnly')
}

pub fn (mut ctx Context) set_content_type(typ string) {
	ctx.content_type = typ
}

pub fn (mut ctx Context) set_cookie_with_expire_date(key string, val string, expire_date time.Time) {
	ctx.add_header('Set-Cookie', '$key=$val;  Secure; HttpOnly; expires=$expire_date.utc_string()')
}

pub fn (ctx &Context) get_cookie(key string) ?string { // TODO refactor
	mut cookie_header := ctx.get_header('cookie')
	if cookie_header == '' {
		cookie_header = ctx.get_header('Cookie')
	}
	cookie_header = ' ' + cookie_header
	// println('cookie_header="$cookie_header"')
	// println(ctx.req.headers)
	cookie := if cookie_header.contains(';') { cookie_header.find_between(' $key=', ';') } else { cookie_header.find_between(' $key=',
			'\r') }
	if cookie != '' {
		return cookie.trim_space()
	}
	return error('Cookie not found')
}

pub fn (mut ctx Context) set_status(code int, desc string) {
	if code < 100 || code > 599 {
		ctx.status = '500 Internal Server Error'
	} else {
		ctx.status = '$code $desc'
	}
}

pub fn (mut ctx Context) add_header(key string, val string) {
	// println('add_header($key, $val)')
	ctx.headers = ctx.headers + '\r\n$key: $val'
	// println(ctx.headers)
}

pub fn (ctx &Context) get_header(key string) string {
	return ctx.req.headers[key]
}

// fn handle_conn(conn net.Socket) {
// println('handle')
// }
pub fn run<T>(port int) {
	mut app := T{}
	run_app<T>(mut app, port)
}

pub fn run_app<T>(mut app T, port int) {
	println('Running a Vweb app on http://localhost:$port')
	mut l := net.listen_tcp(port) or { panic('failed to listen') }
	app.Context = Context{
		conn: 0
	}
	app.init_once()
	$for method in T.methods {
		$if method.return_type is Result {
			// check routes for validity
		}
	}
	// app.reset()
	for {
		mut conn := l.accept() or { panic('accept() failed') }
		handle_conn<T>(mut conn, mut app)
		// app.vweb.page_gen_time = time.ticks() - t
		// eprintln('handle conn() took ${time.ticks()-t}ms')
		// message := readall(conn)
		// println(message)
		/*
		if message.len > max_http_post_size {
			println('message.len = $message.len > max_http_post_size')
			conn.send_string(http_500) or {}
			conn.close() or {}
			continue
		}
		*/
		// lines := message.split_into_lines()
		// println(lines)
		/*
		if lines.len < 2 {
			conn.send_string(http_500) or {}
			conn.close() or {}
			continue
		}
		*/
	}
}

fn handle_conn<T>(mut conn net.TcpConn, mut app T) {
	conn.set_read_timeout(1 * time.second)
	defer {
		conn.close() or { }
	}
	// fn handle_conn<T>(conn net.Socket, app_ T) T {
	// mut app := app_
	// first_line := strip(lines[0])
	mut reader := io.new_buffered_reader(reader: io.make_reader(conn))
	page_gen_start := time.ticks()
	first_line := reader.read_line() or {
		println('Failed first_line')
		return
	}
	$if debug {
		println('firstline="$first_line"')
	}
	// Parse the first line
	// "GET / HTTP/1.1"
	// first_line := s.all_before('\n')
	vals := first_line.split(' ')
	if vals.len < 2 {
		println('no vals for http')
		send_string(mut conn, http_500) or { }
		return
	}
	mut headers := []string{}
	mut body := ''
	mut in_headers := true
	mut len := 0
	// for line in lines[1..] {
	for lindex in 0 .. 100 {
		// println(j)
		line := reader.read_line() or {
			println('Failed read_line $lindex')
			break
		}
		sline := strip(line)
		if sline == '' {
			// if in_headers {
			// End of headers, no body => exit
			if len == 0 {
				break
			}
			//} //else {
			// End of body
			// break
			//}
			// read body
			read_body := io.read_all(reader: reader) or { []byte{} }
			body += read_body.bytestr()
			break
		}
		if in_headers {
			// println(sline)
			headers << sline
			if sline.starts_with('Content-Length') {
				len = sline.all_after(': ').int()
				// println('GOT CL=$len')
			}
		}
	}
	req := http.Request{
		headers: http.parse_headers(headers) // s.split_into_lines())
		data: strip(body)
		ws_func: 0
		user_ptr: 0
		method: http.method_from_str(vals[0])
		url: vals[1]
	}
	$if debug {
		println('req.headers = ')
		println(req.headers)
		println('req.data="$req.data"')
		// println('vweb action = "$action"')
	}
	// mut app := T{
	app.Context = Context{
		req: req
		conn: conn
		form: map[string]string{}
		static_files: app.static_files
		static_mime_types: app.static_mime_types
		page_gen_start: page_gen_start
	}
	// }
	if req.method in methods_with_form {
		app.parse_form(req.data)
	}
	if vals.len < 2 {
		$if debug {
			println('no vals for http')
		}
		return
	}
	// Serve a static file if it is one
	// TODO: handle url parameters properly - for now, ignore them
	mut static_file_name := app.req.url
	if static_file_name.contains('?') {
		static_file_name = static_file_name.all_before('?')
	}
	static_file := app.static_files[static_file_name]
	mime_type := app.static_mime_types[static_file_name]
	if static_file != '' && mime_type != '' {
		data := os.read_file(static_file) or {
			send_string(mut conn, http_404) or { }
			return
		}
		app.send_response_to_client(mime_type, data)
		data.free()
		return
	}
	app.init()
	// Call the right action
	$if debug {
		println('route matching...')
	}
	// t := time.ticks()
	// mut action := ''
	mut route_words_a := [][]string{}
	// mut url_words := vals[1][1..].split('/').filter(it != '')
	x := vals[1][1..].split('/')
	mut url_words := x.filter(it != '')
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
					if (req_method_str == '' &&
						url_words[0] == method.name && url_words.len == 1) ||
						(req_method_str == req.method.str() && url_words[0] == method.name && url_words.len ==
						1)
					{
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
						if route_words.len == 1 && route_words[0] in methods_without_first {
							req_method << route_words[0]
						}
						if url_words.len == route_words.len ||
							(url_words.len >= route_words.len - 1 && route_words.len > 0 && route_words.last().ends_with('...'))
						{
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
		send_string(mut conn, http_404) or { }
		return
	}
	$for method in T.methods {
		$if method.return_type is Result {
			// search again for method
			if action == method.name && method.attrs.len > 0 {
				// call action method
				if method.args.len == vars.len {
					app.$method(vars)
				} else {
					eprintln('warning: uneven parameters count ($method.args.len) in `$method.name`, compared to the vweb route `$method.attrs` ($vars.len)')
				}
			}
		}
	}
}

pub fn (mut ctx Context) parse_form(s string) {
	if ctx.req.method !in methods_with_form {
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

fn (mut ctx Context) scan_static_directory(directory_path string, mount_path string) {
	files := os.ls(directory_path) or { panic(err) }
	if files.len > 0 {
		for file in files {
			full_path := directory_path + '/' + file
			if os.is_dir(full_path) {
				ctx.scan_static_directory(full_path, mount_path + '/' + file)
			} else if file.contains('.') && !file.starts_with('.') && !file.ends_with('.') {
				ext := os.file_ext(file)
				// Rudimentary guard against adding files not in mime_types.
				// Use serve_static directly to add non-standard mime types.
				if ext in mime_types {
					ctx.serve_static(mount_path + '/' + file, full_path, mime_types[ext])
				}
			}
		}
	}
}

pub fn (mut ctx Context) handle_static(directory_path string) bool {
	if ctx.done || !os.exists(directory_path) {
		return false
	}
	dir_path := directory_path.trim_space().trim_right('/')
	mut mount_path := ''
	if dir_path != '.' && os.is_dir(dir_path) {
		// Mount point hygene, "./assets" => "/assets".
		mount_path = '/' + dir_path.trim_left('.').trim('/')
	}
	ctx.scan_static_directory(dir_path, mount_path)
	return true
}

pub fn (mut ctx Context) serve_static(url string, file_path string, mime_type string) {
	ctx.static_files[url] = file_path
	ctx.static_mime_types[url] = mime_type
}

pub fn (ctx &Context) ip() string {
	mut ip := ctx.req.headers['X-Forwarded-For']
	if ip == '' {
		ip = ctx.req.headers['X-Real-IP']
	}
	if ip.contains(',') {
		ip = ip.all_before(',')
	}
	if ip == '' {
		ip = ctx.conn.peer_ip() or { '' }
	}
	return ip
}

pub fn (mut ctx Context) error(s string) {
	ctx.form_error = s
}

/*
fn readall(conn net.Socket) string {
	// read all message from socket
	//printf("waitall=%d\n", C.MSG_WAITALL)
	mut message := ''
	buf := [1024]byte
	for {
		n := C.recv(conn.sockfd, buf, 1024, 0)
		m := conn.crecv(buf, 1024)
		message += unsafe { byteptr(buf).vstring_with_len(m) }
		if message.len > max_http_post_size { break }
		if n == m { break }
	}
	return message
}
*/
fn strip(s string) string {
	// strip('\nabc\r\n') => 'abc'
	return s.trim('\r\n')
}

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

pub type RawHtml = string

fn send_string(mut conn net.TcpConn, s string) ? {
	conn.write(s.bytes()) ?
}
