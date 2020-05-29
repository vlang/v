// Copyright (c) 2019-2020 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module vweb

import os
import net
import net.http
import net.urllib
import strings

pub const (
	methods_with_form = ['POST', 'PUT', 'PATCH']
	method_all = ['GET','POST','PUT','PATCH','DELETE']
	header_server = 'Server: VWeb\r\n'
	header_connection_close = 'Connection: close\r\n'
	headers_close = '${header_server}${header_connection_close}\r\n'
	http_404 = 'HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n${headers_close}404 Not Found'
	http_500 = 'HTTP/1.1 500 Internal Server Error\r\nContent-Type: text/plain\r\n${headers_close}500 Internal Server Error'
	mime_types = {
		'.css': 'text/css; charset=utf-8',
		'.gif': 'image/gif',
		'.htm': 'text/html; charset=utf-8',
		'.html': 'text/html; charset=utf-8',
		'.jpg': 'image/jpeg',
		'.js': 'application/javascript',
		'.wasm': 'application/wasm',
		'.pdf': 'application/pdf',
		'.png': 'image/png',
		'.svg': 'image/svg+xml',
		'.xml': 'text/xml; charset=utf-8'
	}
	max_http_post_size = 1024 * 1024
	default_port = 8080
)

pub struct Context {
mut:
	static_files map[string]string
	static_mime_types map[string]string
pub:
	req http.Request
	conn net.Socket
	// TODO Response
pub mut:
	form map[string]string
	headers string // response headers
	done bool
}

fn (mut ctx Context) send_response_to_client(mimetype string, res string) bool {
	if ctx.done { return false }
	ctx.done = true
	mut sb := strings.new_builder(1024)
	sb.write('HTTP/1.1 200 OK\r\nContent-Type: ') sb.write(mimetype)
	sb.write('\r\nContent-Length: ')              sb.write(res.len.str())
	sb.write(ctx.headers)
	sb.write('\r\n')
	sb.write(headers_close)
	sb.write(res)
	ctx.conn.send_string(sb.str()) or { return false }
	sb.free()
	return true
}

pub fn (mut ctx Context) html(s string) {
	ctx.send_response_to_client('text/html', s)
}

pub fn (mut ctx Context) text(s string) {
	ctx.send_response_to_client('text/plain', s)
}

pub fn (mut ctx Context) json(s string) {
	ctx.send_response_to_client('application/json', s)
}

pub fn (mut ctx Context) redirect(url string) {
	if ctx.done { return }
	ctx.done = true
	ctx.conn.send_string('HTTP/1.1 302 Found\r\nLocation: ${url}${ctx.headers}\r\n${headers_close}') or { return }
}

pub fn (mut ctx Context) not_found(s string) {
	if ctx.done { return }
	ctx.done = true
	ctx.conn.send_string(http_404) or { return }
}

pub fn (mut ctx Context) set_cookie(key, val string) {
	// TODO support directives, escape cookie value (https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Set-Cookie)
	//println('Set-Cookie $key=$val')
	ctx.add_header('Set-Cookie', '${key}=${val};  Secure; HttpOnly')
}

pub fn (ctx &Context) get_cookie(key string) ?string { // TODO refactor
	mut cookie_header := ctx.get_header('cookie')
	if cookie_header == '' {
		cookie_header = ctx.get_header('Cookie')
	}
	cookie_header = ' ' + cookie_header
	//println('cookie_header="$cookie_header"')
	//println(ctx.req.headers)
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

pub fn (mut ctx Context) add_header(key, val string) {
	//println('add_header($key, $val)')
	ctx.headers = ctx.headers + '\r\n$key: $val'
	//println(ctx.headers)
}

pub fn (ctx &Context) get_header(key string) string {
	return ctx.req.headers[key]
}

//fn handle_conn(conn net.Socket) {
	//println('handle')

//}

pub fn foo<T>() {

}

pub fn run<T>(port int) {
//pub fn run<T>(app mut T, port int) {
	println('Running a Vweb app on http://localhost:$port ...')
	l := net.listen(port) or { panic('failed to listen') }
	mut app := T{}
	app.vweb = Context{}
	app.init()
	//app.reset()
	for {
		conn := l.accept() or { panic('accept() failed') }
		handle_conn<T>(conn, mut app)
		//foobar<T>()
		// TODO move this to handle_conn<T>(conn, app)
		//message := readall(conn)
		//println(message)
/*
		if message.len > max_http_post_size {
			println('message.len = $message.len > max_http_post_size')
			conn.send_string(http_500) or {}
			conn.close() or {}
			continue
		}
		*/

		//lines := message.split_into_lines()
		//println(lines)

/*
		if lines.len < 2 {
			conn.send_string(http_500) or {}
			conn.close() or {}
			continue
		}
		*/
	}
}

fn handle_conn<T>(conn net.Socket, app mut T) {
	//first_line := strip(lines[0])
	first_line := conn.read_line()
	println('firstline="$first_line"')
	$if debug { println(first_line) }
	// Parse the first line
	// "GET / HTTP/1.1"
	//first_line := s.all_before('\n')
	vals := first_line.split(' ')
	if vals.len < 2 {
		println('no vals for http')
		conn.send_string(http_500) or {}
		conn.close() or {}
		return
		//continue
	}
	mut headers := []string{}
	mut body := ''
	mut in_headers := true
	mut len := 0
	mut body_len := 0
	//for line in lines[1..] {
	for _ in 0..100 {
		//println(j)
		line := conn.read_line()
		sline := strip(line)
		if sline == '' {
			//if in_headers {
				// End of headers, no body => exit
				if len == 0 {
					break
				}
			//} //else {
				// End of body
				//break
			//}
			//println('HHH')
			in_headers = false
		}
		if in_headers {
			//println(sline)
			headers << sline
			if sline.starts_with('Content-Length') {
				len = sline.all_after(': ').int()
				//println('GOT CL=$len')
			}
		} else {
			body += sline + '\r\n'
			body_len += body.len
			if body_len >= len {
				break
			}
			//println('body:$body')
		}
	}

	mut action := vals[1][1..].all_before('/')
	if action.contains('?') {
		action = action.all_before('?')
	}
	if action == '' {
		action = 'index'
	}
	req := http.Request{
		headers: http.parse_headers(headers) //s.split_into_lines())
		data: strip(body)
		ws_func: 0
		user_ptr: 0
		method: vals[0]
		url: vals[1]
	}
	$if debug {
		println('req.headers = ')
		println(req.headers)
		println('req.data="$req.data"' )
		println('vweb action = "$action"')
	}
	//mut app := T{
	app.vweb = Context{
		req: req
		conn: conn
		form: map[string]string
		static_files: app.vweb.static_files
		static_mime_types: app.vweb.static_mime_types
	}
	//}
	if req.method in methods_with_form {
		app.vweb.parse_form(req.data)
	}
	if vals.len < 2 {
		$if debug {
			println('no vals for http')
		}
		conn.close() or {}
		return
		//continue
	}

	// Serve a static file if it's one
	static_file := app.vweb.static_files[app.vweb.req.url]
	mime_type := app.vweb.static_mime_types[app.vweb.req.url]

	if static_file != '' && mime_type != '' {
		data := os.read_file(static_file) or {
			conn.send_string(http_404) or {}
			return
		}
		app.vweb.send_response_to_client(mime_type, data)
		return
	}

	// Call the right action
	$if debug {
		println('action=$action')
	}
	app.$action()
	/*
	app.$action() or {
		conn.send_string(http_404) or {}
	}
	*/
	conn.close() or {}
	app.reset()
}

fn (mut ctx Context) parse_form(s string) {
	if ctx.req.method !in methods_with_form {
		return
	}
	//pos := s.index('\r\n\r\n')
	//if pos > -1 {
	mut str_form := s//[pos..s.len]
	str_form = str_form.replace('+', ' ')
	words := str_form.split('&')
	for word in words {
		$if debug {
			println('parse form keyval="$word"')
		}
		keyval := word.trim_space().split('=')
		if keyval.len != 2 { continue }
		key := keyval[0]
		val := urllib.query_unescape(keyval[1]) or {
			continue
		}
		$if debug {
			println('http form "$key" => "$val"')
		}
		ctx.form[key] = val
	}
	//}
	// todo: parse form-data and application/json
	// ...
}

fn (mut ctx Context) scan_static_directory(directory_path, mount_path string) {
	files := os.ls(directory_path) or { panic(err) }

	if files.len > 0 {
		for file in files {

			if os.is_dir(file) {
				ctx.scan_static_directory(directory_path + '/' + file, mount_path + '/' + file)
			} else if file.contains('.') && ! file.starts_with('.') && ! file.ends_with('.') {
				ext := os.file_ext(file)

				// Rudimentary guard against adding files not in mime_types.
				// Use serve_static directly to add non-standard mime types.
				if ext in mime_types {
					ctx.serve_static(mount_path + '/' + file, directory_path + '/' + file, mime_types[ext])
				}
			}
		}
	}
}

pub fn (mut ctx Context) handle_static(directory_path string) bool {
	if ctx.done || ! os.exists(directory_path) {
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

pub fn (mut ctx Context) serve_static(url, file_path, mime_type string) {
	ctx.static_files[url] = file_path
	ctx.static_mime_types[url] = mime_type
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
		message += string( byteptr(buf), m )
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
