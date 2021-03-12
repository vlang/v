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
	methods_with_form       = [http.Method.post, .put, .patch]
	header_server           = 'Server: VWeb\r\n'
	header_connection_close = 'Connection: close\r\n'
	headers_close           = '$header_server$header_connection_close\r\n'
	// TODO: use http.response structs
	http_400                = 'HTTP/1.1 400 Bad Request\r\nContent-Type: text/plain\r\nContent-Length: 15\r\n${headers_close}400 Bad Request'
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

struct UnexpectedExtraAttributeError {
	msg  string
	code int
}

struct MultiplePathAttributesError {
	msg  string = 'Expected at most one path attribute'
	code int
}

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

pub fn run<T>(conf &Config) {
	mut app := T{}
	run_app<T>(mut app, conf)
}

pub fn run_app<T>(mut app T, conf &Config) {
	mut l := net.listen_tcp(conf.port) or { panic('failed to listen') }
	println('[Vweb] Running app on http://localhost:$conf.port')
	$for method in T.methods {
		$if method.return_type is Result {
			// check routes for validity
		}
	}
	for {
		mut conn := l.accept() or { panic('accept() failed') }
		// TODO: running handle_conn concurrently can cause a race-condition
		//       - make `app` shared
		//       - use kqueue / epoll
		go handle_conn<T>(mut conn, mut app, conf)
	}
}

[manualfree]
fn handle_conn<T>(mut conn net.TcpConn, mut app T, conf &Config) {
	conn.set_read_timeout(30 * time.second)
	conn.set_write_timeout(30 * time.second)
	defer {
		conn.close() or {}
	}
	mut reader := io.new_buffered_reader(reader: io.make_reader(conn))
	defer {
		reader.free()
	}
	page_gen_start := time.ticks()
	req := parse_request(mut reader) or {
		eprintln('error parsing request: $err')
		return
	}
	mut ctx := Context{
		req: req
		conn: conn
		form: map[string]string{}
		// TODO: clone?
		static_files: conf.static_files
		static_mime_types: conf.static_mime_types
		page_gen_start: page_gen_start
	}
	if req.method in vweb.methods_with_form {
		if 'multipart/form-data' in req.lheaders['content-type'].split('; ') {
			boundary := req.lheaders['content-type'].split('; ').filter(it.starts_with('boundary='))
			if boundary.len != 1 {
				send_string(mut conn, vweb.http_400) or {}
				return
			}
			form, files := parse_multipart_form(req.data, boundary[0][9..])
			for k, v in form {
				ctx.form[k] = v
			}
			for k, v in files {
				ctx.files[k] = v
			}
		} else {
			form := parse_form(req.data)
			for k, v in form {
				ctx.form[k] = v
			}
		}
	}
	// Serve a static file if it is one
	// TODO: get the real path
	url := urllib.parse(ctx.req.url.to_lower()) or {
		eprintln('error parsing path: $err')
		return
	}
	if serve_static<T>(mut ctx, url) {
		// successfully served a static file
		return
	}

	// Call the right action
	$if debug {
		println('route matching...')
	}
	url_words := url.path.split('/').filter(it != '')
	// copy query args to ctx.query
	for k, v in url.query().data {
		ctx.query[k] = v.data[0]
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
			if ctx.req.method in http_methods {
				// Route immediate matches first
				// For example URL `/register` matches route `/:user`, but `fn register()`
				// should be called first.
				if !route_path.contains('/:') && url_words == route_words {
					// We found a match
					app.$method(mut ctx)
					return
				}

				if url_words.len == 0 && route_words == ['index'] && method.name == 'index' {
					app.$method(mut ctx)
					return
				}

				if params := route_matches(url_words, route_words) {
					method_args = params.clone()
					if method_args.len + 1 != method.args.len {
						eprintln('warning: uneven parameters count ($method.args.len) in `$method.name`, compared to the vweb route `$method.attrs` ($method_args.len)')
					}
					app.$method(mut ctx, method_args)
					return
				}
			}
		}
	}
	// site not found
	send_string(mut conn, vweb.http_404) or {}
}

fn route_matches(url_words []string, route_words []string) ?[]string {
	// URL path should be at least as long as the route path
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
	if !route_words[route_words.len - 1].ends_with('...') {
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
fn serve_static<T>(mut ctx Context, url urllib.URL) bool {
	// TODO: handle url parameters properly - for now, ignore them
	static_file := ctx.static_files[url.path]
	mime_type := ctx.static_mime_types[url.path]
	if static_file == '' || mime_type == '' {
		return false
	}
	data := os.read_file(static_file) or {
		send_string(mut ctx.conn, vweb.http_404) or {}
		return true
	}
	ctx.send_response_to_client(mime_type, data)
	unsafe { data.free() }
	return true
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
