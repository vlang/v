// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module vweb

import io
import net
import net.http
import net.urllib
import time

// A type which don't get filtered inside templates
pub type RawHtml = string

// A dummy structure that returns from routes to indicate that you actually sent something to a user
[noinit]
pub struct Result {}

// VWeb Router Config
pub struct Config {
pub:
	server_header string = 'VWeb'

	read_buffer_size  int = 4096
	write_buffer_size int = 4096
	read_timeout      time.Duration = 30 * time.second
	write_timeout     time.Duration = 30 * time.second
}

// VWeb Router
[noinit]
pub struct Router<T> {
	config Config
	routes map[string]Route
mut:
	static_files map[string]StaticFile
pub mut:
	app T
}

pub fn new<T>(app T, config ...Config) ?Router<T> {

	// Parsing methods attributes for faster matching (this is conjecture, just implemented TODO)
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

	return Router<T>{
		// Adding default config to end
		config: if config.len > 0 { config[0] } else { Config{} }
		routes: routes
		static_files: map[string]StaticFile{}
		app: app
	}
}

pub fn (mut router Router<T>) serve_static(mount_at string, dir string) ? {
	return error('not implemented')
}

[manualfree]
pub fn (mut router Router<T>) listen(addr string) ? {
	mut listener := net.listen_tcp(.ip6, addr) or {
		return error('failed to listen $err.code $err')
	}

	println('[VWeb] Running app on `$addr`')
	for {
		// Create a new app object for each connection, copy global data like db connections
		mut app := T{}
		$for field in T.fields {
			if 'vweb_global' in field.attrs || field.is_shared {
				app.$(field.name) = router.app.$(field.name)
			}
		}
		mut conn := listener.accept() or {
			// failures should not panic
			eprintln('accept() failed with error: $err.msg')
			continue
		}

		go router.handle_connection<T>(mut conn, mut &app)
	}
}

pub fn (mut router Router<T>) test(req http.Request, timeout ...time.Duration) ?http.Response {
	return error('not implemented')
}

[manualfree]
fn (mut router Router<T>) handle_connection<T>(mut conn net.TcpConn, mut app T) {
	conn.set_read_timeout(router.config.read_timeout)
	conn.set_write_timeout(router.config.write_timeout)
	defer {
		conn.close() or {}
		unsafe {
			free(app)
		}
	}

	mut reader := io.new_buffered_reader(reader: conn, cap: router.config.read_buffer_size)
	defer {
		reader.free()
	}

	request_start := time.ticks()

	// REQUEST
	request := http.parse_request(mut reader) or {
		// ? Prevents errors from being thrown when BufferedReader is empty
		if '$err' != 'none' {
			eprintln('error parsing request: $err')
		}
		return
	}

	// URL PARSE
	url := urllib.parse(request.url) or {
		eprintln('error parsing path: $err')
		return
	}

	// STATIC
	if router.serve_if_static<T>(mut app, url) {
		// successfully served a static file
		return
	}

	// FORM
	form, files := parse_form_from_request(request) or {
		// Bad request
		conn.write(http_400.bytes()) or {}
		return
	}

	// QUERY
	query := parse_query_from_url(url)
	url_words := url.path.split('/').filter(it != '')

	// Setting up context
	app.Context = Context{
		request_start: request_start
		request: request
		conn: conn
		form: form
		query: query
		files: files
		response: http.new_response(
			header: http.new_header_from_map({
				http.CommonHeader.server:     router.config.server_header
				http.CommonHeader.connection: 'close'
			})
		)
	}

	// Calling middleware...
	app.before_request()

	// Matching route
	$for method in T.methods {
		$if method.return_type is Result {
			route := router.routes[method.name] or {
				eprintln('parsed attributes for the `$method.name` are not found, skipping...')
				Route{}
			}

			// Skip if the HTTP request method does not match the attributes
			if request.method in route.methods {
				// Used for route matching
				route_words := route.path.split('/').filter(it != '')

				// Route immediate matches first
				// For example URL `/register` matches route `/:user`, but `fn register()`
				// should be called first.
				if !route.path.contains('/:') && url_words == route_words {
					// We found a match
					app.$method()
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
	conn.write(http_404.bytes()) or {}
}
