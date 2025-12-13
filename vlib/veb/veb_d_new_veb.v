// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module veb

import fasthttp
import net.http
import time
import net.urllib

struct RequestParams {
	global_app         voidptr
	controllers_sorted []&ControllerPath
	routes             &map[string]Route
}

const http_ok_response = 'HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'.bytes()

pub fn run_at[A, X](mut global_app A, params RunParams) ! {
	run_new[A, X](mut global_app, params.port)!
}

// run_new - start a new veb server using the parallel fasthttp backend.
pub fn run_new[A, X](mut global_app A, port int) ! {
	// gapp = global_app
	if port <= 0 || port > 65535 {
		return error('invalid port number `${port}`, it should be between 1 and 65535')
	}

	// Generate routes and controllers just like the original run() function.
	routes := generate_routes[A, X](global_app)!
	controllers_sorted := check_duplicate_routes_in_controllers[A](global_app, routes)!

	// Allocate params on the heap to keep it valid for the server lifetime
	params := &RequestParams{
		global_app:         global_app
		controllers_sorted: controllers_sorted
		routes:             &routes
	}

	// Configure and run the fasthttp server
	mut server := fasthttp.new_server(fasthttp.ServerConfig{
		port:      port
		handler:   parallel_request_handler[A, X]
		user_data: voidptr(params)
	}) or {
		eprintln('Failed to create server: ${err}')
		return
	}
	println('[veb] Running multi-threaded app on http://localhost:${port}/')
	flush_stdout()
	server.run() or { panic(err) }
}

// const test_text = 'test'

fn parallel_request_handler[A, X](req fasthttp.HttpRequest) ![]u8 {
	/*
	if true {
		return test_text.bytes()
	}
	*/
	// Get parameters from user_data - copy to avoid use-after-free
	params := unsafe { *(&RequestParams(req.user_data)) }
	mut global_app := unsafe { &A(params.global_app) }
	// println('parallel_request_handler() params.routes=${params.routes}')
	// println('global_app=$global_app')
	// println('params=$params')
	// println('req=$req')
	// println('buffer=${req.buffer.bytestr()}')
	s := req.buffer.bytestr()
	// method := unsafe { tos(&req.buffer[req.method.start], req.method.len) }
	// println('method=${method}')
	// path := unsafe { tos(&req.buffer[req.path.start], req.path.len) }
	// println('path=${path}')
	client_fd := req.client_conn_fd

	// Parse the raw request bytes into a standard `http.Request`.
	req2 := http.parse_request_head_str(s.clone()) or {
		eprintln('[veb] Failed to parse request: ${err}')
		println('s=')
		println(s)
		println('==============')
		return http_ok_response // tiny_bad_request_response
	}
	// Create and populate the `veb.Context`.
	completed_context := handle_request_and_route[A, X](mut global_app, req2, client_fd,
		params.routes, params.controllers_sorted)
	// Serialize the final `http.Response` into a byte array.
	if completed_context.takeover {
		eprintln('[veb] WARNING: ctx.takeover_conn() was called, but this is not supported by this server backend. The connection will be closed after this response.')
	}
	// The fasthttp server expects a complete response buffer to be returned.
	return completed_context.res.bytes()
} // handle_request_and_route is a unified function that creates the context,

// runs middleware, and finds the correct route for a request.
fn handle_request_and_route[A, X](mut app A, req http.Request, client_fd int, routes &map[string]Route, controllers []&ControllerPath) &Context {
	/*
	// Create a `net.TcpConn` from the file descriptor for context compatibility.
	mut conn := &net.TcpConn{
		sock:        net.tcp_socket_from_handle_raw(client_fd)
		handle:      client_fd
		is_blocking: false // vanilla_http_server ensures this
	}
	*/
	// Create and populate the `veb.Context` from the request.
	mut url := urllib.parse(req.url) or {
		// This should be rare if http.parse_request succeeded.
		mut bad_ctx := &Context{
			req: req
			// conn: conn
		}
		bad_ctx.not_found()
		return bad_ctx
	}
	query := parse_query_from_url(url)
	form, files := parse_form_from_request(req) or {
		mut bad_ctx := &Context{
			req: req
			// conn: conn
		}
		bad_ctx.request_error('Failed to parse form data: ${err.msg()}')
		return bad_ctx
	}
	host_with_port := req.header.get(.host) or { '' }
	host, _ := urllib.split_host_port(host_with_port)
	mut ctx := &Context{
		req:            req
		page_gen_start: time.ticks()
		// conn:           conn
		query: query
		form:  form
		files: files
	}
	if connection_header := req.header.get(.connection) {
		if connection_header.to_lower() == 'close' {
			ctx.client_wants_to_close = true
		}
	}
	$if A is StaticApp {
		ctx.custom_mime_types = app.static_mime_types.clone()
	}
	// Match controller paths first
	$if A is ControllerInterface {
		if completed_context := handle_controllers[X](controllers, ctx, mut url, host) {
			return completed_context
		}
	}
	// Create a new user context and pass veb's context
	mut user_context := X{}
	user_context.Context = ctx
	// println('calling handle_route')
	handle_route[A, X](mut app, mut user_context, url, host, routes)
	return &user_context.Context
}
