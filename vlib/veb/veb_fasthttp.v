// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module veb

import fasthttp
import net.http
import strconv
import strings
import time
import net.urllib

struct RequestParams {
	global_app                voidptr
	controllers_sorted        []&ControllerPath
	routes                    &map[string]Route
	benchmark_page_generation bool
}

const http_ok_response = 'HTTP/1.1 200 OK\r\nContent-Type: application/json\r\nContent-Length: 0\r\nConnection: keep-alive\r\n\r\n'.bytes()

pub fn run_at[A, X](mut global_app A, params RunParams) ! {
	run_new[A, X](mut global_app, params)!
}

// run_new - start a new veb server using the parallel fasthttp backend.
pub fn run_new[A, X](mut global_app A, params RunParams) ! {
	if params.port <= 0 || params.port > 65535 {
		return error('invalid port number `${params.port}`, it should be between 1 and 65535')
	}
	if ssl_enabled(params) {
		maybe_init_server[A](mut global_app, new_server_without_lifecycle())
		run_at_with_ssl[A, X](mut global_app, params)!
		return
	}

	// Generate routes and controllers just like the original run() function.
	routes := generate_routes[A, X](global_app)!
	controllers_sorted := check_duplicate_routes_in_controllers[A](global_app, routes)!

	// Allocate params on the heap to keep it valid for the server lifetime
	request_params := &RequestParams{
		global_app:                unsafe { voidptr(&global_app) }
		controllers_sorted:        controllers_sorted
		routes:                    &routes
		benchmark_page_generation: params.benchmark_page_generation
	}

	// Configure and run the fasthttp server
	mut server := fasthttp.new_server(fasthttp.ServerConfig{
		family:                  params.family
		port:                    params.port
		handler:                 parallel_request_handler[A, X]
		max_request_buffer_size: params.max_request_buffer_size
		timeout_in_seconds:      params.timeout_in_seconds
		user_data:               voidptr(request_params)
	}) or {
		eprintln('Failed to create server: ${err}')
		return
	}
	maybe_init_server[A](mut global_app, new_server_with_lifecycle(server.handle()))
	println('[veb] Running multi-threaded app on ${server_protocol(params)}://${startup_host(params)}:${params.port}/')
	flush_stdout()
	$if A is BeforeAcceptApp {
		global_app.before_accept_loop()
	}
	server.run() or { panic(err) }
}

fn parallel_request_handler[A, X](req fasthttp.HttpRequest) !fasthttp.HttpResponse {
	// Get parameters from user_data - copy to avoid use-after-free
	params := unsafe { *(&RequestParams(req.user_data)) }
	mut global_app := unsafe { &A(params.global_app) }

	client_fd := req.client_conn_fd

	head_end := if req.body.start > 0 { req.body.start } else { req.buffer.len }
	head := req.buffer[..head_end].bytestr()
	// Parse the request head into a standard `http.Request`, then copy just the body.
	mut req2 := http.parse_request_head_str(head) or {
		return fasthttp.HttpResponse{
			content: 'HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
		}
	}
	if req.body.len > 0 {
		req2.data = req.buffer[req.body.start..req.body.start + req.body.len].bytestr()
	}
	// If the request uses chunked transfer encoding, decode the chunked body
	if transfer_encoding_is_chunked(req2.header) {
		req2.data = decode_chunked_body(req2.data) or {
			return fasthttp.HttpResponse{
				content: 'HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
			}
		}
	}
	if invalid_resp := content_length_validation_response(req, req2) {
		return invalid_resp
	}
	// Create and populate the `veb.Context`.
	completed_context := handle_request_and_route[A, X](mut global_app, req2, client_fd, params)

	if completed_context.takeover {
		// The handler has taken over the connection (e.g. for SSE or WebSocket).
		// The response was already sent directly over ctx.conn.
		// Tell fasthttp to hand off the fd without closing it.
		return fasthttp.HttpResponse{
			takeover: true
		}
	}

	if completed_context.return_type == .file {
		return fasthttp.HttpResponse{
			content:      completed_context.res.bytes()
			file_path:    completed_context.return_file
			should_close: should_close_connection(completed_context.req, completed_context.res,
				completed_context.client_wants_to_close)
		}
	}

	// The fasthttp server expects a complete response buffer to be returned.
	return fasthttp.HttpResponse{
		content:      completed_context.res.bytes()
		should_close: should_close_connection(completed_context.req, completed_context.res,
			completed_context.client_wants_to_close)
	}
} // handle_request_and_route is a unified function that creates the context,

fn content_length_validation_response(req fasthttp.HttpRequest, parsed http.Request) ?fasthttp.HttpResponse {
	if transfer_encoding_is_chunked(parsed.header) {
		return none
	}
	content_length := parsed.header.get(.content_length) or { return none }
	expected_length := content_length.int()
	actual_length := req.body.len
	if actual_length == expected_length {
		return none
	}
	if actual_length < expected_length {
		return fasthttp.HttpResponse{
			content: http_408.bytes()
		}
	}
	return fasthttp.HttpResponse{
		content: http.new_response(
			status: .bad_request
			body:   'Mismatch of body length and Content-Length header'
			header: http.new_header(
				key:   .content_type
				value: 'text/plain'
			).join(headers_close)
		).bytes()
	}
}

// runs middleware, and finds the correct route for a request.
fn handle_request_and_route[A, X](mut app A, req http.Request, _client_fd int, params RequestParams) &Context {
	// Create and populate the `veb.Context` from the request.
	mut url := urllib.parse_request_uri(req.url) or {
		// This should be rare if http.parse_request succeeded.
		mut bad_ctx := &Context{
			req: req
		}
		bad_ctx.not_found()
		return bad_ctx
	}
	query := parse_query_from_url(url)
	form, files := parse_form_from_request(req) or {
		mut bad_ctx := &Context{
			req: req
		}
		bad_ctx.request_error('Failed to parse form data: ${err.msg()}')
		return bad_ctx
	}
	host_with_port := req.header.get(.host) or { '' }
	host, _ := urllib.split_host_port(host_with_port)
	page_gen_start := if params.benchmark_page_generation { time.ticks() } else { 0 }
	mut ctx := &Context{
		req:                   req
		page_gen_start:        page_gen_start
		client_fd:             _client_fd
		client_wants_to_close: request_has_connection_close(req)
		query:                 query
		form:                  form
		files:                 files
	}
	$if A is StaticApp {
		ctx.custom_mime_types = app.static_mime_types.clone()
		mut user_context := X{}
		user_context.Context = ctx
		if serve_if_static[X](static_handler_config(app.static_files, app.static_mime_types,
			app.static_hosts, app.enable_static_gzip, app.enable_static_zstd,
			app.enable_static_compression, app.static_compression_max_size,
			app.static_compression_mime_types, app.enable_markdown_negotiation), mut user_context,
			url, host)
		{
			// Preserve the handled context on the heap before the stack-local user context goes away.
			unsafe {
				*ctx = user_context.Context
			}
			return ctx
		}
	}
	// Match controller paths first
	$if A is ControllerInterface {
		if completed_context := handle_controllers[X](params.controllers_sorted, ctx, mut url, host) {
			return completed_context
		}
	}
	// Create a new user context and pass veb's context
	mut user_context := X{}
	user_context.Context = ctx
	handle_route[A, X](mut app, mut user_context, url, host, params.routes)
	// Preserve the handled context on the heap before the stack-local user context goes away.
	unsafe {
		*ctx = user_context.Context
	}
	return ctx
}

// decode_chunked_body decodes a chunked transfer-encoded body string
// into the raw body content.
fn decode_chunked_body(data string) !string {
	mut sb := strings.new_builder(data.len)
	mut pos := 0
	for pos < data.len {
		// Find the end of the chunk size line
		line_end := data.index_after_('\r\n', pos)
		if line_end == -1 {
			return error('invalid chunked body: missing chunk size line ending')
		}
		chunk_size_str := data[pos..line_end].all_before(';').trim_space()
		if chunk_size_str.len == 0 {
			return error('invalid chunked body: empty chunk size')
		}
		chunk_size := int(strconv.parse_uint(chunk_size_str, 16, 64) or {
			return error('invalid chunked body: bad chunk size')
		})
		pos = line_end + 2 // skip past \r\n
		if chunk_size == 0 {
			// Terminal chunk - skip trailers
			break
		}
		if pos + chunk_size > data.len {
			return error('invalid chunked body: chunk data truncated')
		}
		sb.write_string(data[pos..pos + chunk_size])
		pos += chunk_size + 2 // skip chunk data + \r\n delimiter
	}
	return sb.str()
}
