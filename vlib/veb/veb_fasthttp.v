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

const http_500_response = 'HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
const http_400_response = 'HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()

// The va_scope_* helpers wrap V's -prealloc request-arena API so the append
// handler can keep the same per-request bump-allocation behavior the legacy
// fasthttp reactor provided. They compile to nothing without -prealloc.
@[inline]
fn va_scope_begin() voidptr {
	$if prealloc {
		return unsafe { prealloc_scope_begin() }
	}
	return unsafe { nil }
}

@[inline]
fn va_scope_leave(arena voidptr) {
	$if prealloc {
		if arena != unsafe { nil } {
			unsafe { prealloc_scope_leave(arena) }
		}
	}
}

@[inline]
fn va_scope_free_after(arena voidptr) {
	$if prealloc {
		if arena != unsafe { nil } {
			unsafe { prealloc_scope_free_after(arena) }
		}
	}
}

@[inline]
fn va_scope_end(arena voidptr) {
	$if prealloc {
		if arena != unsafe { nil } {
			unsafe { prealloc_scope_end(arena) }
		}
	}
}

@[inline]
fn va_scope_abandon(arena voidptr) {
	$if prealloc {
		if arena != unsafe { nil } {
			unsafe { prealloc_scope_abandon(arena) }
		}
	}
}

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
		append_handler:          parallel_append_handler[A, X]
		max_request_buffer_size: params.max_request_buffer_size
		timeout_in_seconds:      params.timeout_in_seconds
		user_data:               voidptr(request_params)
	}) or {
		eprintln('Failed to create server: ${err}')
		return
	}
	handle := server.handle()
	maybe_init_server[A](mut global_app, new_server_with_lifecycle(handle))
	println('[veb] Running multi-threaded app on ${server_protocol(params)}://${startup_host(params)}:${params.port}/')
	flush_stdout()
	$if A is BeforeAcceptApp {
		mut server_thread := spawn_fasthttp_server_run(mut server)
		// Wait until the listener is bound before invoking before_accept_loop,
		// so callers using `<-app.started` actually see the server ready.
		handle.wait_till_running() or {}
		global_app.before_accept_loop()
		server_thread.wait() or { panic(err) }
	} $else {
		server.run() or { panic(err) }
	}
}

fn spawn_fasthttp_server_run(mut server fasthttp.Server) thread ! {
	return spawn server.run()
}

// parallel_append_handler is veb's fasthttp append handler (fasthttp.AppendHandler):
// it parses and routes the request, then serializes the response DIRECTLY into
// the connection's reused write buffer `out` — no intermediate response buffer —
// and signals takeover / close / file streaming through `ctl`.
fn parallel_append_handler[A, X](req fasthttp.HttpRequest, mut out []u8, worker_state voidptr, mut ctl fasthttp.ResponseControl) fasthttp.Step {
	_ = worker_state // veb keeps shared state in user_data, not per-worker state
	// Per-request bump arena (only compiled in under -prealloc); mirrors the arena
	// the legacy fasthttp reactor used to open for the veb handler.
	arena := va_scope_begin()

	// Get parameters from user_data - copy to avoid use-after-free
	params := unsafe { *(&RequestParams(req.user_data)) }
	mut global_app := unsafe { &A(params.global_app) }

	client_fd := req.client_conn_fd

	head_end := if req.body.start > 0 { req.body.start } else { req.buffer.len }
	head := req.buffer[..head_end].bytestr()
	// Parse the request head into a standard `http.Request`, then copy just the body.
	mut req2 := http.parse_request_head_str(head) or {
		va_scope_leave(arena)
		out << http_500_response
		va_scope_free_after(arena)
		ctl.should_close = true
		return .close
	}
	if req.body.len > 0 {
		req2.data = req.buffer[req.body.start..req.body.start + req.body.len].bytestr()
	}
	// If the request uses chunked transfer encoding, decode the chunked body.
	if transfer_encoding_is_chunked(req2.header) {
		req2.data = decode_chunked_body(req2.data) or {
			va_scope_leave(arena)
			out << http_400_response
			va_scope_free_after(arena)
			ctl.should_close = true
			return .close
		}
	}
	// Create and populate the `veb.Context` and route the request.
	mut completed_context := handle_request_and_route[A, X](mut global_app, req2, client_fd, params)

	match completed_context.takeover_mode {
		.manual {
			// The handler took over the connection (SSE / WebSocket) and already
			// wrote the response over ctx.conn: hand the fd off without closing it.
			// The takeover handler now owns any request-arena allocations.
			va_scope_abandon(arena)
			ctl.takeover_mode = .manual
			return .done
		}
		.reusable {
			// The handler wrote the response itself but wants the connection kept.
			ctl.takeover_mode = .reusable
			ctl.should_close = should_close_connection(completed_context.req,
				completed_context.res, completed_context.client_wants_to_close)
			va_scope_end(arena)
			return .done
		}
		.none {}
	}

	ctl.should_close = should_close_connection(completed_context.req, completed_context.res,
		completed_context.client_wants_to_close)
	// Serialize the response head + body straight into the reused write buffer.
	// Leave the arena first so the buffer growth (and the file_path clone below)
	// allocate on the normal heap, not in the (about-to-be-freed) request scope.
	va_scope_leave(arena)
	if completed_context.return_type == .file {
		// return_file is arena-allocated and the arena is freed below, but the
		// reactor opens the file only AFTER this handler returns — clone it off the
		// arena so the reactor never reads freed memory.
		ctl.file_path = completed_context.return_file.clone()
	}
	// strings.Builder is a `[]u8`, so the reused write buffer can be the builder
	// target directly — write_to appends into `out` with no intermediate buffer.
	completed_context.res.write_to(mut out)
	unsafe { completed_context.res.body.free() }
	completed_context.res.body = ''
	unsafe { free(completed_context) }
	va_scope_free_after(arena)
	return .done
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
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'veb parsed url/form') }
	}
	host_with_port := req.header.get(.host) or { '' }
	host := request_host_name(host_with_port)
	page_gen_start := if params.benchmark_page_generation { time.ticks() } else { 0 }
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'veb parsed host') }
	}
	mut ctx := &Context{
		req:                   req
		page_gen_start:        page_gen_start
		client_fd:             _client_fd
		client_wants_to_close: request_has_connection_close(req)
		query:                 query
		form:                  form
		files:                 files
	}
	mut user_context := X{
		Context: ctx
	}
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'veb context initialized') }
	}
	$if A is StaticApp {
		ctx.custom_mime_types_ref = unsafe { &app.static_mime_types }
		if serve_if_static[X](static_handler_config(app.static_files, app.static_mime_types,
			app.static_hosts, app.static_prefixes, app.enable_static_gzip, app.enable_static_zstd,
			app.enable_static_compression, app.static_compression_max_size,
			app.static_compression_mime_types, app.enable_markdown_negotiation), mut user_context,
			url, host)
		{
			// Preserve the handled context on the heap before the stack-local user context goes away.
			ctx.preserve_for_response_writer(user_context.Context)
			return ctx
		}
	}
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'veb pre-route static checked') }
	}
	// Match controller paths first
	$if A is ControllerInterface {
		if completed_context := handle_controllers[X](params.controllers_sorted, ctx, mut url, host) {
			return completed_context
		}
	}
	// Create a new user context and pass veb's context
	handle_route[A, X](mut app, mut user_context, url, host, params.routes)
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'veb route returned') }
	}
	// Preserve the handled context on the heap before the stack-local user context goes away.
	ctx.preserve_for_response_writer(user_context.Context)
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
