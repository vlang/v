// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module veb

import fasthttp
import net.http
import os
import strconv
import strings
import time
import net.urllib

struct RequestParams {
	global_app                voidptr
	controllers_sorted        []&ControllerPath
	routes                    &map[string]Route
	benchmark_page_generation bool
	// The append handler transfers its arena only after it finishes reading the context.
	defer_after_timer bool
}

// This private snapshot is never passed to application handlers. Each reactor
// worker owns one slot; the handler Context remains a separate GC allocation.
struct ResponseWriterState {
mut:
	context Context
	busy    bool
}

fn response_writer_state_enabled[A]() bool {
	if !response_body_can_share_gc_buffer() { return false }
	// ControllerHandler is public and can retain the base Context it receives.
	$if A is ControllerInterface {
		return false
	}
	return true
}

fn make_response_writer_state[A]() voidptr {
	if !response_writer_state_enabled[A]() { return unsafe { nil } }
	return &ResponseWriterState{}
}

const http_500_response = 'HTTP/1.1 500 Internal Server Error\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
const http_400_response = 'HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
const http_431_response = 'HTTP/1.1 431 Request Header Fields Too Large\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()

// request_head_error_response returns the raw response for a request head that
// `http.parse_request_head_str` rejected.
fn request_head_error_response(err IError) []u8 {
	if err is http.HeaderLimitError || err is http.RequestHeaderLimitError {
		return http_431_response
	}
	return http_500_response
}

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
	if params.max_request_body_size < 0 {
		return error('invalid max_request_body_size `${params.max_request_body_size}`, it should be 0 (no limit) or a positive number of bytes')
	}
	if ssl_enabled(params) {
		prompt_to_kill_processes_listening_on_port(params.port)
		maybe_init_server[A](mut global_app, new_server_without_lifecycle())
		run_at_with_ssl[A, X](mut global_app, params)!
		return
	}

	// Generate routes and controllers just like the original run() function.
	routes := generate_routes[A, X](global_app)!
	controllers_sorted := check_duplicate_routes_in_controllers[A](global_app, routes)!
	prompt_to_kill_processes_listening_on_port(params.port)
	$if veb_livereload ? {
		$if A is StaticApp {
			start_livereload(global_app.static_files.clone())
		} $else {
			start_livereload(map[string]string{})
		}
	}

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
		host:                    params.host
		port:                    params.port
		append_handler:          parallel_append_handler[A, X]
		make_state:              make_response_writer_state[A]
		max_request_buffer_size: params.max_request_buffer_size
		max_request_body_size:   params.max_request_body_size
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

fn listening_process_ids_from_lsof_output(output string) []int {
	mut pids := []int{}
	for line in output.split_into_lines() {
		pid := strconv.atoi(line.trim_space()) or { continue }
		if pid > 0 {
			pids << pid
		}
	}
	return pids
}

fn listening_process_ids(port int) []int {
	$if windows {
		return []int{}
	} $else {
		// `lsof` exits with status 1 when no process is listening, which is expected.
		result := os.exec(['lsof', '-tiTCP:' + '${port}', '-sTCP:LISTEN'])
		return listening_process_ids_from_lsof_output(result.output)
	}
}

fn prompt_to_kill_processes_listening_on_port(port int) {
	pids := listening_process_ids(port)
	if pids.len == 0 {
		return
	}
	pid_list := pids.map(it.str()).join(', ')
	answer := os.input('[veb] Port ${port} is in use by process ${pid_list}. Kill it? [y/N] ')
	if answer.trim_space().to_lower() != 'y' {
		return
	}
	for pid in pids {
		// PIDs are parsed as integers from lsof output, so this command has no user-provided shell input.
		os.exec(['kill', '-TERM', '${pid}'])
	}
	// Give gracefully terminated servers a moment to release the listener before binding ours.
	for _ in 0 .. 10 {
		if listening_process_ids(port).len == 0 {
			return
		}
		time.sleep(100 * time.millisecond)
	}
}

// parallel_append_handler is veb's fasthttp append handler (fasthttp.AppendHandler):
// it parses and routes the request, then serializes the response DIRECTLY into
// the connection's reused write buffer `out` — no intermediate response buffer —
// and signals takeover / close / file streaming through `ctl`.
fn parallel_append_handler[A, X](req fasthttp.HttpRequest, mut out []u8, worker_state voidptr, mut ctl fasthttp.ResponseControl) fasthttp.Step {
	mut writer_storage := unsafe { &Context(nil) }
	mut writer_state := unsafe { &ResponseWriterState(worker_state) }
	if response_writer_state_enabled[A]() && writer_state != unsafe { nil } && !writer_state.busy {
		writer_state.busy = true
		writer_storage = &writer_state.context
	}
	defer {
		if writer_storage != unsafe { nil } {
			// Drop every request reference, including on errors and takeover returns.
			// The slot is fully initialized again before use by the next request.
			unsafe { vmemset(writer_storage, 0, int(sizeof(Context))) }
			writer_state.busy = false
		}
	}
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
		out << request_head_error_response(err)
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
	mut completed_context := handle_request_and_route_with_writer[A, X](mut global_app, req2, client_fd,
		RequestParams{ ...params, defer_after_timer: true }, writer_storage)

	match completed_context.takeover_mode {
		.manual {
			ctl.takeover_mode = .manual
			if completed_context.deferred {
				// Deferred responses own a heap snapshot, never the reusable worker slot.
				va_scope_leave(arena)
				spawn after_timer(mut completed_context, arena)
				return .done
			}
			// The handler took over the connection (SSE / WebSocket) and already
			// wrote the response over ctx.conn: hand the fd off without closing it.
			// The takeover handler now owns any request-arena allocations.
			va_scope_abandon(arena)
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
	completed_context.release_response_body()
	if voidptr(completed_context) != voidptr(writer_storage) {
		unsafe { free(completed_context) }
	}
	va_scope_free_after(arena)
	return .done
}

// runs middleware, and finds the correct route for a request.
fn handle_request_and_route[A, X](mut app A, req http.Request, _client_fd int, params RequestParams) &Context {
	return handle_request_and_route_with_writer[A, X](mut app, req, _client_fd, params, unsafe { nil })
}

fn handle_request_and_route_with_writer[A, X](mut app A, req http.Request, _client_fd int, params RequestParams, writer_storage &Context) &Context {
	$if A is RequestActivityApp {
		app.before_handle_request(req)
	}
	defer {
		$if A is RequestActivityApp {
			app.after_handle_request(req)
		}
	}

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
	mut ctx := unsafe { &Context(writer_storage) }
	if ctx == unsafe { nil } {
		// Keep the original constructor/ownership contract on every fallback path.
		ctx = &Context{
			req:                   req
			page_gen_start:        page_gen_start
			client_fd:             _client_fd
			client_wants_to_close: request_has_connection_close(req)
			query:                 query
			form:                  form
			files:                 files
		}
	} else {
		// Reusable storage gets every default before any application handler runs.
		unsafe {
			*ctx = Context{
				req:                   req
				page_gen_start:        page_gen_start
				client_fd:             _client_fd
				client_wants_to_close: request_has_connection_close(req)
				query:                 query
				form:                  form
				files:                 files
			}
		}
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
			ctx.preserve_for_response_writer(mut user_context.Context)
			return ctx
		}
	}
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'veb pre-route static checked') }
	}
	// Match controller paths first
	$if A is ControllerInterface {
		if completed_context := handle_controllers[X](params.controllers_sorted, ctx, mut url, host) {
			mut armed := unsafe { &Context(completed_context) }
			if !params.defer_after_timer {
				armed.spawn_after_timer_if_armed()
			}
			return completed_context
		}
	}
	// Create a new user context and pass veb's context
	handle_route[A, X](mut app, mut user_context, url, host, params.routes)
	$if trace_prealloc ? {
		unsafe { prealloc_scope_checkpoint(c'veb route returned') }
	}
	// Preserve the handled context on the heap before the stack-local user context goes away.
	ctx.preserve_for_response_writer(mut user_context.Context)
	if ctx.deferred && writer_storage != unsafe { nil } {
		// The timer outlives this dispatch and subsequent worker-slot reuse.
		mut deferred_context := &Context{}
		unsafe { *deferred_context = *ctx }
		ctx = deferred_context
	}
	if !params.defer_after_timer {
		ctx.spawn_after_timer_if_armed()
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
