// Copyright (c) 2019-2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fasthttp

import runtime
import net
import time

const max_thread_pool_size = runtime.nr_cpus()
const max_connection_size = 65536 // Max events per epoll_wait

const tiny_bad_request_response = 'HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
const status_444_response = 'HTTP/1.1 444 No Response\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
const status_413_response = 'HTTP/1.1 413 Payload Too Large\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()

pub struct Slice {
pub:
	start int
	len   int
}

// HttpRequest represents an HTTP request.
// TODO make fields immutable
pub struct HttpRequest {
pub mut:
	buffer             []u8 // A V slice of the read buffer for convenience
	method             Slice
	path               Slice
	version            Slice
	header_fields      Slice
	body               Slice
	client_conn_fd     int
	client_conn_handle usize
	user_data          voidptr // User-defined context data (shared, set from ServerConfig.user_data)
	// worker_state is the value ServerConfig.make_state returned on THIS worker
	// thread (nil when no make_state is configured). It is thread-local by
	// construction — one instance per worker thread — so a handler can keep
	// per-worker resources (a DB connection, a reused render scratch buffer)
	// without any locking: `unsafe { &MyState(req.worker_state) }`.
	worker_state voidptr
}

pub enum ResponseTakeoverMode {
	none
	manual
	reusable
}

// Step is what an append-style handler (see ServerConfig.append_handler) returns
// to the reactor, mirroring vanilla's handler contract:
//   .done    — the response is complete in `out`; the reactor sends it and keeps
//              the connection alive (unless ResponseControl.should_close is set).
//   .close   — the reactor sends whatever is in `out`, then closes the connection.
//   .suspend — reserved for async handlers that park on an external fd. There is
//              no watch reactor yet, so a .suspend currently drops the connection
//              (like vanilla's reactorless backends); it is defined now so the
//              contract is stable when async support lands.
pub enum Step {
	done
	close
	suspend
}

// ResponseControl is the out-of-band channel for an append-style handler: the
// handler appends the raw HTTP response bytes (status line + headers + body)
// directly into the reused `out` buffer and sets these fields to influence how
// the reactor treats the connection.
pub struct ResponseControl {
pub mut:
	// takeover_mode lets the handler take over the socket instead of having the
	// reactor send `out`: .manual hands the fd off entirely (SSE/WebSocket), and
	// .reusable means the handler wrote the response itself but wants the reactor
	// to keep serving the (kept-alive) connection.
	takeover_mode ResponseTakeoverMode
	// should_close asks the reactor to close the connection after this response.
	should_close bool
	// file_path, when set, is streamed (sendfile) after the bytes appended to
	// `out` — the zero-copy static-file path.
	file_path string
}

// AppendHandler is the zero-copy request handler contract: it appends the raw
// HTTP response into the connection's reused write buffer `out` (rather than
// allocating and returning a response), reads per-worker state via `worker_state`
// (see ServerConfig.make_state), signals connection handling through `ctl`, and
// returns a Step. Appending into `out` — which the reactor reuses across requests
// and flushes for every pipelined request in one send — removes the per-request
// response allocation that the return-a-response `handler` contract requires.
pub type AppendHandler = fn (req HttpRequest, mut out []u8, worker_state voidptr, mut ctl ResponseControl) Step

pub struct HttpResponse {
pub mut:
	content       []u8
	file_path     string
	takeover_mode ResponseTakeoverMode
	should_close  bool // if true, close the connection after sending (Connection: close)
	// content_owned lets the backend free or move content after it has been sent.
	content_owned bool
	// request_arena is a prealloc scope handle that must be freed after sending.
	request_arena voidptr
}

fn (mut resp HttpResponse) free_owned_content() {
	if resp.content_owned && resp.content.cap > 0 {
		unsafe { resp.content.free() }
		resp.content = []u8{}
	}
}

fn (mut resp HttpResponse) take_or_clone_content() []u8 {
	if resp.content_owned {
		content := resp.content
		resp.content = []u8{}
		return content
	}
	return resp.content.clone()
}

fn end_request_arena_current_thread(request_arena voidptr) {
	$if prealloc {
		if request_arena != unsafe { nil } {
			unsafe { prealloc_scope_end(request_arena) }
		}
	}
}

fn leave_request_arena_current_thread(request_arena voidptr) {
	$if prealloc {
		if request_arena != unsafe { nil } {
			unsafe { prealloc_scope_leave(request_arena) }
		}
	}
}

fn abandon_request_arena_current_thread(request_arena voidptr) {
	$if prealloc {
		if request_arena != unsafe { nil } {
			unsafe { prealloc_scope_abandon(request_arena) }
		}
	}
}

fn (mut resp HttpResponse) attach_request_arena_if_empty(request_arena voidptr) {
	if resp.request_arena == unsafe { nil } {
		resp.request_arena = request_arena
	}
}

fn (mut resp HttpResponse) end_request_arena_current_thread() {
	end_request_arena_current_thread(resp.request_arena)
	resp.request_arena = unsafe { nil }
}

fn (mut resp HttpResponse) abandon_request_arena_current_thread() {
	abandon_request_arena_current_thread(resp.request_arena)
	resp.request_arena = unsafe { nil }
}

fn (mut resp HttpResponse) take_request_arena() voidptr {
	request_arena := resp.request_arena
	resp.request_arena = unsafe { nil }
	return request_arena
}

// ServerConfig bundles the parameters needed to start a fasthttp server.
pub struct ServerConfig {
pub:
	family                  net.AddrFamily = .ip6
	port                    int            = 3000
	max_request_buffer_size int            = 8192
	timeout_in_seconds      int            = 30
	// handler is the classic contract: it builds and returns a full HttpResponse.
	// Set exactly ONE of `handler` or `append_handler`.
	handler   fn (HttpRequest) !HttpResponse = unsafe { nil }
	user_data voidptr
	// make_state, when set, is called ONCE per worker thread at startup; the value
	// it returns reaches every request on that worker as HttpRequest.worker_state.
	// It is the lock-free per-worker state hook: each worker gets its own instance
	// (a DB connection, a reused scratch buffer) with no shared pool and no mutex.
	make_state fn () voidptr = unsafe { nil }
	// append_handler is the zero-copy contract (see AppendHandler): it appends the
	// raw response into the connection's reused write buffer instead of returning
	// one. When set, it is used instead of `handler`. Set exactly one of the two.
	append_handler AppendHandler = unsafe { nil }
}

// ShutdownParams configures how long graceful shutdown should wait for in-flight requests.
@[params]
pub struct ShutdownParams {
pub:
	timeout         time.Duration = time.infinite
	retry_period_ms int           = 10
}

// WaitTillRunningParams allows parametrizing the calls to `ServerHandle.wait_till_running()`.
@[params]
pub struct WaitTillRunningParams {
pub:
	max_retries     int = 100
	retry_period_ms int = 10
}

// ServerHandle exposes lifecycle controls for a running `fasthttp.Server`.
pub struct ServerHandle {
	ptr voidptr
}

// handle returns a reusable handle for waiting on or shutting down the server.
pub fn (s &Server) handle() ServerHandle {
	return ServerHandle{
		ptr: s
	}
}

// wait_till_running waits until the server transitions to its serving state.
pub fn (h ServerHandle) wait_till_running(params WaitTillRunningParams) !int {
	if h.ptr == unsafe { nil } {
		return error('server handle is not initialized')
	}
	$if linux || bsd || windows {
		mut server := unsafe { &Server(h.ptr) }
		return server.wait_till_running_impl(params)!
	} $else {
		return error('fasthttp server lifecycle control is only supported on linux, Windows, and BSD-family OSes')
	}
}

// shutdown gracefully stops accepting new requests and waits for active requests to finish.
pub fn (h ServerHandle) shutdown(params ShutdownParams) ! {
	if h.ptr == unsafe { nil } {
		return error('server handle is not initialized')
	}
	$if linux || bsd || windows {
		mut server := unsafe { &Server(h.ptr) }
		server.shutdown_impl(params)!
	} $else {
		return error('fasthttp server lifecycle control is only supported on linux, Windows, and BSD-family OSes')
	}
}

$if linux || bsd || windows {
	fn normalized_retry_period_ms(retry_period_ms int) int {
		return if retry_period_ms > 0 { retry_period_ms } else { 1 }
	}

	fn (mut s Server) wait_till_running_impl(params WaitTillRunningParams) !int {
		retry_period_ms := normalized_retry_period_ms(params.retry_period_ms)
		mut attempts := 0
		mut running := s.running
		for !running.load() && attempts < params.max_retries {
			time.sleep(retry_period_ms * time.millisecond)
			attempts++
		}
		if !running.load() {
			return error('maximum retries reached')
		}
		time.sleep(retry_period_ms * time.millisecond)
		return attempts
	}

	fn (mut s Server) shutdown_impl(params ShutdownParams) ! {
		mut stopped := s.stopped
		if stopped.load() {
			return
		}
		mut shutting_down := s.shutting_down
		if shutting_down.compare_and_swap(false, true) {
			s.stop_accepting()
		}
		retry_period_ms := normalized_retry_period_ms(params.retry_period_ms)
		mut watch := time.new_stopwatch()
		for !stopped.load() {
			if params.timeout != time.infinite && watch.elapsed() >= params.timeout {
				return error('graceful shutdown timed out after ${params.timeout}')
			}
			time.sleep(retry_period_ms * time.millisecond)
		}
	}

	fn (s &Server) begin_request() {
		mut active_requests := s.active_requests
		active_requests.add(1)
	}

	fn (s &Server) end_request() {
		mut active_requests := s.active_requests
		active_requests.sub(1)
	}

	fn (s &Server) active_request_count() int {
		mut active_requests := s.active_requests
		return active_requests.load()
	}

	fn (s &Server) is_shutting_down() bool {
		mut shutting_down := s.shutting_down
		return shutting_down.load()
	}

	fn (s &Server) is_stopped() bool {
		mut stopped := s.stopped
		return stopped.load()
	}

	fn (mut s Server) mark_running() {
		mut running := s.running
		running.store(true)
		mut stopped := s.stopped
		stopped.store(false)
	}

	fn (mut s Server) mark_stopped() {
		mut active_requests := s.active_requests
		active_requests.store(0)
		mut running := s.running
		running.store(false)
		mut stopped := s.stopped
		stopped.store(true)
	}
}
