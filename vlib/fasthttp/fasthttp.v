// Copyright (c) 2019-2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fasthttp

import runtime
import net
import time

#include <errno.h>

$if !windows {
	#include <fcntl.h>
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <netinet/tcp.h>
}

const max_thread_pool_size = runtime.nr_cpus()
const max_connection_size = 65536 // Max events per epoll_wait

const tiny_bad_request_response = 'HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
const status_444_response = 'HTTP/1.1 444 No Response\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
const status_413_response = 'HTTP/1.1 413 Payload Too Large\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()

fn C.socket(domain net.AddrFamily, typ net.SocketType, protocol i32) i32

fn C.bind(sockfd i32, addr &net.Addr, addrlen u32) i32

fn C.send(__fd i32, __buf voidptr, __n usize, __flags i32) i32

fn C.recv(__fd i32, __buf voidptr, __n usize, __flags i32) i32

fn C.setsockopt(__fd i32, __level i32, __optname i32, __optval voidptr, __optlen u32) i32

fn C.listen(__fd i32, __n i32) i32

fn C.perror(s &u8)

fn C.close(fd i32) i32

fn C.htons(__hostshort u16) u16

fn C.fcntl(fd i32, cmd i32, arg i32) i32

pub struct Slice {
pub:
	start int
	len   int
}

// HttpRequest represents an HTTP request.
// TODO make fields immutable
pub struct HttpRequest {
pub mut:
	buffer         []u8 // A V slice of the read buffer for convenience
	method         Slice
	path           Slice
	version        Slice
	header_fields  Slice
	body           Slice
	client_conn_fd int
	user_data      voidptr // User-defined context data
}

pub struct HttpResponse {
pub:
	content      []u8
	file_path    string
	takeover     bool // if true, the connection fd is handed off to the caller and must not be closed by fasthttp
	should_close bool // if true, close the connection after sending (Connection: close)
}

// ServerConfig bundles the parameters needed to start a fasthttp server.
pub struct ServerConfig {
pub:
	family                  net.AddrFamily = .ip6
	port                    int            = 3000
	max_request_buffer_size int            = 8192
	timeout_in_seconds      int            = 30
	handler                 fn (HttpRequest) !HttpResponse @[required]
	user_data               voidptr
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
	$if linux || macos {
		mut server := unsafe { &Server(h.ptr) }
		return server.wait_till_running_impl(params)!
	} $else {
		return error('fasthttp server lifecycle control is only supported on linux and macos')
	}
}

// shutdown gracefully stops accepting new requests and waits for active requests to finish.
pub fn (h ServerHandle) shutdown(params ShutdownParams) ! {
	if h.ptr == unsafe { nil } {
		return error('server handle is not initialized')
	}
	$if linux || macos {
		mut server := unsafe { &Server(h.ptr) }
		server.shutdown_impl(params)!
	} $else {
		return error('fasthttp server lifecycle control is only supported on linux and macos')
	}
}

$if linux || macos {
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
