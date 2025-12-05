// Copyright (c) 2019-2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fasthttp

#include <fcntl.h>
#include <errno.h>

$if !windows {
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <netinet/tcp.h>
}

fn C.socket(socket_family int, socket_type int, protocol int) int

fn C.bind(sockfd int, addr &C.sockaddr_in, addrlen u32) int

fn C.send(__fd int, __buf voidptr, __n usize, __flags int) int

fn C.recv(__fd int, __buf voidptr, __n usize, __flags int) int

fn C.setsockopt(__fd int, __level int, __optname int, __optval voidptr, __optlen u32) int

fn C.listen(__fd int, __n int) int

fn C.perror(s &u8)

fn C.close(fd int) int

fn C.htons(__hostshort u16) u16

fn C.fcntl(fd int, cmd int, arg int) int

struct C.in_addr {
	s_addr u32
}

struct C.sockaddr_in {
	sin_family u16
	sin_port   u16
	sin_addr   C.in_addr
	sin_zero   [8]u8
}

pub struct Slice {
pub:
	start int
	len   int
}

pub struct HttpRequest {
pub mut:
	buffer         []u8 // A V slice of the read buffer for convenience
	method         Slice
	path           Slice
	version        Slice
	client_conn_fd int
}

$if linux {
	struct Server {
	pub:
		port int = 3000
	mut:
		listen_fds      [max_thread_pool_size]int
		epoll_fds       [max_thread_pool_size]int
		threads         [max_thread_pool_size]thread
		request_handler fn (HttpRequest) ![]u8 @[required]
	}
} $else $if macos {
	pub struct Server {
	pub mut:
		port            int
		socket_fd       int
		poll_fd         int // kqueue fd
		request_handler fn (req HttpRequest) ![]u8 = unsafe { nil }
	}
}

// new_server creates and initializes a new Server instance.
pub fn new_server(port int, handler fn (req HttpRequest) ![]u8) !&Server {
	mut s := &Server{
		port:            port
		request_handler: handler
	}
	return s
}

const max_connection_size = 4096

const max_thread_pool_size = 16

@[direct_array_access]
fn parse_request_line(buffer []u8) !HttpRequest {
	mut req := HttpRequest{
		buffer: buffer
	}

	mut i := 0
	// Parse HTTP method
	for i < buffer.len && buffer[i] != ` ` {
		i++
	}
	req.method = Slice{
		start: 0
		len:   i
	}
	i++

	// Parse path
	mut path_start := i
	for i < buffer.len && buffer[i] != ` ` {
		i++
	}
	req.path = Slice{
		start: path_start
		len:   i - path_start
	}
	i++

	// Parse HTTP version
	mut version_start := i
	for i < buffer.len && buffer[i] != `\r` {
		i++
	}
	req.version = Slice{
		start: version_start
		len:   i - version_start
	}

	// Move to the end of the request line
	if i + 1 < buffer.len && buffer[i] == `\r` && buffer[i + 1] == `\n` {
		i += 2
	} else {
		return error('Invalid HTTP request line')
	}

	return req
}

fn decode_http_request(buffer []u8) !HttpRequest {
	return parse_request_line(buffer)
}
