// Copyright (c) 2019-2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fasthttp

import runtime

#include <fcntl.h>
#include <errno.h>

$if !windows {
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <netinet/tcp.h>
}

const max_thread_pool_size = runtime.nr_cpus()
const max_connection_size = 65536 // Max events per epoll_wait

const tiny_bad_request_response = 'HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
const status_444_response = 'HTTP/1.1 444 No Response\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()
const status_413_response = 'HTTP/1.1 413 Payload Too Large\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()

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

// ServerConfig bundles the parameters needed to start a fasthttp server.
pub struct ServerConfig {
pub:
	port                    int = 3000
	max_request_buffer_size int = 8192
	handler                 fn (HttpRequest) ![]u8 @[required]
}

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
