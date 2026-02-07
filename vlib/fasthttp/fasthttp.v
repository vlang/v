// Copyright (c) 2019-2025 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module fasthttp

import runtime
import net

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
	content   []u8
	file_path string
}

// ServerConfig bundles the parameters needed to start a fasthttp server.
pub struct ServerConfig {
pub:
	family                  net.AddrFamily = .ip6
	port                    int            = 3000
	max_request_buffer_size int            = 8192
	handler                 fn (HttpRequest) !HttpResponse @[required]
	user_data               voidptr
}
