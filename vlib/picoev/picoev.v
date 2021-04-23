// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module picoev

import picohttpparser

#include <errno.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <fcntl.h>
#include <signal.h>
#flag -I @VEXEROOT/thirdparty/picoev
#flag -L @VEXEROOT/thirdparty/picoev
#flag @VEXEROOT/thirdparty/picoev/picoev.o
#include "src/picoev.h"
const (
	max_fds      = 1024
	max_timeout  = 10
	max_read     = 4096
	max_write    = 8192
)

struct C.in_addr {
mut:
	s_addr int
}

struct C.sockaddr_in {
mut:
	sin_family int
	sin_port   int
	sin_addr   C.in_addr
}

struct C.sockaddr_storage {
}

fn C.atoi() int

fn C.strncasecmp(s1 charptr, s2 charptr, n size_t) int

fn C.socket(domain int, typ int, protocol int) int

// fn C.setsockopt(sockfd int, level int, optname int, optval voidptr, optlen C.socklen_t) int
fn C.setsockopt(sockfd int, level int, optname int, optval voidptr, optlen u32) int

fn C.htonl(hostlong u32) int

fn C.htons(netshort u16) int

// fn C.bind(sockfd int, addr &C.sockaddr, addrlen C.socklen_t) int
// use voidptr for arg 2 becasue sockaddr is a generic descriptor for any kind of socket operation,
// it can also take sockaddr_in depending on the type of socket used in arg 1
fn C.bind(sockfd int, addr voidptr, addrlen u32) int

fn C.listen(sockfd int, backlog int) int

// fn C.accept(sockfd int, addr &C.sockaddr, addrlen &C.socklen_t) int
fn C.accept(sockfd int, addr &C.sockaddr, addrlen &u32) int

fn C.getaddrinfo(node charptr, service charptr, hints &C.addrinfo, res &&C.addrinfo) int

// fn C.connect(sockfd int, addr &C.sockaddr, addrlen C.socklen_t) int
fn C.connect(sockfd int, addr &C.sockaddr, addrlen u32) int

// fn C.send(sockfd int, buf voidptr, len size_t, flags int) size_t
fn C.send(sockfd int, buf voidptr, len size_t, flags int) int

// fn C.recv(sockfd int, buf voidptr, len size_t, flags int) size_t
fn C.recv(sockfd int, buf voidptr, len size_t, flags int) int

// fn C.read() int
fn C.shutdown(socket int, how int) int

// fn C.close() int
fn C.ntohs(netshort u16) int

// fn C.getsockname(sockfd int, addr &C.sockaddr, addrlen &C.socklen_t) int
fn C.getsockname(sockfd int, addr &C.sockaddr, addrlen &u32) int

fn C.fcntl(fd int, cmd int, arg ...voidptr) int

// fn C.write() int
struct C.picoev_loop {
}

struct Picoev {
	loop &C.picoev_loop
	cb   fn(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response)
mut:
	date byteptr
	buf  byteptr
	idx  [max_fds]int
	out  byteptr
pub mut:
	user_data    voidptr = voidptr(0)
	timeout_secs int = 8
	max_headers  int = 100
}

fn C.picoev_del(&C.picoev_loop, int) int

fn C.picoev_set_timeout(&C.picoev_loop, int, int)

// fn C.picoev_handler(loop &C.picoev_loop, fd int, revents int, cb_arg voidptr)
// TODO: (sponge) update to C.picoev_handler with C type def update
type Cpicoev_handler = fn(loop &C.picoev_loop, fd int, revents int, cb_arg voidptr)

fn C.picoev_add(&C.picoev_loop, int, int, int, &Cpicoev_handler, voidptr) int

fn C.picoev_init(int) int

fn C.picoev_create_loop(int) &C.picoev_loop

fn C.picoev_loop_once(&C.picoev_loop, int) int

fn C.picoev_destroy_loop(&C.picoev_loop) int

fn C.picoev_deinit() int

[inline]
fn setup_sock(fd int) {
	on := 1
	if C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_NODELAY, &on, sizeof(int)) < 0 {
		println('setup_sock.setup_sock failed')
	}
	if C.fcntl(fd, C.F_SETFL, C.O_NONBLOCK) != 0 {
		println('fcntl failed')
	}
}

[inline]
fn close_conn(loop &C.picoev_loop, fd int) {
	C.picoev_del(loop, fd)
	C.close(fd)
}

[inline]
fn req_read(fd int, b byteptr, max_len int, idx int) int {
	unsafe {
		return C.read(fd, b + idx, max_len - idx)
	}
}

[inline]
fn mysubstr(s byteptr, from int, len int) string {
	unsafe {
		return tos(s + from, len)
	}
}

fn rw_callback(loop &C.picoev_loop, fd int, events int, cb_arg voidptr) {
	mut p := unsafe {&Picoev(cb_arg)}
	if (events & C.PICOEV_TIMEOUT) != 0 {
		close_conn(loop, fd)
		p.idx[fd] = 0
		return
	} else if (events & C.PICOEV_READ) != 0 {
		C.picoev_set_timeout(loop, fd, p.timeout_secs)

		// Request init
		mut buf := p.buf
		unsafe {
			buf += fd * max_read /* pointer magic */
		}
		mut req := picohttpparser.Request{}
		
		for { /* Request parsing loop */
			r := req_read(fd, buf, max_read, p.idx[fd]) // Get data from socket
			if r == 0 { /* connection closed by peer */
				close_conn(loop, fd)
				p.idx[fd] = 0
				return
			} else if r == -1 { /* error */
				if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK { /* try again later */
					return
				} else { /* fatal error */
					close_conn(loop, fd)
					p.idx[fd] = 0
					return
				}
			}
			p.idx[fd] += r

			mut s := unsafe { tos(buf, p.idx[fd]) }
			pret := req.parse_request(s, p.max_headers) // Parse request via picohttpparser
			if pret > 0 { // Success
				break
			} else if pret == -1 { // Parse error
				eprintln("picohttprequest: ParseError")
				return
			}

			assert pret == -2 /* request is incomplete, continue the loop */
			if p.idx[fd] == sizeof(buf) {
				eprintln("picohttprequest: RequestIsTooLongError")
				return
			}
		}

		// Response init
		mut out := p.out
		unsafe {
			out += fd * max_write /* pointer magic */
		}
		mut res := picohttpparser.Response{
			fd: fd
			date: p.date
			buf_start: out
			buf: out
		}

		// Callback (should call .end() itself)
		p.cb(mut p.user_data, req, mut &res)
		p.idx[fd] = 0
	}
}

fn accept_callback(loop &C.picoev_loop, fd int, events int, cb_arg voidptr) {
	mut p := unsafe {&Picoev(cb_arg)}
	newfd := C.accept(fd, 0, 0)
	if newfd != -1 {
		setup_sock(newfd)
		C.picoev_add(loop, newfd, C.PICOEV_READ, p.timeout_secs, rw_callback, cb_arg)
	}
}

pub fn new(port int, cb voidptr) &Picoev {
	fd := C.socket(C.AF_INET, C.SOCK_STREAM, 0)
	assert fd != -1
	flag := 1
	assert C.setsockopt(fd, C.SOL_SOCKET, C.SO_REUSEADDR, &flag, sizeof(int)) == 0
	assert C.setsockopt(fd, C.SOL_SOCKET, C.SO_REUSEPORT, &flag, sizeof(int)) == 0
	$if linux {
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_QUICKACK, &flag, sizeof(int)) == 0
		timeout := 10
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_DEFER_ACCEPT, &timeout, sizeof(int)) == 0
		queue_len := 4096
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_FASTOPEN, &queue_len, sizeof(int)) == 0
	}
	mut addr := C.sockaddr_in{}
	addr.sin_family = C.AF_INET
	addr.sin_port = C.htons(port)
	addr.sin_addr.s_addr = C.htonl(C.INADDR_ANY)
	size := 16 // sizeof(C.sockaddr_in)
	bind_res := C.bind(fd, &addr, size)
	assert bind_res == 0
	listen_res := C.listen(fd, C.SOMAXCONN)
	assert listen_res == 0
	setup_sock(fd)
	C.picoev_init(max_fds)
	loop := C.picoev_create_loop(max_timeout)
	mut pv := &Picoev{
		loop: loop
		cb: cb
		date: C.get_date()
		buf: unsafe { malloc(max_fds * max_read + 1) }
		out: unsafe { malloc(max_fds * max_write + 1) }
	}
	C.picoev_add(loop, fd, C.PICOEV_READ, 0, accept_callback, pv)
	go update_date(mut pv)
	return pv
}

pub fn (p Picoev) serve() {
	for {
		C.picoev_loop_once(p.loop, 1)
	}
}

fn update_date(mut p Picoev) {
	for {
		p.date = C.get_date()
		C.usleep(1000000)
	}
}
