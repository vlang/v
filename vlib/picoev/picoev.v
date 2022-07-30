// Copyright (c) 2019-2022 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module picoev

import net
import picohttpparser

#include <errno.h>
#include <netinet/tcp.h>
#include <signal.h>
#flag -I @VEXEROOT/thirdparty/picoev
#flag @VEXEROOT/thirdparty/picoev/picoev.o
#include "src/picoev.h"

[typedef]
struct C.picoev_loop {}

fn C.picoev_del(&C.picoev_loop, int) int

fn C.picoev_set_timeout(&C.picoev_loop, int, int)

// fn C.picoev_handler(loop &C.picoev_loop, fd int, revents int, cb_arg voidptr)
// TODO: (sponge) update to C.picoev_handler with C type def update
type PicoevHandler = fn (loop &C.picoev_loop, fd int, revents int, context voidptr)

fn C.picoev_add(&C.picoev_loop, int, int, int, &PicoevHandler, voidptr) int

fn C.picoev_init(int) int

fn C.picoev_create_loop(int) &C.picoev_loop

fn C.picoev_loop_once(&C.picoev_loop, int) int

fn C.picoev_destroy_loop(&C.picoev_loop) int

fn C.picoev_deinit() int

const (
	max_fds     = 1024
	max_timeout = 10
	max_read    = 4096
	max_write   = 8192
)

enum Event {
	read = C.PICOEV_READ
	write = C.PICOEV_WRITE
	timeout = C.PICOEV_TIMEOUT
	add = C.PICOEV_ADD
	del = C.PICOEV_DEL
	readwrite = C.PICOEV_READWRITE
}

pub struct Config {
pub:
	port         int = 8080
	cb           fn (voidptr, picohttpparser.Request, mut picohttpparser.Response)
	err_cb       fn (voidptr, picohttpparser.Request, mut picohttpparser.Response, IError) = default_err_cb
	user_data    voidptr = unsafe { nil }
	timeout_secs int     = 8
	max_headers  int     = 100
}

struct Picoev {
	loop         &C.picoev_loop
	cb           fn (voidptr, picohttpparser.Request, mut picohttpparser.Response)
	err_cb       fn (voidptr, picohttpparser.Request, mut picohttpparser.Response, IError)
	user_data    voidptr
	timeout_secs int
	max_headers  int
mut:
	date &u8
	buf  &u8
	idx  [1024]int
	out  &u8
}

[inline]
fn setup_sock(fd int) ? {
	flag := 1
	if C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_NODELAY, &flag, sizeof(int)) < 0 {
		return error('setup_sock.setup_sock failed')
	}
	if C.fcntl(fd, C.F_SETFL, C.O_NONBLOCK) != 0 {
		return error('fcntl failed')
	}
}

[inline]
fn close_conn(loop &C.picoev_loop, fd int) {
	C.picoev_del(voidptr(loop), fd)
	C.close(fd)
}

[inline]
fn req_read(fd int, b &u8, max_len int, idx int) int {
	unsafe {
		return C.read(fd, b + idx, max_len - idx)
	}
}

fn rw_callback(loop &C.picoev_loop, fd int, events int, context voidptr) {
	mut p := unsafe { &Picoev(context) }
	defer {
		p.idx[fd] = 0
	}
	if (events & int(Event.timeout)) != 0 {
		close_conn(loop, fd)
		return
	} else if (events & int(Event.read)) != 0 {
		C.picoev_set_timeout(voidptr(loop), fd, p.timeout_secs)

		// Request init
		mut buf := p.buf
		unsafe {
			buf += fd * picoev.max_read // pointer magic
		}
		mut req := picohttpparser.Request{}

		// Response init
		mut out := p.out
		unsafe {
			out += fd * picoev.max_write // pointer magic
		}
		mut res := picohttpparser.Response{
			fd: fd
			date: p.date
			buf_start: out
			buf: out
		}

		for {
			// Request parsing loop
			r := req_read(fd, buf, picoev.max_read, p.idx[fd]) // Get data from socket
			if r == 0 {
				// connection closed by peer
				close_conn(loop, fd)
				return
			} else if r == -1 {
				// error
				if C.errno == C.EAGAIN {
					// try again later
					return
				}
				if C.errno == C.EWOULDBLOCK {
					// try again later
					return
				}
				// fatal error
				close_conn(loop, fd)
				return
			}
			p.idx[fd] += r

			mut s := unsafe { tos(buf, p.idx[fd]) }
			pret := req.parse_request(s, p.max_headers) // Parse request via picohttpparser
			if pret > 0 { // Success
				break
			} else if pret == -1 { // Parse error
				p.err_cb(p.user_data, req, mut &res, error('ParseError'))
				return
			}

			assert pret == -2
			// request is incomplete, continue the loop
			if p.idx[fd] == sizeof(buf) {
				p.err_cb(p.user_data, req, mut &res, error('RequestIsTooLongError'))
				return
			}
		}

		// Callback (should call .end() itself)
		p.cb(p.user_data, req, mut &res)
	}
}

fn accept_callback(loop &C.picoev_loop, fd int, events int, cb_arg voidptr) {
	mut p := unsafe { &Picoev(cb_arg) }
	newfd := C.accept(fd, 0, 0)
	if newfd != -1 {
		setup_sock(newfd) or {
			p.err_cb(p.user_data, picohttpparser.Request{}, mut &picohttpparser.Response{},
				err)
		}
		C.picoev_add(voidptr(loop), newfd, int(Event.read), p.timeout_secs, rw_callback,
			cb_arg)
	}
}

fn default_err_cb(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response, error IError) {
	eprintln('picoev: $error')
	res.end()
}

pub fn new(config Config) &Picoev {
	fd := C.socket(net.AddrFamily.ip, net.SocketType.tcp, 0)
	assert fd != -1

	// Setting flags for socket
	flag := 1
	assert C.setsockopt(fd, C.SOL_SOCKET, C.SO_REUSEADDR, &flag, sizeof(int)) == 0
	$if linux {
		assert C.setsockopt(fd, C.SOL_SOCKET, C.SO_REUSEPORT, &flag, sizeof(int)) == 0
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_QUICKACK, &flag, sizeof(int)) == 0
		timeout := 10
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_DEFER_ACCEPT, &timeout, sizeof(int)) == 0
		queue_len := 4096
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_FASTOPEN, &queue_len, sizeof(int)) == 0
	}

	// Setting addr
	mut addr := C.sockaddr_in{
		sin_family: u8(C.AF_INET)
		sin_port: C.htons(config.port)
		sin_addr: C.htonl(C.INADDR_ANY)
	}
	size := sizeof(C.sockaddr_in)
	bind_res := C.bind(fd, voidptr(unsafe { &net.Addr(&addr) }), size)
	assert bind_res == 0
	listen_res := C.listen(fd, C.SOMAXCONN)
	assert listen_res == 0
	setup_sock(fd) or {
		config.err_cb(config.user_data, picohttpparser.Request{}, mut &picohttpparser.Response{},
			err)
	}

	C.picoev_init(picoev.max_fds)
	loop := C.picoev_create_loop(picoev.max_timeout)
	mut pv := &Picoev{
		loop: loop
		cb: config.cb
		err_cb: config.err_cb
		user_data: config.user_data
		timeout_secs: config.timeout_secs
		max_headers: config.max_headers
		date: &u8(C.get_date())
		buf: unsafe { malloc_noscan(picoev.max_fds * picoev.max_read + 1) }
		out: unsafe { malloc_noscan(picoev.max_fds * picoev.max_write + 1) }
	}

	C.picoev_add(voidptr(loop), fd, int(Event.read), 0, accept_callback, pv)
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
		p.date = &u8(C.get_date())
		C.usleep(1000000)
	}
}
