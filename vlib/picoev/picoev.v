module picoev

import net
import picohttpparser

#include <errno.h>
$if windows {
	#include <winsock2.h>
	#include <ws2tcpip.h>
} $else $if macos || freebsd {
	#include <sys/types.h>
	#include <sys/socket.h>
	#include <netinet/in.h>
	#include <netinet/tcp.h>
} $else {
	#include <netinet/tcp.h>
	#include <sys/resource.h>
}

pub const (
	max_fds          = 1024
	max_queue        = 4096

	// events
	picoev_read      = 1
	picoev_write     = 2
	picoev_timeout   = 4
	picoev_add       = 0x40000000
	picoev_del       = 0x20000000
	picoev_readwrite = 3 // 1 xor 2
)

pub struct Target {
pub mut:
	fd      int
	loop_id int = -1
	events  u32
	cb      fn (int, int, voidptr)
	// can be used by backends (never modified by core)
	backend int
}

pub struct Config {
pub:
	port         int = 8080
	cb           fn (voidptr, picohttpparser.Request, mut picohttpparser.Response)
	err_cb       fn (voidptr, picohttpparser.Request, mut picohttpparser.Response, IError) = default_err_cb
	user_data    voidptr = unsafe { nil }
	timeout_secs int     = 8
	max_headers  int     = 100
	max_read     int     = 4096
	max_write    int     = 8192
}

[heap]
pub struct Picoev {
	cb        fn (voidptr, picohttpparser.Request, mut picohttpparser.Response)
	err_cb    fn (voidptr, picohttpparser.Request, mut picohttpparser.Response, IError) = default_err_cb
	user_data voidptr = unsafe { nil }

	timeout_secs int
	max_headers  int = 100
	max_read     int = 4096
	max_write    int = 8192
mut:
	loop             &LoopType = unsafe { nil }
	file_descriptors [max_fds]&Target
	timeouts         map[int]i64
	num_loops        int

	buf &u8 = unsafe { nil }
	idx [1024]int
	out &u8 = unsafe { nil }
}

pub fn (mut pv Picoev) init() {
	assert picoev.max_fds > 0

	pv.num_loops = 0

	for i in 0 .. picoev.max_fds {
		pv.file_descriptors[i] = &Target{}
	}
}

[direct_array_access]
pub fn (mut pv Picoev) add(fd int, events int, timeout int, cb voidptr) int {
	assert fd < picoev.max_fds

	mut target := pv.file_descriptors[fd]
	target.fd = fd
	target.cb = cb
	target.loop_id = pv.loop.id
	target.events = 0

	if pv.update_events(fd, events | picoev.picoev_add) != 0 {
		pv.del(fd)
		return -1
	}

	// update timeout
	pv.set_timeout(fd, timeout)
	return 0
}

[direct_array_access]
fn (mut pv Picoev) del(fd int) int {
	assert fd < picoev.max_fds
	mut target := pv.file_descriptors[fd]

	$if trace_fd ? {
		eprintln('delete ${fd}')
	}

	if pv.update_events(fd, picoev.picoev_del) != 0 {
		target.loop_id = -1
		target.fd = 0
		return -1
	}

	pv.set_timeout(fd, 0)
	target.loop_id = -1
	target.fd = 0
	return 0
}

fn (mut pv Picoev) loop_once(max_wait int) int {
	// time.now() is slow
	pv.loop.now = C.time(C.NULL)

	if pv.poll_once(max_wait) != 0 {
		return -1
	}

	if max_wait != 0 {
		pv.loop.now = C.time(C.NULL)
	}

	pv.handle_timeout()
	return 0
}

[direct_array_access; inline]
fn (mut pv Picoev) set_timeout(fd int, secs int) {
	assert fd < picoev.max_fds
	if secs != 0 {
		pv.timeouts[fd] = pv.loop.now + secs
	} else {
		pv.timeouts.delete(fd)
	}
}

[direct_array_access; inline]
fn (mut pv Picoev) handle_timeout() {
	for fd, timeout in pv.timeouts {
		if timeout <= pv.loop.now {
			target := pv.file_descriptors[fd]
			assert target.loop_id == pv.loop.id

			pv.timeouts.delete(fd)
			unsafe { target.cb(fd, picoev.picoev_timeout, &pv) }
		}
	}
}

fn accept_callback(fd int, events int, cb_arg voidptr) {
	mut pv := unsafe { &Picoev(cb_arg) }
	newfd := C.accept(fd, 0, 0)
	if newfd >= picoev.max_fds {
		// should never happen
		close_socket(fd)
		return
	}

	$if trace_fd ? {
		eprintln('accept ${newfd}')
	}

	if newfd != -1 {
		setup_sock(newfd) or {
			eprintln('socket fail: ${newfd}, old ${fd} ${err.code()}')
			pv.err_cb(pv.user_data, picohttpparser.Request{}, mut &picohttpparser.Response{},
				err)
			return
		}
		pv.add(newfd, picoev.picoev_read, pv.timeout_secs, raw_callback)
	}
}

[inline]
pub fn (mut pv Picoev) close_conn(fd int) {
	pv.del(fd)
	close_socket(fd)
}

[inline]
fn close_socket(fd int) {
	$if trace_fd ? {
		eprintln('close ${fd}')
	}

	$if windows {
		C.closesocket(fd)
	} $else {
		C.close(fd)
	}
}

[inline]
fn setup_sock(fd int) ! {
	flag := 1


	if C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_NODELAY, &flag, sizeof(int)) < 0 {
		return error('setup_sock.setup_sock failed')
	}

	$if freebsd {
		if C.fcntl(fd, C.F_SETFL, C.SOCK_NONBLOCK) != 0 {
			return error('fcntl failed')
		}
	} $else $if windows {
		non_blocking_mode := u32(1)
		if C.ioctlsocket(fd, C.FIONBIO, &non_blocking_mode) == C.SOCKET_ERROR {
			return error('icotlsocket failed')
		}
	} $else {
		if C.fcntl(fd, C.F_SETFL, C.O_NONBLOCK) != 0 {
			return error('fcntl failed')
		}
	}
}

[inline]
fn req_read(fd int, b &u8, max_len int, idx int) int {
	// use `recv` instead of `read` for windows compatibility
	unsafe {
		return C.recv(fd, b + idx, max_len - idx, 0)
	}
}

[direct_array_access]
fn raw_callback(fd int, events int, context voidptr) {
	mut pv := unsafe { &Picoev(context) }
	defer {
		pv.idx[fd] = 0
	}

	if events & picoev.picoev_timeout != 0 {
		$if trace_fd ? {
			eprintln('timeout ${fd}')
		}
		pv.close_conn(fd)
		return
	} else if events & picoev.picoev_read != 0 {
		pv.set_timeout(fd, pv.timeout_secs)

		mut buf := pv.buf
		unsafe {
			buf += fd * pv.max_read // pointer magic
		}
		mut req := picohttpparser.Request{}

		// Response init
		mut out := pv.out
		unsafe {
			out += fd * pv.max_write // pointer magic
		}
		mut res := picohttpparser.Response{
			fd: fd
			buf_start: out
			buf: out
		}

		for {
			// Request parsing loop
			r := req_read(fd, buf, pv.max_read, pv.idx[fd]) // Get data from socket
			if r == 0 {
				// connection closed by peer
				pv.close_conn(fd)
				return
			} else if r == -1 {
				if C.errno == C.EAGAIN {
					// try again later
					return
				}
				$if windows {
					if C.errno == C.WSAEWOULDBLOCK {
						// try again later
						return
					}
				} $else {
					if C.errno == C.EWOULDBLOCK {
						// try again later
						return
					}
				}
				$if trace_fd ? {
					eprintln('fatal error ${fd}: ${C.errno}')
				}

				// fatal error
				pv.close_conn(fd)
				return
			}
			pv.idx[fd] += r

			mut s := unsafe { tos(buf, pv.idx[fd]) }
			pret := req.parse_request(s, pv.max_headers) // Parse request via picohttpparser
			if pret > 0 { // Success
				break
			} else if pret == -1 { // Parse error
				pv.err_cb(pv.user_data, req, mut &res, error('ParseError'))
				return
			}

			assert pret == -2

			// request is incomplete, continue the loop
			if pv.idx[fd] == sizeof(buf) {
				pv.err_cb(pv.user_data, req, mut &res, error('RequestIsTooLongError'))
				return
			}
		}

		// Callback (should call .end() itself)
		pv.cb(pv.user_data, req, mut &res)
	}
}

fn default_err_cb(data voidptr, req picohttpparser.Request, mut res picohttpparser.Response, error IError) {
	eprintln('picoev: ${error}')
	res.end()
}

// new creates a `Picoev` struct and initializes the main loop
pub fn new(config Config) &Picoev {
	// not using the `net` modules sockets, because not all socket options are defined
	fd := C.socket(net.AddrFamily.ip, net.SocketType.tcp, 0)
	assert fd != -1

	$if trace_fd ? {
		println('listen: ${fd}')
	}

	// Setting flags for socket
	flag := 1
	assert C.setsockopt(fd, C.SOL_SOCKET, C.SO_REUSEADDR, &flag, sizeof(int)) == 0

	$if linux {
		// epoll socket options
		assert C.setsockopt(fd, C.SOL_SOCKET, C.SO_REUSEPORT, &flag, sizeof(int)) == 0
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_QUICKACK, &flag, sizeof(int)) == 0
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_DEFER_ACCEPT, &config.timeout_secs,
			sizeof(int)) == 0
		queue_len := picoev.max_queue
		assert C.setsockopt(fd, C.IPPROTO_TCP, C.TCP_FASTOPEN, &queue_len, sizeof(int)) == 0
	}

	// addr settings
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

	mut pv := &Picoev{
		num_loops: 1
		cb: config.cb
		err_cb: config.err_cb
		user_data: config.user_data
		timeout_secs: config.timeout_secs
		max_headers: config.max_headers
		max_read: config.max_read
		max_write: config.max_write
		buf: unsafe { malloc_noscan(picoev.max_fds * config.max_read + 1) }
		out: unsafe { malloc_noscan(picoev.max_fds * config.max_write + 1) }
	}

	// epoll for linux
	// kqueue for macos and freebsd
	// select for windows and others
	$if linux {
		pv.loop = create_epoll_loop(0) or { panic(err) }
	} $else $if macos || freebsd {
		pv.loop = create_kqueue_loop(0) or { panic(err) }
	} $else {
		pv.loop = create_select_loop(0) or { panic(err) }
	}

	pv.init()

	pv.add(fd, picoev.picoev_read, 0, accept_callback)
	return pv
}

pub fn (mut pv Picoev) serve() {
	for {
		pv.loop_once(1)
	}
}
