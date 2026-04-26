module fasthttp

import net
import sync.stdatomic
import time

#include <sys/event.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/uio.h>
#include <signal.h>

fn C.signal(sig int, handler voidptr) voidptr

const buf_size = max_connection_size
const kqueue_max_events = 128
const backlog = max_connection_size
const kqueue_wait_timeout_ms = 100

// send_flags is OR'd into every C.send() call in this file.  On OpenBSD,
// which lacks the per-socket SO_NOSIGPIPE option, we pass MSG_NOSIGNAL
// instead so writes to a disconnected peer return EPIPE rather than
// killing the process with SIGPIPE.  Other BSDs use SO_NOSIGPIPE set on
// the socket at accept time (see accept_clients).
const send_flags = $if openbsd { int(C.MSG_NOSIGNAL) } $else { 0 }

fn C.kevent(kq i32, changelist &C.kevent, nchanges i32, eventlist &C.kevent, nevents i32, timeout &C.timespec) i32
fn C.kqueue() i32
fn C.fstat(fd i32, buf &C.stat) i32

// send_file_bytes has three implementations across BSD-family OSes:
//   macOS:    int sendfile(int fd, int s, off_t offset, off_t *len, sf_hdtr *hdtr, int flags);
//             (len is in/out: caller sets bytes-to-send, kernel writes bytes-actually-sent)
//   FreeBSD/NetBSD/DragonFly:
//             int sendfile(int fd, int s, off_t offset, size_t nbytes, sf_hdtr *hdtr, off_t *sbytes, int flags);
//             (nbytes is input, sbytes is the separate out-param for bytes actually sent)
//   OpenBSD:  no sendfile(2) syscall at all.  We fall back to a single
//             pread(2) + send(2) pair per call, using a bounded stack
//             buffer.  The outer send_pending() loop will call us again
//             until the file is drained or the socket blocks.

const sendfile_fallback_buf_size = 16384

// send_file_bytes asks the kernel to send up to `nbytes` bytes from `file_fd` at
// `offset` into socket `sock_fd`, in a single non-blocking operation.
// Returns (ret, sent) where `ret` is 0 on success or -1 on error (errno set),
// and `sent` is the number of bytes transferred this call (may be >0 even when ret==-1).
fn send_file_bytes(file_fd i32, sock_fd i32, offset i64, nbytes i64) (int, i64) {
	$if macos {
		mut len := nbytes
		ret := C.sendfile(file_fd, sock_fd, offset, &len, unsafe { nil }, 0)
		return int(ret), len
	} $else $if openbsd {
		// No sendfile(2) on OpenBSD; pread into a stack buffer, then send.
		// Cap one call at sendfile_fallback_buf_size so we don't starve
		// other connections in the kqueue loop.
		mut buf := [sendfile_fallback_buf_size]u8{}
		mut want := nbytes
		if want > sendfile_fallback_buf_size {
			want = sendfile_fallback_buf_size
		}
		nread := C.pread(file_fd, &buf[0], usize(want), offset)
		if nread <= 0 {
			// nread == 0 is EOF (shouldn't happen given write_pos < file_len
			// guards in send_pending, but treat it as an error to close the
			// connection); nread < 0 propagates errno for EAGAIN handling.
			return -1, i64(0)
		}
		nsent := C.send(sock_fd, &buf[0], usize(nread), send_flags)
		if nsent < 0 {
			return -1, i64(0)
		}
		return 0, i64(nsent)
	} $else {
		mut sbytes := i64(0)
		ret := C.sendfile(file_fd, sock_fd, offset, usize(nbytes), unsafe { nil }, &sbytes, 0)
		return int(ret), sbytes
	}
}

$if macos {
	// int sendfile(int fd, int s, off_t offset, off_t *len, struct sf_hdtr *hdtr, int flags);
	fn C.sendfile(fd i32, s i32, offset i64, len &i64, hdtr voidptr, flags i32) i32
} $else $if openbsd {
	// ssize_t pread(int fd, void *buf, size_t nbyte, off_t offset);
	fn C.pread(fd i32, buf voidptr, nbyte usize, offset i64) isize
} $else {
	// int sendfile(int fd, int s, off_t offset, size_t nbytes, struct sf_hdtr *hdtr, off_t *sbytes, int flags);
	fn C.sendfile(fd i32, s i32, offset i64, nbytes usize, hdtr voidptr, sbytes &i64, flags i32) i32
}

struct C.kevent {
	ident  u64
	filter i16
	flags  u16
	fflags u32
	data   isize
	udata  voidptr
}

// Helper to set fields of a kevent struct.
fn ev_set(mut ev C.kevent, ident u64, filter i16, flags u16, fflags u32, data isize, udata voidptr) {
	ev.ident = ident
	ev.filter = filter
	ev.flags = flags
	ev.fflags = fflags
	ev.data = data
	ev.udata = udata
}

struct Conn {
	fd        int
	user_data voidptr
mut:
	read_buf       [buf_size]u8
	read_len       int
	read_extra     []u8 // dynamic overflow buffer for large requests (e.g. chunked uploads)
	write_buf      []u8
	write_pos      int
	request_active bool
	read_start     i64 // monotonic timestamp (in microseconds) when first data was received

	// Sendfile state
	file_fd      int = -1
	file_len     i64
	file_pos     i64
	should_close bool
}

pub struct Server {
pub mut:
	family                  net.AddrFamily = .ip6
	port                    int
	max_request_buffer_size int = 8192
	timeout_in_seconds      int = 30
	socket_fd               int = -1
	poll_fd                 int = -1 // kqueue fd
	user_data               voidptr
	request_handler         fn (HttpRequest) !HttpResponse @[required]
	running                 &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
	shutting_down           &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
	stopped                 &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(true)
	active_requests         &stdatomic.AtomicVal[int]  = stdatomic.new_atomic(0)
}

// new_server creates and initializes a new Server instance.
pub fn new_server(config ServerConfig) !&Server {
	mut server := &Server{
		family:                  config.family
		port:                    config.port
		max_request_buffer_size: config.max_request_buffer_size
		timeout_in_seconds:      config.timeout_in_seconds
		user_data:               config.user_data
		request_handler:         config.handler
		running:                 stdatomic.new_atomic(false)
		shutting_down:           stdatomic.new_atomic(false)
		stopped:                 stdatomic.new_atomic(true)
		active_requests:         stdatomic.new_atomic(0)
	}
	return server
}

fn set_nonblocking(fd int) {
	flags := C.fcntl(fd, C.F_GETFL, 0)
	if flags == -1 {
		return
	}
	C.fcntl(fd, C.F_SETFL, flags | C.O_NONBLOCK)
}

fn add_event(kq int, ident u64, filter i16, flags u16, udata voidptr) int {
	mut ev := C.kevent{}
	ev_set(mut &ev, ident, filter, flags, u32(0), isize(0), udata)
	return C.kevent(kq, &ev, 1, unsafe { nil }, 0, unsafe { nil })
}

fn delete_event(kq int, ident u64, filter i16, udata voidptr) {
	mut ev := C.kevent{}
	ev_set(mut &ev, ident, filter, u16(C.EV_DELETE), u32(0), isize(0), udata)
	C.kevent(kq, &ev, 1, unsafe { nil }, 0, unsafe { nil })
}

fn close_conn(server Server, kq int, c_ptr voidptr, mut clients map[int]voidptr) {
	mut c := unsafe { &Conn(c_ptr) }
	clients.delete(c.fd)
	delete_event(kq, u64(c.fd), i16(C.EVFILT_READ), c)
	delete_event(kq, u64(c.fd), i16(C.EVFILT_WRITE), c)
	C.close(c.fd)
	if c.request_active {
		server.end_request()
		c.request_active = false
	}
	if c.write_buf.cap > 0 {
		unsafe { c.write_buf.free() }
	}
	if c.read_extra.cap > 0 {
		unsafe { c.read_extra.free() }
	}
	if c.file_fd != -1 {
		C.close(c.file_fd)
		c.file_fd = -1
	}
	unsafe { free(c_ptr) }
}

fn send_pending(c_ptr voidptr) bool {
	mut c := unsafe { &Conn(c_ptr) }

	// 1. Send memory buffer (headers or small response)
	if c.write_pos < c.write_buf.len {
		remaining := c.write_buf.len - c.write_pos
		write_ptr := unsafe { &c.write_buf[0] + c.write_pos }
		sent := C.send(c.fd, write_ptr, remaining, send_flags)
		if sent > 0 {
			c.write_pos += int(sent)
		}
		if sent < 0 {
			if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
				return true
			}
			c.should_close = true
			return false
		}
	}

	// 2. Send file if buffer is fully sent
	if c.write_pos >= c.write_buf.len && c.file_fd != -1 {
		remaining := c.file_len - c.file_pos
		ret, sent := send_file_bytes(c.file_fd, c.fd, c.file_pos, remaining)
		if sent > 0 {
			c.file_pos += sent
		}
		if ret == -1 {
			if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
				return true
			}
			C.close(c.file_fd)
			c.file_fd = -1
			c.should_close = true
			return false
		}
		if c.file_pos >= c.file_len {
			C.close(c.file_fd)
			c.file_fd = -1
		} else {
			return true
		}
	}

	return !(c.write_pos >= c.write_buf.len && c.file_fd == -1)
}

const status_408_response = 'HTTP/1.1 408 Request Timeout\r\nContent-Type: text/plain\r\nContent-Length: 19\r\nConnection: close\r\n\r\n408 Request Timeout'.bytes()

fn send_bad_request(fd int) {
	C.send(fd, tiny_bad_request_response.data, tiny_bad_request_response.len, send_flags)
}

fn send_request_timeout(fd int) {
	C.send(fd, status_408_response.data, status_408_response.len, send_flags)
}

fn handle_write(server Server, kq int, c_ptr voidptr, mut clients map[int]voidptr) {
	if send_pending(c_ptr) {
		return
	}
	complete_response(server, kq, c_ptr, mut clients, true)
}

fn complete_response(server Server, kq int, c_ptr voidptr, mut clients map[int]voidptr, remove_write_event bool) {
	mut c := unsafe { &Conn(c_ptr) }
	if remove_write_event {
		delete_event(kq, u64(c.fd), i16(C.EVFILT_WRITE), c)
	}
	if server.is_shutting_down() || c.should_close {
		close_conn(server, kq, c_ptr, mut clients)
		return
	}
	if c.request_active {
		server.end_request()
		c.request_active = false
	}
	c.write_buf.clear()
	c.write_pos = 0
	c.read_len = 0
	if c.read_extra.cap > 0 {
		unsafe { c.read_extra.free() }
		c.read_extra = []u8{}
	}
	c.read_start = 0
	c.should_close = false
}

// process_request handles a complete HTTP request: decodes, calls the handler,
// sends the response (or handles takeover/sendfile). Runs in a spawned thread.
fn process_request(server Server, kq int, c_ptr voidptr, mut clients map[int]voidptr) {
	mut c := unsafe { &Conn(c_ptr) }

	mut req_buf := c.get_full_request_data()
	if c.read_extra.cap > 0 {
		unsafe { c.read_extra.free() }
		c.read_extra = []u8{}
	}

	mut decoded := decode_http_request(req_buf) or {
		send_bad_request(c.fd)
		close_conn(server, kq, c_ptr, mut clients)
		return
	}
	server.begin_request()
	c.request_active = true
	decoded.client_conn_fd = c.fd
	decoded.user_data = server.user_data

	resp := server.request_handler(decoded) or {
		send_bad_request(c.fd)
		close_conn(server, kq, c_ptr, mut clients)
		return
	}

	if resp.takeover {
		// The handler has taken ownership of the connection.
		// Remove from kqueue and tracking, but do NOT close the fd.
		clients.delete(c.fd)
		delete_event(kq, u64(c.fd), i16(C.EVFILT_READ), c)
		delete_event(kq, u64(c.fd), i16(C.EVFILT_WRITE), c)
		if c.request_active {
			server.end_request()
			c.request_active = false
		}
		unsafe { free(c_ptr) }
		return
	}

	c.should_close = resp.should_close
	c.write_buf = resp.content.clone()
	if resp.file_path != '' {
		fd := C.open(resp.file_path.str, C.O_RDONLY)
		if fd != -1 {
			mut st := C.stat{}
			if C.fstat(fd, &st) == 0 {
				c.file_fd = fd
				c.file_len = st.st_size
				c.file_pos = 0
			} else {
				C.close(fd)
			}
		}
	}

	c.write_pos = 0
	c.read_len = 0
	c.read_extra.clear()
	c.read_start = 0

	if send_pending(c_ptr) {
		add_event(kq, u64(c.fd), i16(C.EVFILT_WRITE), u16(C.EV_ADD | C.EV_ENABLE | C.EV_CLEAR), c)
		return
	}

	complete_response(server, kq, c_ptr, mut clients, false)
}

// total_read_len returns the total number of request bytes received so far,
// including both the fixed read_buf and the dynamic read_extra overflow.
fn (c &Conn) total_read_len() int {
	return c.read_len + c.read_extra.len
}

// get_full_request_data copies the complete received data into a single []u8.
fn (c &Conn) get_full_request_data() []u8 {
	total := c.total_read_len()
	mut req_buf := []u8{cap: total}
	unsafe {
		req_buf.push_many(&c.read_buf[0], c.read_len)
	}
	if c.read_extra.len > 0 {
		req_buf << c.read_extra
	}
	return req_buf
}

fn handle_read(server Server, kq int, c_ptr voidptr, mut clients map[int]voidptr) {
	mut c := unsafe { &Conn(c_ptr) }

	// Drain the socket for this kqueue notification. EV_CLEAR only rearms once
	// all readable data has been consumed.
	for {
		if c.read_len < buf_size {
			n := C.recv(c.fd, &c.read_buf[c.read_len], buf_size - c.read_len, 0)
			if n < 0 {
				if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
					break
				}
				C.send(c.fd, status_444_response.data, status_444_response.len, send_flags)
				close_conn(server, kq, c_ptr, mut clients)
				return
			}
			if n == 0 {
				if c.total_read_len() == 0 {
					close_conn(server, kq, c_ptr, mut clients)
					return
				}
				break
			}
			c.read_len += int(n)
		} else {
			// Fixed buffer is full, read the rest into dynamic overflow.
			mut tmp := []u8{len: 65536}
			n := C.recv(c.fd, tmp.data, tmp.len, 0)
			if n < 0 {
				if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
					break
				}
				C.send(c.fd, status_444_response.data, status_444_response.len, send_flags)
				close_conn(server, kq, c_ptr, mut clients)
				return
			}
			if n == 0 {
				if c.total_read_len() == 0 {
					close_conn(server, kq, c_ptr, mut clients)
					return
				}
				break
			}
			c.read_extra << tmp[..int(n)]
		}
	}

	total := c.total_read_len()
	if total == 0 {
		return
	}

	// Enforce the configured header limit without capping large request bodies.
	mut header_end := -1
	mut full_data := []u8{}
	if c.read_extra.len > 0 {
		full_data = c.get_full_request_data()
		header_end = find_header_end_in_buf(full_data.data, full_data.len)
	} else {
		header_end = find_header_end_in_buf(&c.read_buf[0], c.read_len)
	}
	if (header_end == -1 && total >= server.max_request_buffer_size)
		|| header_end > server.max_request_buffer_size {
		C.send(c.fd, status_413_response.data, status_413_response.len, send_flags)
		close_conn(server, kq, c_ptr, mut clients)
		return
	}

	// Record when we first started receiving data for this request
	if c.read_start == 0 {
		c.read_start = time.sys_mono_now()
	}

	// Check if the full body has been received.
	if c.read_extra.len > 0 {
		if !has_complete_body(full_data.data, full_data.len) {
			elapsed_ns := time.sys_mono_now() - c.read_start
			timeout_ns := i64(server.timeout_in_seconds) * 1_000_000_000
			if elapsed_ns >= timeout_ns {
				send_request_timeout(c.fd)
				close_conn(server, kq, c_ptr, mut clients)
			}
			return
		}
	} else if !has_complete_body(&c.read_buf[0], c.read_len) {
		// Body not complete yet - check for timeout
		elapsed_ns := time.sys_mono_now() - c.read_start
		timeout_ns := i64(server.timeout_in_seconds) * 1_000_000_000
		if elapsed_ns >= timeout_ns {
			send_request_timeout(c.fd)
			close_conn(server, kq, c_ptr, mut clients)
		}
		// Otherwise wait for more data on the next kqueue event
		return
	}

	process_request(server, kq, c_ptr, mut clients)
}

fn accept_clients(kq int, listen_fd int, mut clients map[int]voidptr) {
	for {
		client_fd := C.accept(listen_fd, unsafe { nil }, unsafe { nil })
		if client_fd < 0 {
			if C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK {
				break
			}
			C.perror(c'accept')
			break
		}
		set_nonblocking(client_fd)
		// Prevent SIGPIPE on writes to disconnected clients.  macOS and
		// FreeBSD/NetBSD/DragonFly expose the per-socket SO_NOSIGPIPE
		// option; OpenBSD does not, and instead expects MSG_NOSIGNAL on
		// each send(2) call (handled via the `send_flags` const above).
		$if !openbsd {
			nosigpipe_opt := 1
			C.setsockopt(client_fd, C.SOL_SOCKET, C.SO_NOSIGPIPE, &nosigpipe_opt, sizeof(int))
		}
		mut c := &Conn{
			fd:        client_fd
			user_data: unsafe { nil }
			file_fd:   -1
		}
		add_event(kq, u64(client_fd), i16(C.EVFILT_READ), u16(C.EV_ADD | C.EV_ENABLE | C.EV_CLEAR),
			c)
		clients[client_fd] = c
	}
}

fn close_all_conns(server Server, kq int, mut clients map[int]voidptr) {
	for client_fd in clients.keys() {
		c_ptr := clients[client_fd] or { continue }
		close_conn(server, kq, c_ptr, mut clients)
	}
}

fn (mut s Server) stop_accepting() {
	if s.poll_fd >= 0 && s.socket_fd >= 0 {
		delete_event(s.poll_fd, u64(s.socket_fd), i16(C.EVFILT_READ), unsafe { nil })
	}
	if s.socket_fd >= 0 {
		C.close(s.socket_fd)
		s.socket_fd = -1
	}
}

// run starts the server and enters the main event loop (Kqueue version).
pub fn (mut s Server) run() ! {
	// Ignore SIGPIPE process-wide.  Writing to a disconnected socket raises
	// SIGPIPE by default, which kills the process.  We suppress it per-send
	// (SO_NOSIGPIPE on macos/freebsd/netbsd/dragonfly, MSG_NOSIGNAL on
	// openbsd), but this signal handler is a safety net for any code path
	// that might miss it (e.g. spawned SSE/WebSocket threads using
	// TcpConn.write).
	C.signal(C.SIGPIPE, C.SIG_IGN)

	s.socket_fd = C.socket(s.family, net.SocketType.tcp, 0)
	if s.socket_fd < 0 {
		C.perror(c'socket')
		return error('socket creation failed')
	}

	opt := 1
	C.setsockopt(s.socket_fd, C.SOL_SOCKET, C.SO_REUSEADDR, &opt, sizeof(int))

	addr := if s.family == .ip6 {
		net.new_ip6(u16(s.port), [u8(0), 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]!)
	} else {
		net.new_ip(u16(s.port), [u8(0), 0, 0, 0]!)
	}
	alen := addr.len()

	if C.bind(s.socket_fd, voidptr(&addr), alen) < 0 {
		C.perror(c'bind')
		return error('socket bind failed')
	}
	if C.listen(s.socket_fd, backlog) < 0 {
		C.perror(c'listen')
		return error('socket listen failed')
	}

	set_nonblocking(s.socket_fd)

	s.poll_fd = C.kqueue()
	if s.poll_fd < 0 {
		C.perror(c'kqueue')
		return error('kqueue creation failed')
	}

	add_event(s.poll_fd, u64(s.socket_fd), i16(C.EVFILT_READ),
		u16(C.EV_ADD | C.EV_ENABLE | C.EV_CLEAR), unsafe { nil })

	listen_fd := s.socket_fd
	s.mark_running()
	println('listening on http://0.0.0.0:${s.port}/')

	mut events := [kqueue_max_events]C.kevent{}
	mut clients := map[int]voidptr{}
	for {
		if s.is_shutting_down() && s.active_request_count() == 0 {
			close_all_conns(s, s.poll_fd, mut clients)
			break
		}
		timeout := C.timespec{
			tv_sec:  0
			tv_nsec: kqueue_wait_timeout_ms * 1_000_000
		}
		nev := C.kevent(s.poll_fd, unsafe { nil }, 0, &events[0], kqueue_max_events, &timeout)
		if nev < 0 {
			if C.errno == C.EINTR {
				// kevent may return EINTR when the process receives a signal
				// (e.g. SIGCHLD from an exec'd subprocess). Treat like a timeout.
				continue
			}
			if s.is_shutting_down() {
				continue
			}
			C.perror(c'kevent')
			break
		}

		for i := 0; i < nev; i++ {
			event := events[i]
			if event.flags & u16(C.EV_ERROR) != 0 {
				if event.ident == u64(listen_fd) {
					C.perror(c'listener error')
					continue
				}
				if event.udata != unsafe { nil } {
					close_conn(s, s.poll_fd, event.udata, mut clients)
				}
				continue
			}

			if event.ident == u64(listen_fd) {
				if s.is_shutting_down() {
					continue
				}
				accept_clients(s.poll_fd, listen_fd, mut clients)
				continue
			}

			if event.udata == unsafe { nil } {
				continue
			}

			if event.flags & u16(C.EV_EOF) != 0 {
				close_conn(s, s.poll_fd, event.udata, mut clients)
				continue
			}

			if event.filter == i16(C.EVFILT_READ) {
				if s.is_shutting_down() {
					close_conn(s, s.poll_fd, event.udata, mut clients)
					continue
				}
				handle_read(s, s.poll_fd, event.udata, mut clients)
			} else if event.filter == i16(C.EVFILT_WRITE) {
				handle_write(s, s.poll_fd, event.udata, mut clients)
			}
		}
		// Sweep for connections waiting for body data that have timed out
		if s.timeout_in_seconds > 0 {
			now := time.sys_mono_now()
			timeout_ns := i64(s.timeout_in_seconds) * 1_000_000_000
			for client_fd in clients.keys() {
				c_ptr := clients[client_fd] or { continue }
				c := unsafe { &Conn(c_ptr) }
				if c.read_start > 0 && c.read_len > 0 && !c.request_active {
					elapsed := now - c.read_start
					if elapsed >= timeout_ns {
						send_request_timeout(c.fd)
						close_conn(s, s.poll_fd, c_ptr, mut clients)
					}
				}
			}
		}
	}

	if s.socket_fd >= 0 {
		C.close(s.socket_fd)
		s.socket_fd = -1
	}
	if s.poll_fd >= 0 {
		C.close(s.poll_fd)
		s.poll_fd = -1
	}
	s.mark_stopped()
}
