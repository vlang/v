module fasthttp

#include <sys/event.h>

fn C.accept(sockfd int, address &C.sockaddr_in, addrlen &u32) int
fn C.kevent(kq int, changelist &C.kevent, nchanges int, eventlist &C.kevent, nevents int, timeout &C.timespec) int
fn C.kqueue() int

struct C.kevent {
	ident  u64
	filter i16
	flags  u16
	fflags u32
	data   isize
	udata  voidptr
}

// Helper to set fields of a kevent struct
fn ev_set(mut ev C.kevent, ident u64, filter i16, flags u16, fflags u32, data isize, udata voidptr) {
	ev.ident = ident
	ev.filter = filter
	ev.flags = flags
	ev.fflags = fflags
	ev.data = data
	ev.udata = udata
}

const buf_size = max_connection_size
const kqueue_max_events = 128
const backlog = max_connection_size
const tiny_bad_request_d = 'HTTP/1.1 400 Bad Request\r\nContent-Length: 0\r\nConnection: close\r\n\r\n'.bytes()

struct Conn {
	fd int
mut:
	read_buf  [buf_size]u8
	read_len  int
	write_buf []u8
	write_pos int
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

fn close_conn(kq int, c_ptr voidptr) {
	mut c := unsafe { &Conn(c_ptr) }
	delete_event(kq, u64(c.fd), i16(C.EVFILT_READ), c)
	delete_event(kq, u64(c.fd), i16(C.EVFILT_WRITE), c)
	C.close(c.fd)
	c.write_buf.clear()
	unsafe { free(c_ptr) }
}

fn send_pending(c_ptr voidptr) bool {
	mut c := unsafe { &Conn(c_ptr) }
	if c.write_buf.len == 0 {
		return false
	}
	remaining := c.write_buf.len - c.write_pos
	if remaining <= 0 {
		return false
	}
	write_ptr := unsafe { &c.write_buf[0] + c.write_pos }
	sent := C.send(c.fd, write_ptr, remaining, 0)
	if sent > 0 {
		c.write_pos += int(sent)
	}
	if sent < 0 && (C.errno == C.EAGAIN || C.errno == C.EWOULDBLOCK) {
		return true
	}
	return c.write_pos < c.write_buf.len
}

fn send_bad_request(fd int) {
	C.send(fd, tiny_bad_request_d.data, tiny_bad_request_d.len, 0)
}

fn handle_write(kq int, c_ptr voidptr) {
	mut c := unsafe { &Conn(c_ptr) }
	if send_pending(c_ptr) {
		return
	}
	delete_event(kq, u64(c.fd), i16(C.EVFILT_WRITE), c)
	close_conn(kq, c_ptr)
}

fn handle_read(mut s Server, kq int, c_ptr voidptr) {
	mut c := unsafe { &Conn(c_ptr) }
	n := C.recv(c.fd, &c.read_buf[c.read_len], buf_size - c.read_len, 0)
	if n <= 0 {
		if n < 0 && C.errno != C.EAGAIN && C.errno != C.EWOULDBLOCK {
			close_conn(kq, c_ptr)
			return
		}
		close_conn(kq, c_ptr)
		return
	}

	c.read_len += int(n)
	if c.read_len == 0 {
		return
	}

	mut req_buf := []u8{cap: c.read_len}
	unsafe {
		req_buf.push_many(&c.read_buf[0], c.read_len)
	}

	mut decoded := decode_http_request(req_buf) or {
		send_bad_request(c.fd)
		close_conn(kq, c_ptr)
		return
	}
	decoded.client_conn_fd = c.fd

	resp := s.request_handler(decoded) or {
		send_bad_request(c.fd)
		close_conn(kq, c_ptr)
		return
	}

	c.write_buf = resp.clone()
	c.write_pos = 0
	c.read_len = 0

	if send_pending(c_ptr) {
		add_event(kq, u64(c.fd), i16(C.EVFILT_WRITE), u16(C.EV_ADD | C.EV_ENABLE | C.EV_CLEAR),
			c)
		return
	}

	close_conn(kq, c_ptr)
}

fn accept_clients(kq int, listen_fd int) {
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
		mut c := &Conn{
			fd: client_fd
		}
		add_event(kq, u64(client_fd), i16(C.EVFILT_READ), u16(C.EV_ADD | C.EV_ENABLE | C.EV_CLEAR),
			c)
	}
}

// run starts the server and enters the main event loop (Kqueue version).
pub fn (mut s Server) run() {
	s.socket_fd = C.socket(C.AF_INET, C.SOCK_STREAM, 0)
	if s.socket_fd < 0 {
		C.perror(c'socket')
		return
	}

	opt := 1
	C.setsockopt(s.socket_fd, C.SOL_SOCKET, C.SO_REUSEADDR, &opt, sizeof(int))

	mut addr := C.sockaddr_in{}
	unsafe {
		C.memset(&addr, 0, sizeof(addr))
	}
	addr.sin_family = u16(C.AF_INET)
	addr.sin_port = u16(C.htons(u16(s.port)))

	if C.bind(s.socket_fd, voidptr(&addr), sizeof(addr)) < 0 {
		C.perror(c'bind')
		return
	}
	if C.listen(s.socket_fd, backlog) < 0 {
		C.perror(c'listen')
		return
	}

	set_nonblocking(s.socket_fd)

	s.poll_fd = C.kqueue()
	if s.poll_fd < 0 {
		C.perror(c'kqueue')
		return
	}

	add_event(s.poll_fd, u64(s.socket_fd), i16(C.EVFILT_READ), u16(C.EV_ADD | C.EV_ENABLE | C.EV_CLEAR),
		unsafe { nil })

	println('listening on http://localhost:${s.port}/')

	mut events := [kqueue_max_events]C.kevent{}
	for {
		nev := C.kevent(s.poll_fd, unsafe { nil }, 0, &events[0], kqueue_max_events, unsafe { nil })
		if nev < 0 {
			C.perror(c'kevent')
			break
		}

		for i := 0; i < nev; i++ {
			event := events[i]
			if event.flags & u16(C.EV_ERROR) != 0 {
				if event.ident == u64(s.socket_fd) {
					C.perror(c'listener error')
					continue
				}
				if event.udata != unsafe { nil } {
					close_conn(s.poll_fd, event.udata)
				}
				continue
			}

			if event.ident == u64(s.socket_fd) {
				accept_clients(s.poll_fd, s.socket_fd)
				continue
			}

			if event.udata == unsafe { nil } {
				continue
			}

			if event.flags & u16(C.EV_EOF) != 0 {
				close_conn(s.poll_fd, event.udata)
				continue
			}

			if event.filter == i16(C.EVFILT_READ) {
				handle_read(mut s, s.poll_fd, event.udata)
			} else if event.filter == i16(C.EVFILT_WRITE) {
				handle_write(s.poll_fd, event.udata)
			}
		}
	}

	C.close(s.socket_fd)
	C.close(s.poll_fd)
}
