module net

import time

const (
	tcp_default_read_timeout  = 30 * time.second
	tcp_default_write_timeout = 30 * time.second
)

pub struct TcpConn {
pub mut:
	sock                 TcpSocket
	max_write_chunk_size int = 4096
mut:
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
}

pub fn dial_tcp(address string) ?&TcpConn {
	mut s := new_tcp_socket() ?
	s.connect(address) ?
	return &TcpConn{
		sock: s
		read_timeout: net.tcp_default_read_timeout
		write_timeout: net.tcp_default_write_timeout
	}
}

pub fn (mut c TcpConn) close() ? {
	$if trace_tcp ? {
		eprintln('    TcpConn.close | c.sock.handle: ${c.sock.handle:6}')
	}
	c.sock.close() ?
	return none
}

// write_ptr blocks and attempts to write all data
pub fn (mut c TcpConn) write_ptr(b &byte, len int) ?int {
	$if trace_tcp ? {
		eprintln('>>> TcpConn.write_ptr | c.sock.handle: ${c.sock.handle:6} | b: ${ptr_str(b)} len: ${len:6}')
	}
	$if trace_tcp_data_write ? {
		eprintln('>>> TcpConn.write_ptr | data.len: ${len:6} | data: ' +
			unsafe { b.vstring_with_len(len) })
	}
	unsafe {
		mut ptr_base := &byte(b)
		mut total_sent := 0
		for total_sent < len {
			ptr := ptr_base + total_sent
			mut chunk_size := len - total_sent
			if chunk_size > c.max_write_chunk_size {
				chunk_size = c.max_write_chunk_size
			}
			mut sent := C.send(c.sock.handle, ptr, chunk_size, msg_nosignal)
			$if trace_tcp_data_write ? {
				eprintln('>>> TcpConn.write_ptr | data chunk, total_sent: ${total_sent:6}, chunk_size: ${chunk_size:6}, sent: ${sent:6}, ptr: ${ptr_str(ptr)}')
			}
			if sent < 0 {
				code := error_code()
				if code == int(error_ewouldblock) {
					c.wait_for_write() ?
					continue
				} else {
					wrap_error(code) ?
				}
			}
			total_sent += sent
		}
		return total_sent
	}
}

// write blocks and attempts to write all data
pub fn (mut c TcpConn) write(bytes []byte) ?int {
	return c.write_ptr(bytes.data, bytes.len)
}

// write_str blocks and attempts to write all data
[deprecated: 'use TcpConn.write_string() instead']
pub fn (mut c TcpConn) write_str(s string) ?int {
	return c.write_ptr(s.str, s.len)
}

// write_string blocks and attempts to write all data
pub fn (mut c TcpConn) write_string(s string) ?int {
	return c.write_ptr(s.str, s.len)
}

pub fn (mut c TcpConn) read_ptr(buf_ptr &byte, len int) ?int {
	mut res := wrap_read_result(C.recv(c.sock.handle, buf_ptr, len, 0)) ?
	$if trace_tcp ? {
		eprintln('<<< TcpConn.read_ptr  | c.sock.handle: ${c.sock.handle:6} | buf_ptr: ${ptr_str(buf_ptr):8} len: ${len:6} | res: ${res:6}')
	}
	if res > 0 {
		$if trace_tcp_data_read ? {
			eprintln('<<< TcpConn.read_ptr  | 1 data.len: ${res:6} | data: ' +
				unsafe { buf_ptr.vstring_with_len(res) })
		}
		return res
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_read() ?
		res = wrap_read_result(C.recv(c.sock.handle, buf_ptr, len, 0)) ?
		$if trace_tcp ? {
			eprintln('<<< TcpConn.read_ptr  | c.sock.handle: ${c.sock.handle:6} | buf_ptr: ${ptr_str(buf_ptr):8} len: ${len:6} | res: ${res:6}')
		}
		$if trace_tcp_data_read ? {
			if res > 0 {
				eprintln('<<< TcpConn.read_ptr  | 2 data.len: ${res:6} | data: ' +
					unsafe { buf_ptr.vstring_with_len(res) })
			}
		}
		return socket_error(res)
	} else {
		wrap_error(code) ?
	}
	return none
}

pub fn (mut c TcpConn) read(mut buf []byte) ?int {
	return c.read_ptr(buf.data, buf.len)
}

pub fn (mut c TcpConn) read_deadline() ?time.Time {
	if c.read_deadline.unix == 0 {
		return c.read_deadline
	}
	return none
}

pub fn (mut c TcpConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

pub fn (mut c TcpConn) write_deadline() ?time.Time {
	if c.write_deadline.unix == 0 {
		return c.write_deadline
	}
	return none
}

pub fn (mut c TcpConn) set_write_deadline(deadline time.Time) {
	c.write_deadline = deadline
}

pub fn (c &TcpConn) read_timeout() time.Duration {
	return c.read_timeout
}

pub fn (mut c TcpConn) set_read_timeout(t time.Duration) {
	c.read_timeout = t
}

pub fn (c &TcpConn) write_timeout() time.Duration {
	return c.write_timeout
}

pub fn (mut c TcpConn) set_write_timeout(t time.Duration) {
	c.write_timeout = t
}

[inline]
pub fn (mut c TcpConn) wait_for_read() ? {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

[inline]
pub fn (mut c TcpConn) wait_for_write() ? {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}

pub fn (c &TcpConn) peer_addr() ?Addr {
	mut addr := C.sockaddr{}
	len := sizeof(C.sockaddr)
	socket_error(C.getpeername(c.sock.handle, &addr, &len)) ?
	return new_addr(addr)
}

pub fn (c &TcpConn) peer_ip() ?string {
	buf := [44]char{}
	peeraddr := C.sockaddr_in{}
	speeraddr := sizeof(peeraddr)
	socket_error(C.getpeername(c.sock.handle, unsafe { &C.sockaddr(&peeraddr) }, &speeraddr)) ?
	cstr := &char(C.inet_ntop(SocketFamily.inet, &peeraddr.sin_addr, &buf[0], sizeof(buf)))
	if cstr == 0 {
		return error('net.peer_ip: inet_ntop failed')
	}
	res := unsafe { cstring_to_vstring(cstr) }
	return res
}

pub fn (c TcpConn) str() string {
	s := c.sock.str().replace('\n', ' ').replace('  ', ' ')
	return 'TcpConn{ write_deadline: $c.write_deadline, read_deadline: $c.read_deadline, read_timeout: $c.read_timeout, write_timeout: $c.write_timeout, sock: $s }'
}

pub struct TcpListener {
pub mut:
	sock TcpSocket
mut:
	accept_timeout  time.Duration
	accept_deadline time.Time
}

pub fn listen_tcp(port int) ?&TcpListener {
	s := new_tcp_socket() ?
	validate_port(port) ?
	mut addr := C.sockaddr_in{}
	addr.sin_family = int(SocketFamily.inet)
	addr.sin_port = C.htons(port)
	addr.sin_addr.s_addr = C.htonl(C.INADDR_ANY)
	size := sizeof(C.sockaddr_in)
	// cast to the correct type
	sockaddr := unsafe { &C.sockaddr(&addr) }
	socket_error(C.bind(s.handle, sockaddr, size)) ?
	socket_error(C.listen(s.handle, 128)) ?
	return &TcpListener{
		sock: s
		accept_deadline: no_deadline
		accept_timeout: infinite_timeout
	}
}

pub fn (mut l TcpListener) accept() ?&TcpConn {
	$if trace_tcp ? {
		eprintln('    TcpListener.accept | l.sock.handle: ${l.sock.handle:6}')
	}
	addr := C.sockaddr_storage{}
	unsafe { C.memset(&addr, 0, sizeof(C.sockaddr_storage)) }
	size := sizeof(C.sockaddr_storage)
	// cast to correct type
	sock_addr := unsafe { &C.sockaddr(&addr) }
	mut new_handle := C.accept(l.sock.handle, sock_addr, &size)
	if new_handle <= 0 {
		l.wait_for_accept() ?
		new_handle = C.accept(l.sock.handle, sock_addr, &size)
		if new_handle == -1 || new_handle == 0 {
			return none
		}
	}
	new_sock := tcp_socket_from_handle(new_handle) ?
	$if trace_tcp ? {
		eprintln('    TcpListener.accept | << new_sock.handle: ${new_sock.handle:6}')
	}
	return &TcpConn{
		sock: new_sock
		read_timeout: net.tcp_default_read_timeout
		write_timeout: net.tcp_default_write_timeout
	}
}

pub fn (c &TcpListener) accept_deadline() ?time.Time {
	if c.accept_deadline.unix != 0 {
		return c.accept_deadline
	}
	return none
}

pub fn (mut c TcpListener) set_accept_deadline(deadline time.Time) {
	c.accept_deadline = deadline
}

pub fn (c &TcpListener) accept_timeout() time.Duration {
	return c.accept_timeout
}

pub fn (mut c TcpListener) set_accept_timeout(t time.Duration) {
	c.accept_timeout = t
}

pub fn (mut c TcpListener) wait_for_accept() ? {
	return wait_for_read(c.sock.handle, c.accept_deadline, c.accept_timeout)
}

pub fn (mut c TcpListener) close() ? {
	$if trace_tcp ? {
		eprintln('    TcpListener.close | c.sock.handle: ${c.sock.handle:6}')
	}
	c.sock.close() ?
	return none
}

pub fn (c &TcpListener) address() ?Addr {
	return c.sock.address()
}

struct TcpSocket {
pub:
	handle int
}

fn new_tcp_socket() ?TcpSocket {
	sockfd := socket_error(C.socket(SocketFamily.inet, SocketType.tcp, 0)) ?
	mut s := TcpSocket{
		handle: sockfd
	}
	$if trace_tcp ? {
		eprintln('    new_tcp_socket | s.handle: ${s.handle:6}')
	}
	// s.set_option_bool(.reuse_addr, true)?
	s.set_option_int(.reuse_addr, 1) ?
	$if !net_blocking_sockets ? {
		$if windows {
			t := u32(1) // true
			socket_error(C.ioctlsocket(sockfd, fionbio, &t)) ?
		} $else {
			socket_error(C.fcntl(sockfd, C.F_SETFL, C.fcntl(sockfd, C.F_GETFL) | C.O_NONBLOCK)) ?
		}
	}
	return s
}

fn tcp_socket_from_handle(sockfd int) ?TcpSocket {
	mut s := TcpSocket{
		handle: sockfd
	}
	$if trace_tcp ? {
		eprintln('    tcp_socket_from_handle | s.handle: ${s.handle:6}')
	}
	// s.set_option_bool(.reuse_addr, true)?
	s.set_option_int(.reuse_addr, 1) ?
	$if !net_blocking_sockets ? {
		$if windows {
			t := u32(1) // true
			socket_error(C.ioctlsocket(sockfd, fionbio, &t)) ?
		} $else {
			socket_error(C.fcntl(sockfd, C.F_SETFL, C.fcntl(sockfd, C.F_GETFL) | C.O_NONBLOCK)) ?
		}
	}
	return s
}

pub fn (mut s TcpSocket) set_option_bool(opt SocketOption, value bool) ? {
	// TODO reenable when this `in` operation works again
	// if opt !in opts_can_set {
	// 	return err_option_not_settable
	// }
	// if opt !in opts_bool {
	// 	return err_option_wrong_type
	// }
	x := int(value)
	socket_error(C.setsockopt(s.handle, C.SOL_SOCKET, int(opt), &x, sizeof(int))) ?
	return none
}

pub fn (mut s TcpSocket) set_option_int(opt SocketOption, value int) ? {
	socket_error(C.setsockopt(s.handle, C.SOL_SOCKET, int(opt), &value, sizeof(int))) ?
	return none
}

fn (mut s TcpSocket) close() ? {
	$if trace_tcp ? {
		eprintln('    TcpSocket.close | s.handle: ${s.handle:6}')
	}
	return shutdown(s.handle)
}

fn (mut s TcpSocket) @select(test Select, timeout time.Duration) ?bool {
	return @select(s.handle, test, timeout)
}

const (
	connect_timeout = 5 * time.second
)

fn (mut s TcpSocket) connect(a string) ? {
	$if trace_tcp ? {
		eprintln('    TcpSocket.connect | s.handle: ${s.handle:6} | a: $a')
	}
	addr := resolve_addr(a, .inet, .tcp) ?
	res := C.connect(s.handle, &addr.addr, addr.len)
	if res == 0 {
		return none
	}
	_ := error_code()
	write_result := s.@select(.write, net.connect_timeout) ?
	if write_result {
		// succeeded
		return none
	}
	except_result := s.@select(.except, net.connect_timeout) ?
	if except_result {
		return err_connect_failed
	}
	// otherwise we timed out
	return err_connect_timed_out
}

// address gets the address of a socket
pub fn (s &TcpSocket) address() ?Addr {
	mut addr := C.sockaddr_in{}
	size := sizeof(C.sockaddr_in)
	// cast to the correct type
	sockaddr := unsafe { &C.sockaddr(&addr) }
	C.getsockname(s.handle, sockaddr, &size)
	return new_addr(sockaddr)
}
