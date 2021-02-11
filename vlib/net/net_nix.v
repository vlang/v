module net

import time
import os

fn C.SUN_LEN(C.sockaddr_un) int

fn C.strncpy(charptr, charptr, int)

struct UnixSocket {
pub:
	handle int
mut:
	path string
}

struct UnixConn {
pub mut:
	sock UnixSocket
mut:
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
}

struct UnixListener {
pub mut:
	sock UnixSocket
mut:
	accept_timeout  time.Duration
	accept_deadline time.Time
}

fn new_unix_socket() ?UnixSocket {
	sockfd := socket_error(C.socket(SocketFamily.unix, SocketType.tcp, 0)) ?
	mut s := UnixSocket{
		handle: sockfd
	}
	return s
}

fn (mut s UnixSocket) close() ? {
	os.rm(s.path) ?
	return shutdown(s.handle)
}

fn (mut s UnixSocket) @select(test Select, timeout time.Duration) ?bool {
	return @select(s.handle, test, timeout)
}

fn (mut s UnixSocket) connect(a string) ? {
	if a.len >= 108 {
		return error('Socket path too long! Max length: 107 chars.')
	}
	mut addr := C.sockaddr_un{}
	unsafe { C.memset(&addr, 0, sizeof(C.sockaddr_un)) }
	addr.sun_family = int(SocketFamily.unix)
	C.strncpy(addr.sun_path, a.str, 108)
	size := C.SUN_LEN(&addr)
	sockaddr := unsafe { &C.sockaddr(&addr) }
	res := C.connect(s.handle, sockaddr, size)
	// if res != 1 {
	// return none
	//}
	if res == 0 {
		return none
	}
	_ := error_code()
	write_result := s.@select(.write, connect_timeout) ?
	if write_result {
		// succeeded
		return none
	}
	except_result := s.@select(.except, connect_timeout) ?
	if except_result {
		return err_connect_failed
	}
	// otherwise we timed out
	return err_connect_timed_out
}

pub fn listen_unix(sock string) ?&UnixListener {
	if sock.len >= 108 {
		return error('Socket path too long! Max length: 107 chars.')
	}
	mut s := new_unix_socket() ?
	s.path = sock
	mut addr := C.sockaddr_un{}
	unsafe { C.memset(&addr, 0, sizeof(C.sockaddr_un)) }
	addr.sun_family = int(SocketFamily.unix)
	C.strncpy(addr.sun_path, sock.str, 108)
	size := C.SUN_LEN(&addr)
	sockaddr := unsafe { &C.sockaddr(&addr) }
	socket_error(C.bind(s.handle, sockaddr, size)) ?
	socket_error(C.listen(s.handle, 128)) ?
	return &UnixListener{
		sock: s
	}
}

pub fn connect_unix(path string) ?&UnixConn {
	mut s := new_unix_socket() ?
	s.connect(path) ?
	return &UnixConn{
		sock: s
		read_timeout: tcp_default_read_timeout
		write_timeout: tcp_default_write_timeout
	}
}

pub fn (mut l UnixListener) accept() ?&UnixConn {
	mut new_handle := C.accept(l.sock.handle, 0, 0)
	if new_handle <= 0 {
		l.wait_for_accept() ?
		new_handle = C.accept(l.sock.handle, 0, 0)
		if new_handle == -1 || new_handle == 0 {
			return none
		}
	}
	new_sock := UnixSocket{
		handle: new_handle
	}
	return &UnixConn{
		sock: new_sock
		read_timeout: tcp_default_read_timeout
		write_timeout: tcp_default_write_timeout
	}
}

pub fn (c &UnixListener) accept_deadline() ?time.Time {
	if c.accept_deadline.unix != 0 {
		return c.accept_deadline
	}
	return none
}

pub fn (mut c UnixListener) set_accept_deadline(deadline time.Time) {
	c.accept_deadline = deadline
}

pub fn (c &UnixListener) accept_timeout() time.Duration {
	return c.accept_timeout
}

pub fn (mut c UnixListener) set_accept_timeout(t time.Duration) {
	c.accept_timeout = t
}

pub fn (mut c UnixListener) wait_for_accept() ? {
	return wait_for_read(c.sock.handle, c.accept_deadline, c.accept_timeout)
}

pub fn (mut c UnixListener) close() ? {
	c.sock.close() ?
	return none
}

pub fn (mut c UnixConn) close() ? {
	c.sock.close() ?
	return none
}

// write_ptr blocks and attempts to write all data
pub fn (mut c UnixConn) write_ptr(b byteptr, len int) ? {
	$if trace_tcp ? {
		eprintln(
			'>>> UnixConn.write_ptr | c.sock.handle: $c.sock.handle | b: ${ptr_str(b)} len: $len |\n' +
			unsafe { b.vstring_with_len(len) })
	}
	unsafe {
		mut ptr_base := byteptr(b)
		mut total_sent := 0
		for total_sent < len {
			ptr := ptr_base + total_sent
			remaining := len - total_sent
			mut sent := C.send(c.sock.handle, ptr, remaining, msg_nosignal)
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
	}
	return none
}

// write blocks and attempts to write all data
pub fn (mut c UnixConn) write(bytes []byte) ? {
	return c.write_ptr(bytes.data, bytes.len)
}

// write_str blocks and attempts to write all data
pub fn (mut c UnixConn) write_str(s string) ? {
	return c.write_ptr(s.str, s.len)
}

pub fn (mut c UnixConn) read_ptr(buf_ptr byteptr, len int) ?int {
	mut res := wrap_read_result(C.recv(c.sock.handle, buf_ptr, len, 0)) ?
	$if trace_tcp ? {
		eprintln('<<< UnixConn.read_ptr  | c.sock.handle: $c.sock.handle | buf_ptr: ${ptr_str(buf_ptr)} len: $len | res: $res')
	}
	if res > 0 {
		return res
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_read() ?
		res = wrap_read_result(C.recv(c.sock.handle, buf_ptr, len, 0)) ?
		$if trace_tcp ? {
			eprintln('<<< UnixConn.read_ptr  | c.sock.handle: $c.sock.handle | buf_ptr: ${ptr_str(buf_ptr)} len: $len | res: $res')
		}
		return socket_error(res)
	} else {
		wrap_error(code) ?
	}
	return none
}

pub fn (mut c UnixConn) read(mut buf []byte) ?int {
	return c.read_ptr(buf.data, buf.len)
}

pub fn (mut c UnixConn) read_deadline() ?time.Time {
	if c.read_deadline.unix == 0 {
		return c.read_deadline
	}
	return none
}

pub fn (mut c UnixConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

pub fn (mut c UnixConn) write_deadline() ?time.Time {
	if c.write_deadline.unix == 0 {
		return c.write_deadline
	}
	return none
}

pub fn (mut c UnixConn) set_write_deadline(deadline time.Time) {
	c.write_deadline = deadline
}

pub fn (c &UnixConn) read_timeout() time.Duration {
	return c.read_timeout
}

pub fn (mut c UnixConn) set_read_timeout(t time.Duration) {
	c.read_timeout = t
}

pub fn (c &UnixConn) write_timeout() time.Duration {
	return c.write_timeout
}

pub fn (mut c UnixConn) set_write_timeout(t time.Duration) {
	c.write_timeout = t
}

[inline]
pub fn (mut c UnixConn) wait_for_read() ? {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

[inline]
pub fn (mut c UnixConn) wait_for_write() ? {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}

pub fn (c UnixConn) str() string {
	s := c.sock.str().replace('\n', ' ').replace('  ', ' ')
	return 'UnixConn{ write_deadline: $c.write_deadline, read_deadline: $c.read_deadline, read_timeout: $c.read_timeout, write_timeout: $c.write_timeout, sock: $s }'
}
