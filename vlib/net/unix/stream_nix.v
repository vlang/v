module unix

import time
import os
import net

const (
	unix_default_read_timeout  = 30 * time.second
	unix_default_write_timeout = 30 * time.second
	connect_timeout            = 5 * time.second
	msg_nosignal               = 0x4000
)

struct StreamSocket {
pub:
	handle int
mut:
	path string
}

struct StreamConn {
pub mut:
	sock StreamSocket
mut:
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
}

struct StreamListener {
pub mut:
	sock StreamSocket
mut:
	accept_timeout  time.Duration
	accept_deadline time.Time
}

fn error_code() int {
	return C.errno
}

fn new_stream_socket() ?StreamSocket {
	sockfd := net.socket_error(C.socket(net.AddrFamily.unix, net.SocketType.tcp, 0)) ?
	mut s := StreamSocket{
		handle: sockfd
	}
	return s
}

fn (mut s StreamSocket) close() ? {
	return shutdown(s.handle)
}

fn (mut s StreamSocket) @select(test Select, timeout time.Duration) ?bool {
	return @select(s.handle, test, timeout)
}

fn (mut s StreamSocket) connect(a string) ? {
	if a.len >= max_sun_path {
		return error('Socket path too long! Max length: ${max_sun_path - 1} chars.')
	}
	mut addr := C.sockaddr_un{}
	unsafe { C.memset(&addr, 0, sizeof(C.sockaddr_un)) }
	addr.sun_family = byte(C.AF_UNIX)
	unsafe { C.strncpy(&addr.sun_path[0], &char(a.str), max_sun_path) }
	size := C.SUN_LEN(&addr)
	res := C.connect(s.handle, voidptr(&addr), size)
	// if res != 1 {
	// return none
	//}
	if res == 0 {
		return
	}
	_ := error_code()
	write_result := s.@select(.write, unix.connect_timeout) ?
	if write_result {
		// succeeded
		return
	}
	except_result := s.@select(.except, unix.connect_timeout) ?
	if except_result {
		return net.err_connect_failed
	}
	// otherwise we timed out
	return net.err_connect_timed_out
}

pub fn listen_stream(sock string) ?&StreamListener {
	if sock.len >= max_sun_path {
		return error('Socket path too long! Max length: ${max_sun_path - 1} chars.')
	}
	mut s := new_stream_socket() ?
	s.path = sock
	mut addr := C.sockaddr_un{}
	unsafe { C.memset(&addr, 0, sizeof(C.sockaddr_un)) }
	addr.sun_family = byte(C.AF_UNIX)
	unsafe { C.strncpy(&addr.sun_path[0], &char(sock.str), max_sun_path) }
	size := C.SUN_LEN(&addr)
	if os.exists(sock) {
		os.rm(sock) ?
	}
	net.socket_error(C.bind(s.handle, voidptr(&addr), size)) ?
	os.chmod(sock, 0o777) ?
	net.socket_error(C.listen(s.handle, 128)) ?
	return &StreamListener{
		sock: s
	}
}

pub fn connect_stream(path string) ?&StreamConn {
	mut s := new_stream_socket() ?
	s.connect(path) ?
	return &StreamConn{
		sock: s
		read_timeout: unix.unix_default_read_timeout
		write_timeout: unix.unix_default_write_timeout
	}
}

pub fn (mut l StreamListener) accept() ?&StreamConn {
	mut new_handle := C.accept(l.sock.handle, 0, 0)
	if new_handle <= 0 {
		l.wait_for_accept() ?
		new_handle = C.accept(l.sock.handle, 0, 0)
		if new_handle == -1 || new_handle == 0 {
			return error('accept failed')
		}
	}
	new_sock := StreamSocket{
		handle: new_handle
	}
	return &StreamConn{
		sock: new_sock
		read_timeout: unix.unix_default_read_timeout
		write_timeout: unix.unix_default_write_timeout
	}
}

pub fn (c &StreamListener) accept_deadline() ?time.Time {
	if c.accept_deadline.unix != 0 {
		return c.accept_deadline
	}
	return error('no deadline')
}

pub fn (mut c StreamListener) set_accept_deadline(deadline time.Time) {
	c.accept_deadline = deadline
}

pub fn (c &StreamListener) accept_timeout() time.Duration {
	return c.accept_timeout
}

pub fn (mut c StreamListener) set_accept_timeout(t time.Duration) {
	c.accept_timeout = t
}

pub fn (mut c StreamListener) wait_for_accept() ? {
	return wait_for_read(c.sock.handle, c.accept_deadline, c.accept_timeout)
}

pub fn (mut c StreamListener) close() ? {
	os.rm(c.sock.path) ?
	c.sock.close() ?
}

pub fn (mut c StreamConn) close() ? {
	c.sock.close() ?
}

// write_ptr blocks and attempts to write all data
pub fn (mut c StreamConn) write_ptr(b &byte, len int) ?int {
	$if trace_unix ? {
		eprintln(
			'>>> StreamConn.write_ptr | c.sock.handle: $c.sock.handle | b: ${ptr_str(b)} len: $len |\n' +
			unsafe { b.vstring_with_len(len) })
	}
	unsafe {
		mut ptr_base := &byte(b)
		mut total_sent := 0
		for total_sent < len {
			ptr := ptr_base + total_sent
			remaining := len - total_sent
			mut sent := C.send(c.sock.handle, ptr, remaining, unix.msg_nosignal)
			if sent < 0 {
				code := error_code()
				if code == int(error_ewouldblock) {
					c.wait_for_write() ?
					continue
				} else {
					net.wrap_error(code) ?
				}
			}
			total_sent += sent
		}
		return total_sent
	}
}

// write blocks and attempts to write all data
pub fn (mut c StreamConn) write(bytes []byte) ?int {
	return c.write_ptr(bytes.data, bytes.len)
}

// write_string blocks and attempts to write all data
pub fn (mut c StreamConn) write_string(s string) ?int {
	return c.write_ptr(s.str, s.len)
}

pub fn (mut c StreamConn) read_ptr(buf_ptr &byte, len int) ?int {
	mut res := wrap_read_result(C.recv(c.sock.handle, voidptr(buf_ptr), len, 0)) ?
	$if trace_unix ? {
		eprintln('<<< StreamConn.read_ptr  | c.sock.handle: $c.sock.handle | buf_ptr: ${ptr_str(buf_ptr)} len: $len | res: $res')
	}
	if res > 0 {
		return res
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_read() ?
		res = wrap_read_result(C.recv(c.sock.handle, voidptr(buf_ptr), len, 0)) ?
		$if trace_unix ? {
			eprintln('<<< StreamConn.read_ptr  | c.sock.handle: $c.sock.handle | buf_ptr: ${ptr_str(buf_ptr)} len: $len | res: $res')
		}
		return net.socket_error(res)
	} else {
		net.wrap_error(code) ?
	}
	return net.socket_error(code)
}

pub fn (mut c StreamConn) read(mut buf []byte) ?int {
	return c.read_ptr(buf.data, buf.len)
}

pub fn (mut c StreamConn) read_deadline() ?time.Time {
	if c.read_deadline.unix == 0 {
		return c.read_deadline
	}
	return none
}

pub fn (mut c StreamConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

pub fn (mut c StreamConn) write_deadline() ?time.Time {
	if c.write_deadline.unix == 0 {
		return c.write_deadline
	}
	return none
}

pub fn (mut c StreamConn) set_write_deadline(deadline time.Time) {
	c.write_deadline = deadline
}

pub fn (c &StreamConn) read_timeout() time.Duration {
	return c.read_timeout
}

pub fn (mut c StreamConn) set_read_timeout(t time.Duration) {
	c.read_timeout = t
}

pub fn (c &StreamConn) write_timeout() time.Duration {
	return c.write_timeout
}

pub fn (mut c StreamConn) set_write_timeout(t time.Duration) {
	c.write_timeout = t
}

[inline]
pub fn (mut c StreamConn) wait_for_read() ? {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

[inline]
pub fn (mut c StreamConn) wait_for_write() ? {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}

pub fn (c StreamConn) str() string {
	s := c.sock.str().replace('\n', ' ').replace('  ', ' ')
	return 'StreamConn{ write_deadline: $c.write_deadline, read_deadline: $c.read_deadline, read_timeout: $c.read_timeout, write_timeout: $c.write_timeout, sock: $s }'
}
