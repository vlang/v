module net

import os
import time

// unix sockets should not experience much delay
pub const (
	unix_default_read_timeout  = 100 * time.millisecond
	unix_default_write_timeout = 100 * time.millisecond
)

[heap]
pub struct UnixConn {
pub mut:
	sock UnixSocket
mut:
	handle         int
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
	is_blocking    bool
}

pub fn dial_unix(socket_type SocketType, socket_path string) !&UnixConn {
	addrs := resolve_addrs(socket_path, .unix, socket_type) or {
		return error('${err.msg()}; could not resolve path ${socket_path}')
	}
	// should only be 1 address
	addr := addrs[0]

	mut s := new_unix_socket(socket_type, socket_path) or {
		return error('${err.msg()}; could not create new unix socket')
	}
	s.connect(addr)!

	return &UnixConn{
		sock: s
		handle: s.handle
		read_timeout: net.unix_default_read_timeout
		write_timeout: net.unix_default_write_timeout
	}
}

pub fn (mut c UnixConn) close() ! {
	$if trace_unix ? {
		eprintln('    UnixConn.close | c.sock.handle: ${c.sock.handle:6}')
	}
	c.sock.close()!
}

pub fn (c UnixConn) read_ptr(buf_ptr &u8, len int) !int {
	mut res := $if is_coroutine ? {
		wrap_read_result(C.photon_recv(c.sock.handle, voidptr(buf_ptr), len, 0, c.read_timeout))!
	} $else {
		wrap_read_result(C.recv(c.sock.handle, voidptr(buf_ptr), len, 0))!
	}
	$if trace_unix ? {
		eprintln('<<< UnixConn.read_ptr  | c.sock.handle: ${c.sock.handle} | buf_ptr: ${ptr_str(buf_ptr)} len: ${len} | res: ${res}')
	}
	if res > 0 {
		$if trace_unix_data_read ? {
			eprintln(
				'<<< UnixConn.read_ptr  | 1 data.len: ${res:6} | hex: ${unsafe { buf_ptr.vbytes(res) }.hex()} | data: ' +
				unsafe { buf_ptr.vstring_with_len(res) })
		}
		return res
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_read()!
		res = $if is_coroutine ? {
			wrap_read_result(C.photon_recv(c.sock.handle, voidptr(buf_ptr), len, 0, c.read_timeout))!
		} $else {
			wrap_read_result(C.recv(c.sock.handle, voidptr(buf_ptr), len, 0))!
		}
		$if trace_unix ? {
			eprintln('<<< UnixConn.read_ptr  | c.sock.handle: ${c.sock.handle} | buf_ptr: ${ptr_str(buf_ptr)} len: ${len} | res: ${res}')
		}
		$if trace_unix_data_read ? {
			if res > 0 {
				eprintln(
					'<<< UnixConn.read_ptr  | 2 data.len: ${res:6} | hex: ${unsafe { buf_ptr.vbytes(res) }.hex()} | data: ' +
					unsafe { buf_ptr.vstring_with_len(res) })
			}
		}
		return socket_error(res)
	} else {
		wrap_error(code)!
	}
	return error('none')
}

pub fn (c UnixConn) read(mut buf []u8) !int {
	return c.read_ptr(buf.data, buf.len)!
}

pub fn (mut c UnixConn) read_deadline() !time.Time {
	if c.read_deadline.unix == 0 {
		return c.read_deadline
	}
	return error('none')
}

// write_ptr blocks and attempts to write all data
pub fn (mut c UnixConn) write_ptr(b &u8, len int) !int {
	$if trace_unix_sock_handle ? {
		eprintln('>>> UnixConn.write_ptr | c: ${ptr_str(c)} | c.sock.handle: ${c.sock.handle} | b: ${ptr_str(b)} | len: ${len}')
	}
	$if trace_unix ? {
		eprintln(
			'>>> UnixConn.write_ptr | c.sock.handle: ${c.sock.handle} | b: ${ptr_str(b)} len: ${len} |\n' +
			unsafe { b.vstring_with_len(len) })
	}
	$if trace_unix_data_write ? {
		eprintln(
			'>>> UnixConn.write_ptr | data.len: ${len:6} | hex: ${unsafe { b.vbytes(len) }.hex()} | data: ' +
			unsafe { b.vstring_with_len(len) })
	}
	unsafe {
		mut ptr_base := &u8(b)
		mut total_sent := 0
		for total_sent < len {
			ptr := ptr_base + total_sent
			remaining := len - total_sent
			mut sent := $if is_coroutine ? {
				C.photon_send(c.sock.handle, ptr, remaining, msg_nosignal, c.write_timeout)
			} $else {
				C.send(c.sock.handle, ptr, remaining, msg_nosignal)
			}
			$if trace_unix_data_write ? {
				eprintln('>>> UnixConn.write_ptr | data chunk, total_sent: ${total_sent:6}, remaining: ${remaining:6}, ptr: ${voidptr(ptr):x} => sent: ${sent:6}')
			}
			if sent < 0 {
				code := error_code()
				if code == int(error_ewouldblock) {
					c.wait_for_write()!
					continue
				} else {
					wrap_error(code)!
				}
			}
			total_sent += sent
		}
		return total_sent
	}
}

// write blocks and attempts to write all data
pub fn (mut c UnixConn) write(bytes []u8) !int {
	return c.write_ptr(bytes.data, bytes.len)
}

// write_string blocks and attempts to write all data
pub fn (mut c UnixConn) write_string(s string) !int {
	return c.write_ptr(s.str, s.len)
}

pub fn (mut c UnixConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

pub fn (mut c UnixConn) write_deadline() !time.Time {
	if c.write_deadline.unix == 0 {
		return c.write_deadline
	}
	return error('none')
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
pub fn (c UnixConn) wait_for_read() ! {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

[inline]
pub fn (mut c UnixConn) wait_for_write() ! {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}

pub fn (mut c UnixConn) set_sock() ! {
	c.sock = unix_socket_from_handle(c.handle)!
	$if trace_unix ? {
		eprintln('    UnixListener.accept | << new_sock.handle: ${c.handle:6}')
	}
}

pub fn (c &UnixConn) addr() !Addr {
	return c.sock.address()
}

pub fn (c &UnixConn) str() string {
	s := c.sock.str().replace('\n', ' ').replace('  ', ' ')
	return 'UnixConn{ write_deadline: ${c.write_deadline}, read_deadline: ${c.read_deadline}, read_timeout: ${c.read_timeout}, write_timeout: ${c.write_timeout}, sock: ${s} }'
}

pub struct UnixListener {
pub mut:
	sock UnixSocket
mut:
	accept_timeout  time.Duration
	accept_deadline time.Time
}

pub fn listen_unix(socket_type SocketType, socket_path string, options ListenOptions) !&UnixListener {
	$if windows {
		if socket_type != .tcp {
			return error('Windows only supports `.tcp` unix sockets (SOCK_STREAM)')
		}
	}

	mut s := new_unix_socket(socket_type, socket_path) or {
		return error('${err.msg()}; could not create new unix socket')
	}

	addrs := resolve_addrs(socket_path, .unix, socket_type) or {
		return error('${err.msg()}; could not resolve path ${socket_path}')
	}
	// TODO add logic here
	addr := addrs[0]

	// cast to the correct type
	alen := addr.len()

	// try to unlink/remove an existing filesystem object at `socket_path`. Ignore errors,
	// because it's ok if the path doesn't exists and if it exists, but can't be unlinked
	// then `bind` will generate an error
	$if windows {
		os.rm(socket_path) or {}
	} $else {
		C.unlink(&char(socket_path.str))
	}
	socket_error_message(C.bind(s.handle, voidptr(&addr), alen), 'binding to ${socket_path} failed')!
	socket_error_message(C.listen(s.handle, options.backlog), 'listening on ${socket_path} with maximum backlog pending queue of ${options.backlog}, failed')!
	return &UnixListener{
		sock: s
		accept_deadline: no_deadline
		accept_timeout: infinite_timeout
	}
}

pub fn (mut l UnixListener) accept() !&UnixConn {
	mut res := l.accept_only()!
	res.set_sock()!
	return res
}

// accept_only accepts a unix connection, see comments of TcpListener.accept_only
pub fn (mut l UnixListener) accept_only() !&UnixConn {
	$if trace_unix ? {
		eprintln('    UnixListener.accept | l.sock.handle: ${l.sock.handle:6}')
	}

	mut new_handle := $if is_coroutine ? {
		C.photon_accept(l.sock.handle, 0, 0, net.unix_default_read_timeout)
	} $else {
		C.accept(l.sock.handle, 0, 0)
	}
	if new_handle <= 0 {
		l.wait_for_accept()!
		new_handle = $if is_coroutine ? {
			C.photon_accept(l.sock.handle, 0, 0, net.unix_default_read_timeout)
		} $else {
			C.accept(l.sock.handle, 0, 0)
		}
		if new_handle == -1 || new_handle == 0 {
			return error('accept failed')
		}
	}

	return &UnixConn{
		handle: new_handle
		read_timeout: net.unix_default_read_timeout
		write_timeout: net.unix_default_write_timeout
	}
}

pub fn (l &UnixListener) accept_deadline() !time.Time {
	if l.accept_deadline.unix != 0 {
		return l.accept_deadline
	}
	return error('invalid deadline')
}

pub fn (mut l UnixListener) set_accept_deadline(deadline time.Time) {
	l.accept_deadline = deadline
}

pub fn (l &UnixListener) accept_timeout() time.Duration {
	return l.accept_timeout
}

pub fn (mut l UnixListener) set_accept_timeout(t time.Duration) {
	l.accept_timeout = t
}

pub fn (mut l UnixListener) wait_for_accept() ! {
	return wait_for_read(l.sock.handle, l.accept_deadline, l.accept_timeout)
}

// close closes the unix sockets and attempts to unlink the file
pub fn (mut l UnixListener) close() ! {
	l.sock.close()!
	l.unlink()!
}

// unlink removes the unix socket from the file system
pub fn (mut l UnixListener) unlink() ! {
	$if windows {
		os.rm(l.sock.socket_path)!
	} $else {
		socket_error_message(C.unlink(&char(l.sock.socket_path.str)), 'could not unlink ${l.sock.socket_path}')!
	}
}

// unlink_on_signal removes the socket from the filesystem when signal `signum` occurs
pub fn (mut l UnixListener) unlink_on_signal(signum os.Signal) ! {
	os.signal_opt(.int, fn [mut l] (sign os.Signal) {
		$if trace_unix ? {
			eprintln('	UnixListener.unlink_on_signal received signal ${sign}; unlinking unix socket ${l.sock.socket_path}')
		}
		l.unlink() or {}
		exit(1)
	})!
}

pub fn (mut l UnixListener) addr() !Addr {
	return l.sock.address()!
}

struct UnixSocket {
	Socket
	socket_path string
}

fn new_unix_socket(socket_type SocketType, socket_path string) !UnixSocket {
	handle := $if is_coroutine ? {
		socket_error(C.photon_socket(.unix, socket_type, 0))!
	} $else {
		socket_error(C.socket(.unix, socket_type, 0))!
	}
	mut s := UnixSocket{
		handle: handle
		socket_path: socket_path
	}

	$if trace_unix ? {
		eprintln('    new_unix_socket | s.handle: ${s.handle:6}')
	}

	$if !net_blocking_sockets ? {
		$if windows {
			t := u32(1) // true
			socket_error(C.ioctlsocket(handle, fionbio, &t))!
		} $else {
			socket_error(C.fcntl(handle, C.F_SETFL, C.fcntl(handle, C.F_GETFL) | C.O_NONBLOCK))!
		}
	}
	return s
}

fn (mut s UnixSocket) close() ! {
	// shutdown might be redundant for unix sockets, but it doesn't hurt to call it
	shutdown(s.handle)
	return close(s.handle)
}

fn (mut s UnixSocket) @select(test Select, timeout time.Duration) !bool {
	return @select(s.handle, test, timeout)
}

fn (mut s UnixSocket) set_option(level int, opt int, value int) ! {
	socket_error(C.setsockopt(s.handle, level, opt, &value, sizeof(int)))!
}

pub fn (mut s UnixSocket) set_option_bool(opt SocketOption, value bool) ! {
	if opt !in opts_can_set {
		return err_option_not_settable
	}
	if opt !in opts_bool {
		return err_option_wrong_type
	}
	x := int(value)
	s.set_option(C.SOL_SOCKET, int(opt), &x)!
}

pub fn (mut s UnixSocket) set_option_int(opt SocketOption, value int) ! {
	s.set_option(C.SOL_SOCKET, int(opt), value)!
}

fn (mut s UnixSocket) connect(a Addr) ! {
	$if !net_blocking_sockets ? {
		res := $if is_coroutine ? {
			C.photon_connect(s.handle, voidptr(&a), a.len(), net.unix_default_read_timeout)
		} $else {
			C.connect(s.handle, voidptr(&a), a.len())
		}
		if res == 0 {
			return
		}
		ecode := error_code()

		// no need to check for einprogress on nix
		// On windows we expect res == -1 && error_code() == ewouldblock
		if is_windows && ecode == int(error_ewouldblock) {
			// The socket is nonblocking and the connection cannot be completed
			// immediately. Wait till the socket is ready to write
			write_result := s.@select(.write, connect_timeout)!
			err := 0
			len := sizeof(err)
			// determine whether connect() completed successfully (SO_ERROR is zero)
			xyz := C.getsockopt(s.handle, C.SOL_SOCKET, C.SO_ERROR, &err, &len)
			if xyz == 0 && err == 0 {
				return
			}
			if write_result {
				if xyz == 0 {
					wrap_error(err)!
					return
				}
				return
			}
			return err_timed_out
		}

		wrap_error(ecode)!
		return
	} $else {
		x := $if is_coroutine ? {
			C.photon_connect(s.handle, voidptr(&a), a.len(), tcp_default_read_timeout)
		} $else {
			C.connect(s.handle, voidptr(&a), a.len())
		}
		socket_error(x)!
	}
}

pub fn unix_socket_from_handle(sockfd int) !UnixSocket {
	mut s := UnixSocket{
		handle: sockfd
	}

	$if trace_unix ? {
		eprintln('    unix_socket_from_handle | s.handle: ${s.handle:6}')
	}
	// s.set_default_options()!

	$if !net_blocking_sockets ? {
		$if windows {
			t := u32(1) // true
			socket_error(C.ioctlsocket(sockfd, fionbio, &t))!
		} $else {
			socket_error(C.fcntl(sockfd, C.F_SETFL, C.fcntl(sockfd, C.F_GETFL) | C.O_NONBLOCK))!
		}
	}
	return s
}
