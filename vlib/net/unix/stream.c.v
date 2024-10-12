module unix

import time
import os
import net

const unix_default_read_timeout = 30 * time.second
const unix_default_write_timeout = 30 * time.second
const connect_timeout = 5 * time.second
const msg_nosignal = 0x4000

// UnixDialer is a concrete instance of the Dialer interface,
// for creating unix socket connections.
pub struct UnixDialer {}

// dial will try to create a new abstract connection to the given address.
// It will return an error, if that is not possible.
pub fn (u UnixDialer) dial(address string) !net.Connection {
	return connect_stream(address)!
}

@[heap]
pub struct StreamConn {
pub mut:
	sock StreamSocket
mut:
	handle         int
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
	is_blocking    bool
}

// connect_stream returns a SOCK_STREAM connection for an unix domain socket on `socket_path`
pub fn connect_stream(socket_path string) !&StreamConn {
	if socket_path.len >= max_sun_path {
		return error('Socket path too long! Max length: ${max_sun_path - 1} chars.')
	}
	mut s := new_stream_socket(socket_path) or {
		return error('${err.msg()}; could not create new unix socket')
	}

	s.connect(socket_path)!

	return &StreamConn{
		sock:          s
		read_timeout:  unix_default_read_timeout
		write_timeout: unix_default_write_timeout
	}
}

// addr returns the local address of the stream
pub fn (c StreamConn) addr() !net.Addr {
	return error('not implemented for unix connections')
}

// peer_addr returns the address of the remote peer of the stream
pub fn (c StreamConn) peer_addr() !net.Addr {
	return error('not implemented for unix connections')
}

// close closes the connection
pub fn (mut c StreamConn) close() ! {
	$if trace_unix ? {
		eprintln('    StreamConn.close | c.sock.handle: ${c.sock.handle:6}')
	}
	c.sock.close()!
}

// write_ptr blocks and attempts to write all data
pub fn (mut c StreamConn) write_ptr(b &u8, len int) !int {
	$if trace_unix_sock_handle ? {
		eprintln('>>> StreamConn.write_ptr | c: ${ptr_str(c)} | c.sock.handle: ${c.sock.handle} | b: ${ptr_str(b)} | len: ${len}')
	}
	$if trace_unix ? {
		eprintln(
			'>>> StreamConn.write_ptr | c.sock.handle: ${c.sock.handle} | b: ${ptr_str(b)} len: ${len} |\n' +
			unsafe { b.vstring_with_len(len) })
	}
	$if trace_unix_data_write ? {
		eprintln(
			'>>> StreamConn.write_ptr | data.len: ${len:6} | hex: ${unsafe { b.vbytes(len) }.hex()} | data: ' +
			unsafe { b.vstring_with_len(len) })
	}
	unsafe {
		mut ptr_base := &u8(b)
		mut total_sent := 0
		for total_sent < len {
			ptr := ptr_base + total_sent
			remaining := len - total_sent
			mut sent := $if is_coroutine ? {
				C.photon_send(c.sock.handle, ptr, remaining, net.msg_nosignal, c.write_timeout)
			} $else {
				C.send(c.sock.handle, ptr, remaining, net.msg_nosignal)
			}
			$if trace_unix_data_write ? {
				eprintln('>>> UnixConn.write_ptr | data chunk, total_sent: ${total_sent:6}, remaining: ${remaining:6}, ptr: ${voidptr(ptr):x} => sent: ${sent:6}')
			}
			if sent < 0 {
				code := net.error_code()
				if code == int(net.error_ewouldblock) {
					c.wait_for_write()!
					continue
				} else {
					net.wrap_error(code)!
				}
			}
			total_sent += sent
		}
		return total_sent
	}
}

// write blocks and attempts to write all data
pub fn (mut c StreamConn) write(bytes []u8) !int {
	return c.write_ptr(bytes.data, bytes.len)
}

// write_string blocks and attempts to write all data
pub fn (mut c StreamConn) write_string(s string) !int {
	return c.write_ptr(s.str, s.len)
}

// read_ptr attempts to write all data
pub fn (mut c StreamConn) read_ptr(buf_ptr &u8, len int) !int {
	mut res := $if is_coroutine ? {
		wrap_read_result(C.photon_recv(c.sock.handle, voidptr(buf_ptr), len, 0, c.read_timeout))!
	} $else {
		wrap_read_result(C.recv(c.sock.handle, voidptr(buf_ptr), len, 0))!
	}
	$if trace_unix ? {
		eprintln('<<< StreamConn.read_ptr  | c.sock.handle: ${c.sock.handle} | buf_ptr: ${ptr_str(buf_ptr)} len: ${len} | res: ${res}')
	}
	if res > 0 {
		$if trace_unix_data_read ? {
			eprintln(
				'<<< StreamConn.read_ptr  | 1 data.len: ${res:6} | hex: ${unsafe { buf_ptr.vbytes(res) }.hex()} | data: ' +
				unsafe { buf_ptr.vstring_with_len(res) })
		}
		return res
	}
	code := net.error_code()
	if code == int(net.error_ewouldblock) {
		c.wait_for_read()!
		res = $if is_coroutine ? {
			wrap_read_result(C.photon_recv(c.sock.handle, voidptr(buf_ptr), len, 0, c.read_timeout))!
		} $else {
			wrap_read_result(C.recv(c.sock.handle, voidptr(buf_ptr), len, 0))!
		}
		$if trace_unix ? {
			eprintln('<<< StreamConn.read_ptr  | c.sock.handle: ${c.sock.handle} | buf_ptr: ${ptr_str(buf_ptr)} len: ${len} | res: ${res}')
		}
		$if trace_unix_data_read ? {
			if res > 0 {
				eprintln(
					'<<< StreamConn.read_ptr  | 2 data.len: ${res:6} | hex: ${unsafe { buf_ptr.vbytes(res) }.hex()} | data: ' +
					unsafe { buf_ptr.vstring_with_len(res) })
			}
		}
		return net.socket_error(res)
	} else {
		net.wrap_error(code)!
	}
	return error('none')
}

// read data into `buf`
pub fn (mut c StreamConn) read(mut buf []u8) !int {
	return c.read_ptr(buf.data, buf.len)
}

// read_deadline returns the read deadline
pub fn (mut c StreamConn) read_deadline() !time.Time {
	if c.read_deadline.unix() == 0 {
		return c.read_deadline
	}
	return error('none')
}

// set_read_deadlien sets the read deadline
pub fn (mut c StreamConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

// write_deadline returns the write deadline
pub fn (mut c StreamConn) write_deadline() !time.Time {
	if c.write_deadline.unix() == 0 {
		return c.write_deadline
	}
	return error('none')
}

// set_write_deadline sets the write deadline
pub fn (mut c StreamConn) set_write_deadline(deadline time.Time) {
	c.write_deadline = deadline
}

// read_timeout returns the read timeout
pub fn (c &StreamConn) read_timeout() time.Duration {
	return c.read_timeout
}

// set_read_timeout sets the read timeout
pub fn (mut c StreamConn) set_read_timeout(t time.Duration) {
	c.read_timeout = t
}

// write_timeout returns the write timeout
pub fn (c &StreamConn) write_timeout() time.Duration {
	return c.write_timeout
}

// set_write_timeout sets the write timeout
pub fn (mut c StreamConn) set_write_timeout(t time.Duration) {
	c.write_timeout = t
}

// wait_for_read blocks until the socket is ready to read
@[inline]
pub fn (mut c StreamConn) wait_for_read() ! {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

// wait_for_read blocks until the socket is ready to write
@[inline]
pub fn (mut c StreamConn) wait_for_write() ! {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}

// str returns a string representation of connection `c`
pub fn (c StreamConn) str() string {
	s := c.sock.str().replace('\n', ' ').replace('  ', ' ')
	return 'StreamConn{ write_deadline: ${c.write_deadline}, read_deadline: ${c.read_deadline}, read_timeout: ${c.read_timeout}, write_timeout: ${c.write_timeout}, sock: ${s} }'
}

pub struct StreamListener {
pub mut:
	sock StreamSocket
mut:
	accept_timeout  time.Duration
	accept_deadline time.Time
}

@[params]
pub struct ListenOptions {
pub:
	backlog int = 128
}

// listen_stream creates an unix domain socket at `socket_path`
pub fn listen_stream(socket_path string, options ListenOptions) !&StreamListener {
	if socket_path.len >= max_sun_path {
		return error('Socket path too long! Max length: ${max_sun_path - 1} chars.')
	}
	mut s := new_stream_socket(socket_path) or {
		return error('${err.msg()}; could not create new unix stream socket')
	}

	addrs := net.resolve_addrs(socket_path, .unix, .tcp) or {
		return error('${err.msg()}; could not resolve path ${socket_path}')
	}
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

	net.socket_error_message(C.bind(s.handle, voidptr(&addr), alen), 'binding to ${socket_path} failed')!
	net.socket_error_message(C.listen(s.handle, options.backlog), 'listening on ${socket_path} with maximum backlog pending queue of ${options.backlog}, failed')!
	return &StreamListener{
		sock:            s
		accept_deadline: no_deadline
		accept_timeout:  infinite_timeout
	}
}

// accept accepts blocks until a new connection occurs
pub fn (mut l StreamListener) accept() !&StreamConn {
	$if trace_unix ? {
		eprintln('    StreamListener.accept | l.sock.handle: ${l.sock.handle:6}')
	}

	mut new_handle := $if is_coroutine ? {
		C.photon_accept(l.sock.handle, 0, 0, unix_default_read_timeout)
	} $else {
		C.accept(l.sock.handle, 0, 0)
	}
	if new_handle <= 0 {
		l.wait_for_accept()!
		new_handle = $if is_coroutine ? {
			C.photon_accept(l.sock.handle, 0, 0, unix_default_read_timeout)
		} $else {
			C.accept(l.sock.handle, 0, 0)
		}
		if new_handle == -1 || new_handle == 0 {
			return error('accept failed')
		}
	}

	mut c := &StreamConn{
		handle:        new_handle
		read_timeout:  unix_default_read_timeout
		write_timeout: unix_default_write_timeout
	}
	c.sock = stream_socket_from_handle(c.handle)!
	return c
}

// accept_deadline returns the deadline until a new client is accepted
pub fn (l &StreamListener) accept_deadline() !time.Time {
	if l.accept_deadline.unix() != 0 {
		return l.accept_deadline
	}
	return error('no deadline')
}

// set_accept_deadline sets the deadlinme until a new client is accepted
pub fn (mut l StreamListener) set_accept_deadline(deadline time.Time) {
	l.accept_deadline = deadline
}

// accept_timeout returns the timeout until a new client is accepted
pub fn (l &StreamListener) accept_timeout() time.Duration {
	return l.accept_timeout
}

// set_accept_timeout sets the timeout until a new client is accepted
pub fn (mut l StreamListener) set_accept_timeout(t time.Duration) {
	l.accept_timeout = t
}

// wait_for_accept blocks until a client can be accepted
pub fn (mut l StreamListener) wait_for_accept() ! {
	return wait_for_read(l.sock.handle, l.accept_deadline, l.accept_timeout)
}

// close closes the listening socket and unlinks/removes the socket file
pub fn (mut l StreamListener) close() ! {
	l.sock.close()!
	l.unlink()!
}

// unlink removes the unix socket from the file system
pub fn (mut l StreamListener) unlink() ! {
	$if windows {
		os.rm(l.sock.socket_path)!
	} $else {
		net.socket_error_message(C.unlink(&char(l.sock.socket_path.str)), 'could not unlink ${l.sock.socket_path}')!
	}
}

// unlink_on_signal removes the socket from the filesystem when signal `signum` occurs
pub fn (mut l StreamListener) unlink_on_signal(signum os.Signal) ! {
	os.signal_opt(.int, fn [mut l] (sign os.Signal) {
		$if trace_unix ? {
			eprintln('	StreamListener.unlink_on_signal received signal ${sign}; unlinking unix socket ${l.sock.socket_path}')
		}
		l.unlink() or {}
		exit(1)
	})!
}

// addr returns the `net.Addr` version of the listening socket's path
pub fn (mut l StreamListener) addr() !net.Addr {
	return l.sock.address()!
}

pub struct StreamSocket {
	net.Socket
mut:
	socket_path string
}

fn new_stream_socket(socket_path string) !StreamSocket {
	handle := $if is_coroutine ? {
		net.socket_error(C.photon_socket(.unix, .tcp, 0))!
	} $else {
		net.socket_error(C.socket(.unix, .tcp, 0))!
	}
	mut s := StreamSocket{
		handle:      handle
		socket_path: socket_path
	}

	$if trace_unix ? {
		eprintln('    new_unix_socket | s.handle: ${s.handle:6}')
	}

	$if net_nonblocking_sockets ? {
		net.set_blocking(handle, false)!
	}
	return s
}

fn (mut s StreamSocket) close() ! {
	// shutdown might be redundant for unix domain sockets, but it doesn't hurt to call it
	shutdown(s.handle)
	return close(s.handle)
}

fn (mut s StreamSocket) select(test Select, timeout time.Duration) !bool {
	return select(s.handle, test, timeout)
}

// set_option sets an option on the socket
fn (mut s StreamSocket) set_option(level int, opt int, value int) ! {
	net.socket_error(C.setsockopt(s.handle, level, opt, &value, sizeof(int)))!
}

// set_option_bool sets a boolean option on the socket
pub fn (mut s StreamSocket) set_option_bool(opt net.SocketOption, value bool) ! {
	if opt !in net.opts_can_set {
		return net.err_option_not_settable
	}
	if opt !in net.opts_bool {
		return net.err_option_wrong_type
	}
	x := int(value)
	s.set_option(C.SOL_SOCKET, int(opt), x)!
}

// set_option_bool sets an int option on the socket
pub fn (mut s StreamSocket) set_option_int(opt net.SocketOption, value int) ! {
	s.set_option(C.SOL_SOCKET, int(opt), value)!
}

fn (mut s StreamSocket) connect(socket_path string) ! {
	if socket_path.len >= max_sun_path {
		return error('Socket path too long! Max length: ${max_sun_path - 1} chars.')
	}

	addrs := net.resolve_addrs(socket_path, .unix, .tcp) or {
		return error('${err.msg()}; could not resolve path ${socket_path}')
	}
	addr := addrs[0]
	// cast to the correct type
	alen := addr.len()

	$if net_nonblocking_sockets ? {
		res := $if is_coroutine ? {
			C.photon_connect(s.handle, voidptr(&addr), alen, unix_default_read_timeout)
		} $else {
			C.connect(s.handle, voidptr(&addr), alen)
		}
		if res == 0 {
			return
		}
		ecode := net.error_code()

		// no need to check for einprogress on nix
		// On windows we expect res == -1 && net.error_code() == ewouldblock
		$if windows {
			if ecode == int(net.error_ewouldblock) {
				// The socket is nonblocking and the connection cannot be completed
				// immediately. Wait till the socket is ready to write
				write_result := s.select(.write, connect_timeout)!
				err := 0
				len := sizeof(err)
				// determine whether connect() completed successfully (SO_ERROR is zero)
				xyz := C.getsockopt(s.handle, C.SOL_SOCKET, C.SO_ERROR, &err, &len)
				if xyz == 0 && err == 0 {
					return
				}
				if write_result {
					if xyz == 0 {
						net.wrap_error(err)!
						return
					}
					return
				}
				return net.err_timed_out
			}
		}
		net.wrap_error(ecode)!
		return
	} $else {
		x := $if is_coroutine ? {
			C.photon_connect(s.handle, voidptr(&addr), alen, unix_default_read_timeout)
		} $else {
			C.connect(s.handle, voidptr(&addr), alen)
		}
		net.socket_error(x)!
	}
}

// stream_socket_from_handle returns a `StreamSocket` instance from the raw file descriptor `sockfd`
pub fn stream_socket_from_handle(sockfd int) !&StreamSocket {
	mut s := &StreamSocket{
		handle: sockfd
	}

	$if trace_unix ? {
		eprintln('    stream_socket_from_handle | s.handle: ${s.handle:6}')
	}

	$if net_nonblocking_sockets ? {
		net.set_blocking(sockfd, false)!
	}
	return s
}
