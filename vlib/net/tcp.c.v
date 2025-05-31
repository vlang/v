module net

import time
import strings

pub const tcp_default_read_timeout = 30 * time.second
pub const tcp_default_write_timeout = 30 * time.second

// TCPDialer is a concrete instance of the Dialer interface,
// for creating tcp connections.
pub struct TCPDialer {}

// dial will try to create a new abstract connection to the given address.
// It will return an error, if that is not possible.
pub fn (t TCPDialer) dial(address string) !Connection {
	return dial_tcp(address)!
}

// default_tcp_dialer will give you an instance of Dialer, that is suitable for making new tcp connections.
pub fn default_tcp_dialer() Dialer {
	return &TCPDialer{}
}

@[heap]
pub struct TcpConn {
pub mut:
	sock           TcpSocket
	handle         int
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
	is_blocking    bool = true
}

// dial_tcp will try to create a new TcpConn to the given address.
pub fn dial_tcp(oaddress string) !&TcpConn {
	mut address := oaddress
	$if windows {
		// resolving 0.0.0.0 to localhost, works on linux and macos, but not on windows, so try to emulate it:
		if address.starts_with(':::') {
			address = address.replace_once(':::', 'localhost:')
		}
		if address.starts_with('0.0.0.0:') {
			address = address.replace_once('0.0.0.0:', 'localhost:')
		}
	}
	addrs := resolve_addrs_fuzzy(address, .tcp) or {
		return error('${err.msg()}; could not resolve address ${address} in dial_tcp')
	}

	// Keep track of dialing errors that take place
	mut errs := []IError{}

	// Very simple dialer
	for addr in addrs {
		mut s := new_tcp_socket(addr.family()) or {
			return error('${err.msg()}; could not create new tcp socket in dial_tcp')
		}
		s.connect(addr) or {
			errs << err
			// Connection failed
			s.close() or { continue }
			continue
		}

		mut conn := &TcpConn{
			sock:          s
			read_timeout:  tcp_default_read_timeout
			write_timeout: tcp_default_write_timeout
		}
		// The blocking / non-blocking mode is determined before the connection is established.
		$if net_nonblocking_sockets ? {
			conn.is_blocking = false
		}
		return conn
	}

	// Once we've failed now try and explain why we failed to connect
	// to any of these addresses
	mut err_builder := strings.new_builder(1024)
	err_builder.write_string('dial_tcp failed for address ${address}\n')
	err_builder.write_string('tried addrs:\n')
	for i := 0; i < errs.len; i++ {
		addr := addrs[i]
		why := errs[i]
		err_builder.write_string('\t${addr}: ${why}\n')
	}

	// failed
	return error(err_builder.str())
}

// dial_tcp_with_bind will bind the given local address `laddr` and dial.
pub fn dial_tcp_with_bind(saddr string, laddr string) !&TcpConn {
	addrs := resolve_addrs_fuzzy(saddr, .tcp) or {
		return error('${err.msg()}; could not resolve address ${saddr} in dial_tcp_with_bind')
	}

	// Very simple dialer
	for addr in addrs {
		mut s := new_tcp_socket(addr.family()) or {
			return error('${err.msg()}; could not create new tcp socket in dial_tcp_with_bind')
		}
		s.bind(laddr) or {
			s.close() or { continue }
			continue
		}
		s.connect(addr) or {
			// Connection failed
			s.close() or { continue }
			continue
		}

		mut conn := &TcpConn{
			sock:          s
			read_timeout:  tcp_default_read_timeout
			write_timeout: tcp_default_write_timeout
		}
		// The blocking / non-blocking mode is determined before the connection is established.
		$if net_nonblocking_sockets ? {
			conn.is_blocking = false
		}
		return conn
	}
	// failed
	return error('dial_tcp_with_bind failed for address ${saddr}')
}

// close closes the tcp connection
pub fn (mut c TcpConn) close() ! {
	$if trace_tcp ? {
		eprintln('    TcpConn.close | c.sock.handle: ${c.sock.handle:6}')
	}
	c.sock.close()!
}

// read_ptr reads data from the tcp connection to the given buffer. It reads at most `len` bytes.
// It returns the number of actually read bytes, which can vary between 0 to `len`.
pub fn (c TcpConn) read_ptr(buf_ptr &u8, len int) !int {
	mut should_ewouldblock := false
	mut res := $if is_coroutine ? {
		C.photon_recv(c.sock.handle, voidptr(buf_ptr), len, 0, c.read_timeout)
	} $else {
		// The new socket returned by accept() behaves differently in blocking mode and needs special treatment.
		mut has_data := true
		if c.is_blocking {
			if ok := select(c.sock.handle, .read, 1) {
				has_data = ok
			} else {
				false
			}
		}
		if has_data {
			C.recv(c.sock.handle, voidptr(buf_ptr), len, msg_dontwait)
		} else {
			should_ewouldblock = true
			-1
		}
	}
	$if trace_tcp ? {
		eprintln('<<< TcpConn.read_ptr  | c.sock.handle: ${c.sock.handle} | buf_ptr: ${ptr_str(buf_ptr)} len: ${len} | res: ${res}')
	}
	if res > 0 {
		$if trace_tcp_data_read ? {
			eprintln(
				'<<< TcpConn.read_ptr  | 1 data.len: ${res:6} | hex: ${unsafe { buf_ptr.vbytes(res) }.hex()} | data: ' +
				unsafe { buf_ptr.vstring_with_len(res) })
		}
		return res
	}
	code := if should_ewouldblock { int(error_ewouldblock) } else { error_code() }
	if code in [int(error_ewouldblock), int(error_eagain), C.EINTR] {
		c.wait_for_read()!
		res = $if is_coroutine ? {
			C.photon_recv(c.sock.handle, voidptr(buf_ptr), len, 0, c.read_timeout)
		} $else {
			C.recv(c.sock.handle, voidptr(buf_ptr), len, msg_dontwait)
		}
		$if trace_tcp ? {
			eprintln('<<< TcpConn.read_ptr  | c.sock.handle: ${c.sock.handle} | buf_ptr: ${ptr_str(buf_ptr)} len: ${len} | res: ${res}')
		}
		$if trace_tcp_data_read ? {
			if res > 0 {
				eprintln(
					'<<< TcpConn.read_ptr  | 2 data.len: ${res:6} | hex: ${unsafe { buf_ptr.vbytes(res) }.hex()} | data: ' +
					unsafe { buf_ptr.vstring_with_len(res) })
			}
		}
		return socket_error(res)
	} else {
		wrap_error(code)!
	}
	return error('none')
}

// read reads data from the tcp connection into the mutable buffer `buf`.
// The number of bytes read is limited to the length of the buffer `buf.len`.
// The returned value is the number of read bytes (between 0 and `buf.len`).
pub fn (c TcpConn) read(mut buf []u8) !int {
	return c.read_ptr(buf.data, buf.len)!
}

pub fn (mut c TcpConn) read_deadline() !time.Time {
	if c.read_deadline.unix() == 0 {
		return c.read_deadline
	}
	return error('none')
}

// write_ptr blocks and attempts to write all data
pub fn (mut c TcpConn) write_ptr(b &u8, len int) !int {
	$if trace_tcp_sock_handle ? {
		eprintln('>>> TcpConn.write_ptr | c: ${ptr_str(c)} | c.sock.handle: ${c.sock.handle} | b: ${ptr_str(b)} | len: ${len}')
	}
	$if trace_tcp ? {
		eprintln(
			'>>> TcpConn.write_ptr | c.sock.handle: ${c.sock.handle} | b: ${ptr_str(b)} len: ${len} |\n' +
			unsafe { b.vstring_with_len(len) })
	}
	$if trace_tcp_data_write ? {
		eprintln(
			'>>> TcpConn.write_ptr | data.len: ${len:6} | hex: ${unsafe { b.vbytes(len) }.hex()} | data: ' +
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
			$if trace_tcp_data_write ? {
				eprintln('>>> TcpConn.write_ptr | data chunk, total_sent: ${total_sent:6}, remaining: ${remaining:6}, ptr: ${voidptr(ptr):x} => sent: ${sent:6}')
			}
			if sent < 0 {
				code := error_code()
				$if trace_tcp_send_failures ? {
					eprintln('>>> TcpConn.write_ptr | send_failure, data.len: ${len:6}, total_sent: ${total_sent:6}, remaining: ${remaining:6}, ptr: ${voidptr(ptr):x}, c.write_timeout: ${c.write_timeout:3} => sent: ${sent:6}, error code: ${code:3}')
				}
				if code in [int(error_ewouldblock), int(error_eagain), C.EINTR] {
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
pub fn (mut c TcpConn) write(bytes []u8) !int {
	return c.write_ptr(bytes.data, bytes.len)
}

// write_string blocks and attempts to write all data
pub fn (mut c TcpConn) write_string(s string) !int {
	return c.write_ptr(s.str, s.len)
}

pub fn (mut c TcpConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

pub fn (mut c TcpConn) write_deadline() !time.Time {
	if c.write_deadline.unix() == 0 {
		return c.write_deadline
	}
	return error('none')
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

@[inline]
pub fn (c TcpConn) wait_for_read() ! {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

@[inline]
pub fn (mut c TcpConn) wait_for_write() ! {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}

// set_sock initialises the c.sock field. It should be called after `.accept_only()!`.
// Note: just use `.accept()!`. In most cases it is simpler, and calls `.set_sock()!` for you.
pub fn (mut c TcpConn) set_sock() ! {
	c.sock = tcp_socket_from_handle(c.handle)!
	$if trace_tcp ? {
		eprintln('    TcpListener.accept | << new_sock.handle: ${c.handle:6}')
	}
}

// peer_addr retrieves the ip address and port number used by the peer
pub fn (c &TcpConn) peer_addr() !Addr {
	return peer_addr_from_socket_handle(c.sock.handle)
}

// peer_ip retrieves the ip address used by the peer, and returns it as a string
pub fn (c &TcpConn) peer_ip() !string {
	address := c.peer_addr()!.str()
	if address.contains(']:') {
		// ipv6 addresses similar to this: '[::1]:46098'
		ip := address.all_before(']:').all_after('[')
		return ip
	}
	// ipv4 addresses similar to '127.0.0.1:7346'
	ip := address.all_before(':')
	return ip
}

pub fn (c &TcpConn) addr() !Addr {
	return c.sock.address()
}

pub fn (c TcpConn) str() string {
	s := c.sock.str().replace('\n', ' ').replace('  ', ' ')
	return 'TcpConn{ write_deadline: ${c.write_deadline}, read_deadline: ${c.read_deadline}, read_timeout: ${c.read_timeout}, write_timeout: ${c.write_timeout}, sock: ${s} }'
}

pub struct TcpListener {
pub mut:
	sock            TcpSocket
	accept_timeout  time.Duration
	accept_deadline time.Time
	is_blocking     bool = true
}

@[params]
pub struct ListenOptions {
pub:
	dualstack bool = true
	backlog   int  = 128
}

pub fn listen_tcp(family AddrFamily, saddr string, options ListenOptions) !&TcpListener {
	if family !in [.ip, .ip6] {
		return error('listen_tcp only supports ip and ip6')
	}
	mut s := new_tcp_socket(family) or { return error('${err.msg()}; could not create new socket') }
	s.set_dualstack(options.dualstack) or {}

	addrs := resolve_addrs(saddr, family, .tcp) or {
		return error('${err.msg()}; could not resolve address ${saddr}')
	}
	// TODO(logic to pick here)
	addr := addrs[0]

	// cast to the correct type
	alen := addr.len()
	socket_error_message(C.bind(s.handle, voidptr(&addr), alen), 'binding to ${saddr} failed')!
	mut res := C.listen(s.handle, options.backlog)
	if res == 0 {
		mut listener := &TcpListener{
			sock:            s
			accept_deadline: no_deadline
			accept_timeout:  infinite_timeout
		}
		// The blocking / non-blocking mode is determined before the connection is established.
		$if net_nonblocking_sockets ? {
			listener.is_blocking = false
		}
		return listener
	}

	$if !net_nonblocking_sockets ? {
		socket_error_message(res, 'listening on ${saddr} with maximum backlog pending queue of ${options.backlog}, failed')!
		return &TcpListener(unsafe { nil }) // for compiler passed
	} $else {
		// non-blocking sockets may also not succeed immediately when they listen() and need to check the status and take action accordingly.
		for {
			code := error_code()
			if code in [int(error_einprogress), int(error_ewouldblock), int(error_eagain), C.EINTR] {
				select(s.handle, .read, connect_timeout)!
				res = C.listen(s.handle, options.backlog)
				if res == 0 {
					break
				}
			} else {
				socket_error_message(res, 'listening on ${saddr} with maximum backlog pending queue of ${options.backlog}, failed')!
				break // for compiler passed
			}
		}
		mut listener := &TcpListener{
			sock:            s
			accept_deadline: no_deadline
			accept_timeout:  infinite_timeout
		}
		// The blocking / non-blocking mode is determined before the connection is established.
		$if net_nonblocking_sockets ? {
			listener.is_blocking = false
		}
		return listener
	}
}

// accept a tcp connection from an external source to the listener `l`.
pub fn (mut l TcpListener) accept() !&TcpConn {
	mut res := l.accept_only()!
	res.set_sock()!
	return res
}

// accept_only accepts a tcp connection from an external source to the listener `l`.
// Unlike `accept`, `accept_only` *will not call* `.set_sock()!` on the result,
// and is thus faster.
//
// Note: you *need* to call `.set_sock()!` manually, before using the
// connection after calling `.accept_only()!`, but that does not have to happen
// in the same thread that called `.accept_only()!`.
// The intention of this API, is to have a more efficient way to accept
// connections, that are later processed by a thread pool, while the main
// thread remains active, so that it can accept other connections.
// See also vlib/vweb/vweb.v .
//
// If you do not need that, just call `.accept()!` instead, which will call
// `.set_sock()!` for you.
pub fn (mut l TcpListener) accept_only() !&TcpConn {
	$if trace_tcp ? {
		eprintln('    TcpListener.accept | l.sock.handle: ${l.sock.handle:6}')
	}

	// The blocking mode `accept()` does not support a timeout option, so `select` is used instead.
	$if !is_coroutine ? {
		if l.is_blocking {
			l.wait_for_accept()!
		}
	}

	mut new_handle := $if is_coroutine ? {
		C.photon_accept(l.sock.handle, 0, 0, tcp_default_read_timeout)
	} $else {
		C.accept(l.sock.handle, 0, 0)
	}

	if !l.is_blocking && new_handle <= 0 {
		code := error_code()
		if code in [int(error_einprogress), int(error_ewouldblock), int(error_eagain), C.EINTR] {
			l.wait_for_accept()!
			new_handle = $if is_coroutine ? {
				C.photon_accept(l.sock.handle, 0, 0, tcp_default_read_timeout)
			} $else {
				C.accept(l.sock.handle, 0, 0)
			}
		}
	}
	if new_handle <= 0 {
		return error('accept failed')
	}

	return &TcpConn{
		handle:        new_handle
		read_timeout:  tcp_default_read_timeout
		write_timeout: tcp_default_write_timeout
		is_blocking:   l.is_blocking
	}
}

pub fn (c &TcpListener) accept_deadline() !time.Time {
	if c.accept_deadline.unix() != 0 {
		return c.accept_deadline
	}
	return error('invalid deadline')
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

pub fn (mut c TcpListener) wait_for_accept() ! {
	return wait_for_read(c.sock.handle, c.accept_deadline, c.accept_timeout)
}

pub fn (mut c TcpListener) close() ! {
	c.sock.close()!
}

pub fn (c &TcpListener) addr() !Addr {
	return c.sock.address()
}

struct TcpSocket {
	Socket
}

// This is a workaround for issue https://github.com/vlang/v/issues/20858
// `noline` ensure that in `-prod` mode(CFLAG = `-O3 -flto`), gcc does not generate wrong instruction sequence
@[noinline]
pub fn new_tcp_socket(family AddrFamily) !TcpSocket {
	handle := $if is_coroutine ? {
		socket_error(C.photon_socket(family, SocketType.tcp, 0))!
	} $else {
		socket_error(C.socket(family, SocketType.tcp, 0))!
	}
	mut s := TcpSocket{
		handle: handle
	}
	$if trace_tcp ? {
		eprintln('    new_tcp_socket | s.handle: ${s.handle:6}')
	}

	// TODO(emily):
	// we shouldn't be using ioctlsocket in the 21st century
	// use the non-blocking socket option instead please :)

	// Some options need to be set before the connection is established, otherwise they will not work.
	s.set_default_options()!

	// Set the desired "blocking/non-blocking" mode before the connection is established,
	// and do not change it once the connection is successful.
	$if net_nonblocking_sockets ? {
		set_blocking(handle, false)!
	}
	return s
}

fn tcp_socket_from_handle(sockfd int) !TcpSocket {
	mut s := TcpSocket{
		handle: sockfd
	}
	$if trace_tcp ? {
		eprintln('    tcp_socket_from_handle | s.handle: ${s.handle:6}')
	}

	s.set_dualstack(true) or {
		// Not ipv6, we dont care
	}
	s.set_default_options()!

	return s
}

// tcp_socket_from_handle_raw is similar to tcp_socket_from_handle, but it does not modify any socket options
pub fn tcp_socket_from_handle_raw(sockfd int) TcpSocket {
	mut s := TcpSocket{
		handle: sockfd
	}
	$if trace_tcp ? {
		eprintln('    tcp_socket_from_handle_raw | s.handle: ${s.handle:6}')
	}
	return s
}

fn (mut s TcpSocket) set_option(level int, opt int, value int) ! {
	socket_error(C.setsockopt(s.handle, level, opt, &value, sizeof(int)))!
}

pub fn (mut s TcpSocket) set_option_bool(opt SocketOption, value bool) ! {
	// TODO: reenable when this `in` operation works again
	// if opt !in opts_can_set {
	// 	return err_option_not_settable
	// }
	// if opt !in opts_bool {
	// 	return err_option_wrong_type
	// }
	x := int(value)
	s.set_option(C.SOL_SOCKET, int(opt), x)!
}

pub fn (mut s TcpSocket) set_option_int(opt SocketOption, value int) ! {
	s.set_option(C.SOL_SOCKET, int(opt), value)!
}

pub fn (mut s TcpSocket) set_dualstack(on bool) ! {
	x := int(!on)
	s.set_option(C.IPPROTO_IPV6, int(SocketOption.ipv6_only), x)!
}

fn (mut s TcpSocket) set_default_options() ! {
	s.set_option_int(.reuse_addr, 1)!

	// At the socket level to ignore the exception signal (usually SIGNPIPE).
	// In Linux, instead of using set_option(), specify the C.MSG_NOSIGNAL flag in c.send().
	// In Windows, there is no need to process this signal.
	$if macos {
		s.set_option(C.SOL_SOCKET, C.SO_NOSIGPIPE, 1)!
	}

	// Enable the NODELAY option by default.
	s.set_option(C.IPPROTO_TCP, C.TCP_NODELAY, 1)!
}

// bind a local rddress for TcpSocket
pub fn (mut s TcpSocket) bind(addr string) ! {
	addrs := resolve_addrs(addr, AddrFamily.ip, .tcp) or {
		return error('${err.msg()}; could not resolve address ${addr}')
	}

	// TODO(logic to pick here)
	a := addrs[0]

	// cast to the correct type
	alen := a.len()
	socket_error_message(C.bind(s.handle, voidptr(&a), alen), 'binding to ${addr} failed') or {
		return err
	}
}

fn (mut s TcpSocket) close() ! {
	shutdown(s.handle)
	return close(s.handle)
}

const connect_timeout = 5 * time.second

fn (mut s TcpSocket) connect(a Addr) ! {
	$if net_nonblocking_sockets ? {
		res := $if is_coroutine ? {
			C.photon_connect(s.handle, voidptr(&a), a.len(), tcp_default_read_timeout)
		} $else {
			C.connect(s.handle, voidptr(&a), a.len())
		}
		if res == 0 {
			return
		}
		ecode := error_code()
		// On nix non-blocking sockets we expect einprogress
		// On windows we expect res == -1 && error_code() == ewouldblock
		if (is_windows && ecode == int(error_ewouldblock)) || (!is_windows && res == -1
			&& ecode in [int(error_einprogress), int(error_eagain), C.EINTR]) {
			// The  socket  is  nonblocking and the connection cannot be completed
			// immediately.  (UNIX domain sockets failed with EAGAIN instead.)
			// It is possible to select(2) or poll(2) for completion by selecting
			// the socket for  writing.   After  select(2) indicates  writability,
			// use getsockopt(2) to read the SO_ERROR option at level SOL_SOCKET to
			// determine whether connect() completed successfully (SO_ERROR is zero) or
			// unsuccessfully (SO_ERROR is one of the usual error codes  listed  here,
			// ex‐ plaining the reason for the failure).
			write_result := select(s.handle, .write, connect_timeout)!
			err := 0
			len := sizeof(err)
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
