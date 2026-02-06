module net

import time

const raw_default_read_timeout = time.second / 10
const raw_default_write_timeout = time.second / 10

struct RawSocket {
	Socket
	l Addr
mut:
	has_r    bool
	r        Addr
	protocol Protocol
}

// RawConn represents a raw socket connection for low-level network access.
// Raw sockets allow sending and receiving packets at the IP layer,
// bypassing the TCP/UDP transport layer.
pub struct RawConn {
pub mut:
	sock RawSocket
mut:
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
}

// RawSocketConfig configures the creation of a raw socket.
@[params]
pub struct RawSocketConfig {
pub:
	family   AddrFamily = .ip
	protocol Protocol   = .icmp
}

// new_raw_socket creates a new raw socket with the given configuration.
// Raw sockets typically require elevated privileges (root/administrator).
pub fn new_raw_socket(config RawSocketConfig) !&RawConn {
	sockfd := socket_error(C.socket(config.family, SocketType.raw, int(config.protocol)))!
	mut s := &RawSocket{
		handle:   sockfd
		protocol: config.protocol
		l:        Addr{
			addr: AddrData{
				Ip6: Ip6{}
			}
		}
		r:        Addr{
			addr: AddrData{
				Ip6: Ip6{}
			}
		}
	}

	$if net_nonblocking_sockets ? {
		set_blocking(sockfd, false)!
	}

	return &RawConn{
		sock:          s
		read_timeout:  raw_default_read_timeout
		write_timeout: raw_default_write_timeout
	}
}

// write_ptr writes data from `b` of length `len` to the connected remote address.
pub fn (mut c RawConn) write_ptr(b &u8, len int) !int {
	remote := c.sock.remote() or { return error('no remote address set for raw socket') }
	return c.write_to_ptr(remote, b, len)
}

// write writes `buf` to the connected remote address.
pub fn (mut c RawConn) write(buf []u8) !int {
	return c.write_ptr(buf.data, buf.len)
}

// write_string writes string `s` to the connected remote address.
pub fn (mut c RawConn) write_string(s string) !int {
	return c.write_ptr(s.str, s.len)
}

// write_to_ptr writes data from `b` of length `len` to the specified `addr`.
pub fn (mut c RawConn) write_to_ptr(addr Addr, b &u8, len int) !int {
	res := C.sendto(c.sock.handle, b, len, 0, voidptr(&addr), addr.len())
	if res >= 0 {
		return res
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_write()!
		socket_error(C.sendto(c.sock.handle, b, len, 0, voidptr(&addr), addr.len()))!
	} else {
		wrap_error(code)!
	}
	return error('write failed')
}

// write_to writes `buf` to the specified `addr`.
pub fn (mut c RawConn) write_to(addr Addr, buf []u8) !int {
	return c.write_to_ptr(addr, buf.data, buf.len)
}

// write_to_string writes string `s` to the specified `addr`.
pub fn (mut c RawConn) write_to_string(addr Addr, s string) !int {
	return c.write_to_ptr(addr, s.str, s.len)
}

// read_ptr reads from the socket into `buf_ptr` up to `len` bytes,
// returning the number of bytes read and the source `Addr`.
pub fn (c &RawConn) read_ptr(buf_ptr &u8, len int) !(int, Addr) {
	mut addr := Addr{
		addr: AddrData{
			Ip6: Ip6{}
		}
	}
	addr_len := sizeof(Addr)
	mut res := wrap_read_result(C.recvfrom(c.sock.handle, voidptr(buf_ptr), len, 0, voidptr(&addr),
		&addr_len))!
	if res > 0 {
		return res, addr
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_read()!
		res = wrap_read_result(C.recvfrom(c.sock.handle, voidptr(buf_ptr), len, 0, voidptr(&addr),
			&addr_len))!
		res2 := socket_error(res)!
		return res2, addr
	} else {
		wrap_error(code)!
	}
	return error('read failed')
}

// read reads from the socket into `buf`, returning the number of bytes read and the source `Addr`.
pub fn (mut c RawConn) read(mut buf []u8) !(int, Addr) {
	return c.read_ptr(buf.data, buf.len)!
}

// read_deadline returns the current read deadline.
pub fn (c &RawConn) read_deadline() !time.Time {
	if c.read_deadline.unix() == 0 {
		return c.read_deadline
	}
	return error('none')
}

// set_read_deadline sets the read deadline.
pub fn (mut c RawConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

// write_deadline returns the current write deadline.
pub fn (c &RawConn) write_deadline() !time.Time {
	if c.write_deadline.unix() == 0 {
		return c.write_deadline
	}
	return error('none')
}

// set_write_deadline sets the write deadline.
pub fn (mut c RawConn) set_write_deadline(deadline time.Time) {
	c.write_deadline = deadline
}

// read_timeout returns the current read timeout duration.
pub fn (c &RawConn) read_timeout() time.Duration {
	return c.read_timeout
}

// set_read_timeout sets the read timeout duration.
pub fn (mut c RawConn) set_read_timeout(t time.Duration) {
	c.read_timeout = t
}

// write_timeout returns the current write timeout duration.
pub fn (c &RawConn) write_timeout() time.Duration {
	return c.write_timeout
}

// set_write_timeout sets the write timeout duration.
pub fn (mut c RawConn) set_write_timeout(t time.Duration) {
	c.write_timeout = t
}

// wait_for_read waits for a read operation to be available.
@[inline]
pub fn (c &RawConn) wait_for_read() ! {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

// wait_for_write waits for a write operation to be available.
@[inline]
pub fn (mut c RawConn) wait_for_write() ! {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}

// str returns a string representation of the RawConn.
pub fn (c &RawConn) str() string {
	return 'RawConn'
}

// close closes the raw socket connection.
pub fn (mut c RawConn) close() ! {
	return c.sock.close()
}

// set_remote sets the remote address for write operations.
pub fn (mut c RawConn) set_remote(addr Addr) {
	c.sock.has_r = true
	c.sock.r = addr
}

// protocol returns the protocol used by this socket.
pub fn (c &RawConn) protocol() Protocol {
	return c.sock.protocol
}

// set_option_bool sets a boolean socket option.
pub fn (mut s RawSocket) set_option_bool(opt SocketOption, value bool) ! {
	x := int(value)
	socket_error(C.setsockopt(s.handle, C.SOL_SOCKET, int(opt), &x, sizeof(int)))!
}

// set_option_int sets an integer socket option.
pub fn (mut s RawSocket) set_option_int(opt SocketOption, value int) ! {
	socket_error(C.setsockopt(s.handle, C.SOL_SOCKET, int(opt), &value, sizeof(int)))!
}

// set_ip_header_included enables or disables the IP_HDRINCL option.
// When enabled, the user must provide the complete IP header.
pub fn (mut s RawSocket) set_ip_header_included(on bool) ! {
	x := int(on)
	socket_error(C.setsockopt(s.handle, C.IPPROTO_IP, C.IP_HDRINCL, &x, sizeof(int)))!
}

// close shuts down and closes the socket.
pub fn (mut s RawSocket) close() ! {
	shutdown(s.handle)
	return close(s.handle)
}

// select waits for no more than `timeout` for the IO operation, defined by `test`, to be available.
pub fn (mut s RawSocket) select(test Select, timeout time.Duration) !bool {
	return select(s.handle, test, timeout)
}

// remote returns the remote `Addr` of the socket or `none` if not set.
pub fn (s &RawSocket) remote() ?Addr {
	if s.has_r {
		return s.r
	}
	return none
}

// addr returns the local address of the socket.
pub fn (c &RawConn) addr() !Addr {
	return c.sock.address()
}
