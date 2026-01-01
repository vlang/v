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

pub struct RawConn {
pub mut:
	sock RawSocket
mut:
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
}

@[params]
pub struct RawSocketConfig {
pub:
	family   AddrFamily = .ip
	protocol Protocol   = .icmp
}

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

pub fn (mut c RawConn) write_ptr(b &u8, len int) !int {
	remote := c.sock.remote() or { return error('no remote address set for raw socket') }
	return c.write_to_ptr(remote, b, len)
}

pub fn (mut c RawConn) write(buf []u8) !int {
	return c.write_ptr(buf.data, buf.len)
}

pub fn (mut c RawConn) write_string(s string) !int {
	return c.write_ptr(s.str, s.len)
}

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

pub fn (mut c RawConn) write_to(addr Addr, buf []u8) !int {
	return c.write_to_ptr(addr, buf.data, buf.len)
}

pub fn (mut c RawConn) write_to_string(addr Addr, s string) !int {
	return c.write_to_ptr(addr, s.str, s.len)
}

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

pub fn (mut c RawConn) read(mut buf []u8) !(int, Addr) {
	return c.read_ptr(buf.data, buf.len)!
}

pub fn (c &RawConn) read_deadline() !time.Time {
	if c.read_deadline.unix() == 0 {
		return c.read_deadline
	}
	return error('none')
}

pub fn (mut c RawConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

pub fn (c &RawConn) write_deadline() !time.Time {
	if c.write_deadline.unix() == 0 {
		return c.write_deadline
	}
	return error('none')
}

pub fn (mut c RawConn) set_write_deadline(deadline time.Time) {
	c.write_deadline = deadline
}

pub fn (c &RawConn) read_timeout() time.Duration {
	return c.read_timeout
}

pub fn (mut c RawConn) set_read_timeout(t time.Duration) {
	c.read_timeout = t
}

pub fn (c &RawConn) write_timeout() time.Duration {
	return c.write_timeout
}

pub fn (mut c RawConn) set_write_timeout(t time.Duration) {
	c.write_timeout = t
}

@[inline]
pub fn (c &RawConn) wait_for_read() ! {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

@[inline]
pub fn (mut c RawConn) wait_for_write() ! {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}

pub fn (c &RawConn) str() string {
	return 'RawConn'
}

pub fn (mut c RawConn) close() ! {
	return c.sock.close()
}

pub fn (mut c RawConn) set_remote(addr Addr) {
	c.sock.has_r = true
	c.sock.r = addr
}

pub fn (c &RawConn) protocol() Protocol {
	return c.sock.protocol
}

pub fn (mut s RawSocket) set_option_bool(opt SocketOption, value bool) ! {
	x := int(value)
	socket_error(C.setsockopt(s.handle, C.SOL_SOCKET, int(opt), &x, sizeof(int)))!
}

pub fn (mut s RawSocket) set_option_int(opt SocketOption, value int) ! {
	socket_error(C.setsockopt(s.handle, C.SOL_SOCKET, int(opt), &value, sizeof(int)))!
}

pub fn (mut s RawSocket) set_ip_header_included(on bool) ! {
	x := int(on)
	socket_error(C.setsockopt(s.handle, C.IPPROTO_IP, C.IP_HDRINCL, &x, sizeof(int)))!
}

pub fn (mut s RawSocket) close() ! {
	shutdown(s.handle)
	return close(s.handle)
}

pub fn (mut s RawSocket) select(test Select, timeout time.Duration) !bool {
	return select(s.handle, test, timeout)
}

pub fn (s &RawSocket) remote() ?Addr {
	if s.has_r {
		return s.r
	}
	return none
}

pub fn (c &RawConn) addr() !Addr {
	return c.sock.address()
}
