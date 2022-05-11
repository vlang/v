module net

import time

const (
	udp_default_read_timeout  = time.second / 10
	udp_default_write_timeout = time.second / 10
)

struct UdpSocket {
	Socket
	l Addr
	// TODO(emily): replace with option again
	// when i figure out how to coerce it properly
mut:
	has_r bool
	r     Addr
}

pub struct UdpConn {
pub mut:
	sock UdpSocket
mut:
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
}

pub fn dial_udp(raddr string) ?&UdpConn {
	addrs := resolve_addrs_fuzzy(raddr, .udp)?

	for addr in addrs {
		// create a local socket for this
		// bind to any port (or file) (we dont care in this
		// case because we only care about the remote)
		if sock := new_udp_socket_for_remote(addr) {
			return &UdpConn{
				sock: sock
				read_timeout: net.udp_default_read_timeout
				write_timeout: net.udp_default_write_timeout
			}
		}
	}

	return none
}

// pub fn dial_udp(laddr string, raddr string) ?&UdpConn {
// 	local := resolve_addr(laddr, .inet, .udp)?

// 	sbase := new_udp_socket()?

// 	sock := UdpSocket{
// 		handle: sbase.handle
// 		l: local
// 		r: resolve_wrapper(raddr)
// 	}
// }

pub fn (mut c UdpConn) write_ptr(b &u8, len int) ?int {
	remote := c.sock.remote() or { return err_no_udp_remote }
	return c.write_to_ptr(remote, b, len)
}

pub fn (mut c UdpConn) write(buf []u8) ?int {
	return c.write_ptr(buf.data, buf.len)
}

pub fn (mut c UdpConn) write_string(s string) ?int {
	return c.write_ptr(s.str, s.len)
}

pub fn (mut c UdpConn) write_to_ptr(addr Addr, b &u8, len int) ?int {
	res := C.sendto(c.sock.handle, b, len, 0, voidptr(&addr), addr.len())
	if res >= 0 {
		return res
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_write()?
		socket_error(C.sendto(c.sock.handle, b, len, 0, voidptr(&addr), addr.len()))?
	} else {
		wrap_error(code)?
	}
	return none
}

// write_to blocks and writes the buf to the remote addr specified
pub fn (mut c UdpConn) write_to(addr Addr, buf []u8) ?int {
	return c.write_to_ptr(addr, buf.data, buf.len)
}

// write_to_string blocks and writes the buf to the remote addr specified
pub fn (mut c UdpConn) write_to_string(addr Addr, s string) ?int {
	return c.write_to_ptr(addr, s.str, s.len)
}

// read reads from the socket into buf up to buf.len returning the number of bytes read
pub fn (mut c UdpConn) read(mut buf []u8) ?(int, Addr) {
	mut addr := Addr{
		addr: AddrData{
			Ip6: Ip6{}
		}
	}
	len := sizeof(Addr)
	mut res := wrap_read_result(C.recvfrom(c.sock.handle, voidptr(buf.data), buf.len,
		0, voidptr(&addr), &len))?
	if res > 0 {
		return res, addr
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_read()?
		// same setup as in tcp
		res = wrap_read_result(C.recvfrom(c.sock.handle, voidptr(buf.data), buf.len, 0,
			voidptr(&addr), &len))?
		res2 := socket_error(res)?
		return res2, addr
	} else {
		wrap_error(code)?
	}
	return none
}

pub fn (c &UdpConn) read_deadline() ?time.Time {
	if c.read_deadline.unix == 0 {
		return c.read_deadline
	}
	return none
}

pub fn (mut c UdpConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

pub fn (c &UdpConn) write_deadline() ?time.Time {
	if c.write_deadline.unix == 0 {
		return c.write_deadline
	}
	return none
}

pub fn (mut c UdpConn) set_write_deadline(deadline time.Time) {
	c.write_deadline = deadline
}

pub fn (c &UdpConn) read_timeout() time.Duration {
	return c.read_timeout
}

pub fn (mut c UdpConn) set_read_timeout(t time.Duration) {
	c.read_timeout = t
}

pub fn (c &UdpConn) write_timeout() time.Duration {
	return c.write_timeout
}

pub fn (mut c UdpConn) set_write_timeout(t time.Duration) {
	c.write_timeout = t
}

[inline]
pub fn (mut c UdpConn) wait_for_read() ? {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

[inline]
pub fn (mut c UdpConn) wait_for_write() ? {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}

pub fn (c &UdpConn) str() string {
	// TODO
	return 'UdpConn'
}

pub fn (mut c UdpConn) close() ? {
	return c.sock.close()
}

pub fn listen_udp(laddr string) ?&UdpConn {
	addrs := resolve_addrs_fuzzy(laddr, .udp)?
	// TODO(emily):
	// here we are binding to the first address
	// and that is probably not ideal
	addr := addrs[0]
	return &UdpConn{
		sock: new_udp_socket(addr)?
		read_timeout: net.udp_default_read_timeout
		write_timeout: net.udp_default_write_timeout
	}
}

fn new_udp_socket(local_addr Addr) ?&UdpSocket {
	family := local_addr.family()

	sockfd := socket_error(C.socket(family, SocketType.udp, 0))?
	mut s := &UdpSocket{
		handle: sockfd
		l: local_addr
		r: Addr{
			addr: AddrData{
				Ip6: Ip6{}
			}
		}
	}

	s.set_option_bool(.reuse_addr, true)?

	if family == .ip6 {
		s.set_dualstack(true)?
	}

	$if !net_blocking_sockets ? {
		// NOTE: refer to comments in tcp.v
		$if windows {
			t := u32(1) // true
			socket_error(C.ioctlsocket(sockfd, fionbio, &t))?
		} $else {
			socket_error(C.fcntl(sockfd, C.F_SETFD, C.O_NONBLOCK))?
		}
	}

	// cast to the correct type
	socket_error(C.bind(s.handle, voidptr(&local_addr), local_addr.len()))?
	return s
}

fn new_udp_socket_for_remote(raddr Addr) ?&UdpSocket {
	// Invent a sutible local address for this remote addr
	// Appease compiler
	mut addr := Addr{
		addr: AddrData{
			Ip6: Ip6{}
		}
	}
	match raddr.family() {
		.ip {
			// Use ip dualstack
			addr = new_ip(0, addr_ip_any)
		}
		.ip6 {
			// Use ip6 dualstack
			addr = new_ip6(0, addr_ip6_any)
		}
		.unix {
			addr = temp_unix()?
		}
		else {
			panic('Invalid family')
		}
	}
	mut sock := new_udp_socket(addr)?
	sock.has_r = true
	sock.r = raddr

	return sock
}

pub fn (mut s UdpSocket) set_option_bool(opt SocketOption, value bool) ? {
	// TODO reenable when this `in` operation works again
	// if opt !in opts_can_set {
	// 	return err_option_not_settable
	// }
	// if opt !in opts_bool {
	// 	return err_option_wrong_type
	// }
	x := int(value)
	socket_error(C.setsockopt(s.handle, C.SOL_SOCKET, int(opt), &x, sizeof(int)))?
}

pub fn (mut s UdpSocket) set_dualstack(on bool) ? {
	x := int(!on)
	socket_error(C.setsockopt(s.handle, C.IPPROTO_IPV6, int(SocketOption.ipv6_only), &x,
		sizeof(int)))?
}

fn (mut s UdpSocket) close() ? {
	return shutdown(s.handle)
}

fn (mut s UdpSocket) @select(test Select, timeout time.Duration) ?bool {
	return @select(s.handle, test, timeout)
}

fn (s &UdpSocket) remote() ?Addr {
	if s.has_r {
		return s.r
	}
	return none
}
