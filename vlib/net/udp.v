module net

import time

const (
	udp_default_read_timeout  = 30 * time.second
	udp_default_write_timeout = 30 * time.second
)

pub struct UdpConn {
	sock           UdpSocket
mut:
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
}

pub fn dial_udp(laddr string, raddr string) ?UdpConn {
	// Dont have to do this when its fixed
	// this just allows us to store this `none` optional in a struct
	resolve_wrapper := fn (raddr string) ?Addr {
		x := resolve_addr(raddr, .inet, .udp) or { return none }
		return x
	}
	local := resolve_addr(laddr, .inet, .udp) ?
	sbase := new_udp_socket(local.port) ?
	sock := UdpSocket{
		handle: sbase.handle
		l: local
		r: resolve_wrapper(raddr)
	}
	return UdpConn{
		sock: sock
		read_timeout: udp_default_read_timeout
		write_timeout: udp_default_write_timeout
	}
}

pub fn (c UdpConn) write_ptr(b byteptr, len int) ? {
	remote := c.sock.remote() or { return err_no_udp_remote }
	return c.write_to_ptr(remote, b, len)
}

pub fn (c UdpConn) write(buf []byte) ? {
	return c.write_ptr(buf.data, buf.len)
}

pub fn (c UdpConn) write_str(s string) ? {
	return c.write_ptr(s.str, s.len)
}

pub fn (c UdpConn) write_to_ptr(addr Addr, b byteptr, len int) ? {
	res := C.sendto(c.sock.handle, b, len, 0, &addr.addr, addr.len)
	if res >= 0 {
		return none
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_write() ?
		socket_error(C.sendto(c.sock.handle, b, len, 0, &addr.addr, addr.len)) ?
	} else {
		wrap_error(code) ?
	}
	return none
}

// write_to blocks and writes the buf to the remote addr specified
pub fn (c UdpConn) write_to(addr Addr, buf []byte) ? {
	return c.write_to_ptr(addr, buf.data, buf.len)
}

// write_to_string blocks and writes the buf to the remote addr specified
pub fn (c UdpConn) write_to_string(addr Addr, s string) ? {
	return c.write_to_ptr(addr, s.str, s.len)
}

// read reads from the socket into buf up to buf.len returning the number of bytes read
pub fn (c UdpConn) read(mut buf []byte) ?(int, Addr) {
	mut addr_from := C.sockaddr{}
	len := sizeof(C.sockaddr)
	mut res := wrap_read_result(C.recvfrom(c.sock.handle, buf.data, buf.len, 0, &addr_from,
		&len)) ?
	if res > 0 {
		addr := new_addr(addr_from) ?
		return res, addr
	}
	code := error_code()
	if code == int(error_ewouldblock) {
		c.wait_for_read() ?
		// same setup as in tcp
		res = wrap_read_result(C.recvfrom(c.sock.handle, buf.data, buf.len, 0, &addr_from,
			&len)) ?
		res2 := socket_error(res) ?
		addr := new_addr(addr_from) ?
		return res2, addr
	} else {
		wrap_error(code) ?
	}
	return none
}

pub fn (c UdpConn) read_deadline() ?time.Time {
	if c.read_deadline.unix == 0 {
		return c.read_deadline
	}
	return none
}

pub fn (mut c UdpConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

pub fn (c UdpConn) write_deadline() ?time.Time {
	if c.write_deadline.unix == 0 {
		return c.write_deadline
	}
	return none
}

pub fn (mut c UdpConn) set_write_deadline(deadline time.Time) {
	c.write_deadline = deadline
}

pub fn (c UdpConn) read_timeout() time.Duration {
	return c.read_timeout
}

pub fn (mut c UdpConn) set_read_timeout(t time.Duration) {
	c.read_timeout = t
}

pub fn (c UdpConn) write_timeout() time.Duration {
	return c.write_timeout
}

pub fn (mut c UdpConn) set_write_timeout(t time.Duration) {
	c.write_timeout = t
}

[inline]
pub fn (c UdpConn) wait_for_read() ? {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

[inline]
pub fn (c UdpConn) wait_for_write() ? {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}

pub fn (c UdpConn) str() string {
	// TODO
	return 'UdpConn'
}

pub fn (c UdpConn) close() ? {
	return c.sock.close()
}

pub fn listen_udp(port int) ?UdpConn {
	s := new_udp_socket(port) ?
	return UdpConn{
		sock: s
		read_timeout: udp_default_read_timeout
		write_timeout: udp_default_write_timeout
	}
}

struct UdpSocket {
	handle int
	l      Addr
	r      ?Addr
}

fn new_udp_socket(local_port int) ?UdpSocket {
	sockfd := socket_error(C.socket(SocketFamily.inet, SocketType.udp, 0)) ?
	s := UdpSocket{
		handle: sockfd
	}
	s.set_option_bool(.reuse_addr, true) ?
	$if windows {
		t := true
		socket_error(C.ioctlsocket(sockfd, fionbio, &t)) ?
	} $else {
		socket_error(C.fcntl(sockfd, C.F_SETFD, C.O_NONBLOCK))
	}
	// In UDP we always have to bind to a port
	validate_port(local_port) ?
	mut addr := C.sockaddr_in{}
	addr.sin_family = int(SocketFamily.inet)
	addr.sin_port = C.htons(local_port)
	addr.sin_addr.s_addr = C.htonl(C.INADDR_ANY)
	size := sizeof(C.sockaddr_in)
	// cast to the correct type
	sockaddr := unsafe { &C.sockaddr(&addr) }
	socket_error(C.bind(s.handle, sockaddr, size)) ?
	return s
}

pub fn (s UdpSocket) remote() ?Addr {
	return s.r
}

pub fn (s UdpSocket) set_option_bool(opt SocketOption, value bool) ? {
	// TODO reenable when this `in` operation works again
	// if opt !in opts_can_set {
	// 	return err_option_not_settable
	// }
	// if opt !in opts_bool {
	// 	return err_option_wrong_type
	// }
	socket_error(C.setsockopt(s.handle, C.SOL_SOCKET, int(opt), &value, sizeof(bool))) ?
	return none
}

fn (s UdpSocket) close() ? {
	return shutdown(s.handle)
}

fn (s UdpSocket) @select(test Select, timeout time.Duration) ?bool {
	return @select(s.handle, test, timeout)
}
