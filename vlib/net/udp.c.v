module net

import time

const udp_default_read_timeout = time.second / 10
const udp_default_write_timeout = time.second / 10

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
	is_blocking    bool = true
}

@[_pack: '1']
struct Ipv4MulticastRequest {
	multiaddr    [4]u8
	interface_ip [4]u8
}

@[_pack: '1']
struct Ipv6MulticastRequest {
	multiaddr       [16]u8
	interface_index u32
}

pub fn dial_udp(raddr string) !&UdpConn {
	addrs := resolve_addrs_fuzzy(raddr, .udp)!

	for addr in addrs {
		// create a local socket for this
		// bind to any port (or file) (we dont care in this
		// case because we only care about the remote)
		if sock := new_udp_socket_for_remote(addr) {
			mut conn := &UdpConn{
				sock:          sock
				read_timeout:  udp_default_read_timeout
				write_timeout: udp_default_write_timeout
			}
			$if net_nonblocking_sockets ? {
				conn.is_blocking = false
			}
			return conn
		}
	}

	return error('none')
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

pub fn (mut c UdpConn) write_ptr(b &u8, len int) !int {
	remote := c.sock.remote() or { return err_no_udp_remote }
	return c.write_to_ptr(remote, b, len)
}

pub fn (mut c UdpConn) write(buf []u8) !int {
	return c.write_ptr(buf.data, buf.len)
}

pub fn (mut c UdpConn) write_string(s string) !int {
	return c.write_ptr(s.str, s.len)
}

pub fn (mut c UdpConn) write_to_ptr(addr Addr, b &u8, len int) !int {
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
	return error('none')
}

// write_to blocks and writes the buf to the remote addr specified
pub fn (mut c UdpConn) write_to(addr Addr, buf []u8) !int {
	return c.write_to_ptr(addr, buf.data, buf.len)
}

// write_to_string blocks and writes the buf to the remote addr specified
pub fn (mut c UdpConn) write_to_string(addr Addr, s string) !int {
	return c.write_to_ptr(addr, s.str, s.len)
}

// read_ptr reads from the socket into `buf_ptr` up to `len` bytes, returning the number of bytes read and the `Addr` read from.
pub fn (c &UdpConn) read_ptr(buf_ptr &u8, len int) !(int, Addr) {
	mut addr := Addr{
		addr: AddrData{
			Ip6: Ip6{}
		}
	}
	addr_len := sizeof(Addr)
	mut res := 0
	if c.is_blocking {
		// Honor read deadlines/timeouts before entering a blocking recvfrom call.
		c.wait_for_read()!
		res = wrap_read_result(C.recvfrom(c.sock.handle, voidptr(buf_ptr), len, 0, voidptr(&addr),
			&addr_len))!
	} else {
		res = wrap_read_result(C.recvfrom(c.sock.handle, voidptr(buf_ptr), len, 0, voidptr(&addr),
			&addr_len))!
	}
	if res > 0 {
		return res, addr
	}
	code := error_code()
	if code in [int(error_ewouldblock), int(error_eagain), C.EINTR] {
		c.wait_for_read()!
		// same setup as in tcp
		res = wrap_read_result(C.recvfrom(c.sock.handle, voidptr(buf_ptr), len, 0, voidptr(&addr),
			&addr_len))!
		res2 := socket_error(res)!
		return res2, addr
	} else {
		wrap_error(code)!
	}
	return error('none')
}

// read reads from the socket into buf up to buf.len returning the number of bytes read
pub fn (mut c UdpConn) read(mut buf []u8) !(int, Addr) {
	return c.read_ptr(buf.data, buf.len)!
}

pub fn (c &UdpConn) read_deadline() !time.Time {
	if c.read_deadline.unix() == 0 {
		return c.read_deadline
	}
	return error('none')
}

pub fn (mut c UdpConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

pub fn (c &UdpConn) write_deadline() !time.Time {
	if c.write_deadline.unix() == 0 {
		return c.write_deadline
	}
	return error('none')
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

@[inline]
pub fn (c &UdpConn) wait_for_read() ! {
	return wait_for_read(c.sock.handle, c.read_deadline, c.read_timeout)
}

@[inline]
pub fn (mut c UdpConn) wait_for_write() ! {
	return wait_for_write(c.sock.handle, c.write_deadline, c.write_timeout)
}

pub fn (c &UdpConn) str() string {
	// TODO
	return 'UdpConn'
}

pub fn (mut c UdpConn) close() ! {
	return c.sock.close()
}

// join_multicast_group joins the UDP socket to an IPv4 or IPv6 multicast group.
// For IPv4 sockets, `iface_addr` must be an IPv4 address or `''` to use the default interface.
// For IPv6 sockets, `iface_addr` must be a numeric interface index or `''` to use the default interface.
pub fn (mut c UdpConn) join_multicast_group(multicast_addr string, iface_addr string) ! {
	family := c.multicast_family()!
	match family {
		.ip {
			mreq := Ipv4MulticastRequest{
				multiaddr:    parse_ipv4_multicast_addr(multicast_addr)!
				interface_ip: parse_ipv4_interface_addr(iface_addr)!
			}
			c.sock.set_option_raw(C.IPPROTO_IP, C.IP_ADD_MEMBERSHIP, &mreq,
				sizeof(Ipv4MulticastRequest))!
		}
		.ip6 {
			mreq := Ipv6MulticastRequest{
				multiaddr:       parse_ipv6_multicast_addr(multicast_addr)!
				interface_index: parse_ipv6_interface_index(iface_addr)!
			}
			c.sock.set_option_raw(C.IPPROTO_IPV6, ipv6_membership_socket_option(true), &mreq,
				sizeof(Ipv6MulticastRequest))!
		}
		else {
			return error('net: udp multicast is only supported on ip and ip6 sockets')
		}
	}
}

// leave_multicast_group leaves an IPv4 or IPv6 multicast group previously joined by the socket.
// `iface_addr` follows the same rules as `join_multicast_group`.
pub fn (mut c UdpConn) leave_multicast_group(multicast_addr string, iface_addr string) ! {
	family := c.multicast_family()!
	match family {
		.ip {
			mreq := Ipv4MulticastRequest{
				multiaddr:    parse_ipv4_multicast_addr(multicast_addr)!
				interface_ip: parse_ipv4_interface_addr(iface_addr)!
			}
			c.sock.set_option_raw(C.IPPROTO_IP, C.IP_DROP_MEMBERSHIP, &mreq,
				sizeof(Ipv4MulticastRequest))!
		}
		.ip6 {
			mreq := Ipv6MulticastRequest{
				multiaddr:       parse_ipv6_multicast_addr(multicast_addr)!
				interface_index: parse_ipv6_interface_index(iface_addr)!
			}
			c.sock.set_option_raw(C.IPPROTO_IPV6, ipv6_membership_socket_option(false), &mreq,
				sizeof(Ipv6MulticastRequest))!
		}
		else {
			return error('net: udp multicast is only supported on ip and ip6 sockets')
		}
	}
}

// set_multicast_ttl sets the IPv4 TTL or IPv6 hop limit used for outgoing multicast traffic.
// Valid values are between 0 and 255 inclusive.
pub fn (mut c UdpConn) set_multicast_ttl(ttl int) ! {
	if ttl < 0 || ttl > 255 {
		return error('net: multicast ttl must be between 0 and 255')
	}
	match c.multicast_family()! {
		.ip {
			c.sock.set_option_int(C.IPPROTO_IP, C.IP_MULTICAST_TTL, ttl)!
		}
		.ip6 {
			c.sock.set_option_int(C.IPPROTO_IPV6, C.IPV6_MULTICAST_HOPS, ttl)!
		}
		else {
			return error('net: udp multicast is only supported on ip and ip6 sockets')
		}
	}
}

// set_multicast_loop enables or disables local loopback for outgoing multicast packets.
pub fn (mut c UdpConn) set_multicast_loop(enable bool) ! {
	value := int(enable)
	match c.multicast_family()! {
		.ip {
			c.sock.set_option_int(C.IPPROTO_IP, C.IP_MULTICAST_LOOP, value)!
		}
		.ip6 {
			c.sock.set_option_int(C.IPPROTO_IPV6, C.IPV6_MULTICAST_LOOP, value)!
		}
		else {
			return error('net: udp multicast is only supported on ip and ip6 sockets')
		}
	}
}

// set_multicast_interface sets the outgoing multicast interface for the UDP socket.
// For IPv4 sockets, `iface_addr` must be an IPv4 address or `''` to clear it to the default interface.
// For IPv6 sockets, `iface_addr` must be a numeric interface index or `''` to use the default interface.
pub fn (mut c UdpConn) set_multicast_interface(iface_addr string) ! {
	match c.multicast_family()! {
		.ip {
			addr := parse_ipv4_interface_addr(iface_addr)!
			c.sock.set_option_raw(C.IPPROTO_IP, C.IP_MULTICAST_IF, &addr[0], sizeof([4]u8))!
		}
		.ip6 {
			index := parse_ipv6_interface_index(iface_addr)!
			c.sock.set_option_int(C.IPPROTO_IPV6, C.IPV6_MULTICAST_IF, int(index))!
		}
		else {
			return error('net: udp multicast is only supported on ip and ip6 sockets')
		}
	}
}

pub fn listen_udp(laddr string) !&UdpConn {
	addrs := resolve_addrs_fuzzy(laddr, .udp)!
	// TODO(emily):
	// here we are binding to the first address
	// and that is probably not ideal
	addr := addrs[0]
	mut conn := &UdpConn{
		sock:          new_udp_socket(addr)!
		read_timeout:  udp_default_read_timeout
		write_timeout: udp_default_write_timeout
	}
	$if net_nonblocking_sockets ? {
		conn.is_blocking = false
	}
	return conn
}

fn new_udp_socket(local_addr Addr) !&UdpSocket {
	family := local_addr.family()

	sockfd := socket_error(C.socket(i32(family), i32(SocketType.udp), 0))!
	mut s := &UdpSocket{
		handle: sockfd
		l:      local_addr
		r:      Addr{
			addr: AddrData{
				Ip6: Ip6{}
			}
		}
	}

	s.set_option_bool(.reuse_addr, true)!

	if family == .ip6 {
		s.set_dualstack(true)!
	}

	$if net_nonblocking_sockets ? {
		set_blocking(sockfd, false)!
	}

	// cast to the correct type
	socket_error(C.bind(s.handle, voidptr(&local_addr), local_addr.len()))!
	return s
}

fn new_udp_socket_for_remote(raddr Addr) !&UdpSocket {
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
			addr = temp_unix()!
		}
		else {
			panic('Invalid family')
		}
	}

	mut sock := new_udp_socket(addr)!
	sock.has_r = true
	sock.r = raddr

	return sock
}

fn (c &UdpConn) multicast_family() !AddrFamily {
	family := c.sock.l.family()
	if family !in [.ip, .ip6] {
		return error('net: udp multicast is only supported on ip and ip6 sockets')
	}
	return family
}

fn normalize_ip_literal(address string) string {
	if address.len >= 2 && address[0] == `[` && address[address.len - 1] == `]` {
		return address[1..address.len - 1]
	}
	return address
}

fn parse_ipv4_interface_addr(iface_addr string) ![4]u8 {
	if iface_addr.len == 0 {
		return [4]u8{}
	}
	address := normalize_ip_literal(iface_addr)
	mut parsed := [4]u8{}
	if C.inet_pton(i32(AddrFamily.ip), &char(address.str), &parsed[0]) != 1 {
		return error('net: ipv4 multicast interface must be an ipv4 address')
	}
	return parsed
}

fn parse_ipv4_multicast_addr(multicast_addr string) ![4]u8 {
	address := normalize_ip_literal(multicast_addr)
	mut parsed := [4]u8{}
	if C.inet_pton(i32(AddrFamily.ip), &char(address.str), &parsed[0]) != 1 {
		return error('net: invalid ipv4 multicast address `${multicast_addr}`')
	}
	if parsed[0] < 224 || parsed[0] > 239 {
		return error('net: `${multicast_addr}` is not an ipv4 multicast address')
	}
	return parsed
}

fn parse_ipv6_multicast_addr(multicast_addr string) ![16]u8 {
	address := normalize_ip_literal(multicast_addr)
	mut parsed := [16]u8{}
	if C.inet_pton(i32(AddrFamily.ip6), &char(address.str), &parsed[0]) != 1 {
		return error('net: invalid ipv6 multicast address `${multicast_addr}`')
	}
	if parsed[0] != 0xff {
		return error('net: `${multicast_addr}` is not an ipv6 multicast address')
	}
	return parsed
}

fn parse_ipv6_interface_index(iface_addr string) !u32 {
	if iface_addr.len == 0 {
		return 0
	}
	for ch in iface_addr {
		if ch < `0` || ch > `9` {
			return error('net: ipv6 multicast interface must be a numeric interface index')
		}
	}
	return iface_addr.u32()
}

fn ipv6_membership_socket_option(join bool) int {
	$if windows {
		return if join { C.IPV6_ADD_MEMBERSHIP } else { C.IPV6_DROP_MEMBERSHIP }
	} $else {
		return if join { C.IPV6_JOIN_GROUP } else { C.IPV6_LEAVE_GROUP }
	}
}

fn (mut s UdpSocket) set_option_raw(level int, opt int, value voidptr, value_len u32) ! {
	socket_error(C.setsockopt(s.handle, level, opt, value, value_len))!
}

fn (mut s UdpSocket) set_option_int(level int, opt int, value int) ! {
	s.set_option_raw(level, opt, &value, sizeof(int))!
}

// set_option_bool sets a boolean socket option on the UDP socket.
pub fn (mut s UdpSocket) set_option_bool(opt SocketOption, value bool) ! {
	// TODO: reenable when this `in` operation works again
	// if opt !in opts_can_set {
	// 	return err_option_not_settable
	// }
	// if opt !in opts_bool {
	// 	return err_option_wrong_type
	// }
	x := int(value)
	s.set_option_int(C.SOL_SOCKET, int(opt), x)!
}

// set_dualstack enables or disables dual-stack behavior for IPv6 UDP sockets.
pub fn (mut s UdpSocket) set_dualstack(on bool) ! {
	x := int(!on)
	s.set_option_int(C.IPPROTO_IPV6, int(SocketOption.ipv6_only), x)!
}

// close shuts down and closes the socket for communication.
pub fn (mut s UdpSocket) close() ! {
	shutdown(s.handle)
	return close(s.handle)
}

// select waits for no more than `timeout` for the IO operation, defined by `test`, to be available.
pub fn (mut s UdpSocket) select(test Select, timeout time.Duration) !bool {
	return select(s.handle, test, timeout)
}

// remote returns the remote `Addr` address of the socket or `none` if no remote is has been resolved.
pub fn (s &UdpSocket) remote() ?Addr {
	if s.has_r {
		return s.r
	}
	return none
}
