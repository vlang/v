// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// BEAM backend network types
// These are placeholder types that allow network-related code to compile for the BEAM backend.
// Actual network operations would need to be implemented using Erlang/OTP network libraries.
module net

import time

// Platform-specific constants for BEAM
const max_unix_path = 104

const is_windows = false

// Error constants used in socket operations
pub const msg_nosignal = 0
pub const msg_dontwait = 0

pub const error_ewouldblock = 11
pub const error_einprogress = 115
pub const error_eagain = 11
pub const error_eintr = 4

// no_deadline should be given to functions when no deadline is wanted
const no_deadline = time.unix(0)

// no_timeout should be given to functions when no timeout is wanted
pub const no_timeout = time.Duration(0)

// infinite_timeout should be given to functions when an infinite timeout is wanted
pub const infinite_timeout = time.infinite

// TCP default timeouts
pub const tcp_default_read_timeout = 30 * time.second
pub const tcp_default_write_timeout = 30 * time.second

// Select represents a select operation
enum Select {
	read
	write
	except
}

// SocketType are the available socket types
pub enum SocketType {
	udp       = 2
	tcp       = 1
	seqpacket = 5
	raw       = 3
}

// AddrFamily are the available address families
pub enum AddrFamily {
	unix   = 1
	ip     = 2
	ip6    = 10
	unspec = 0
}

// SocketOption are the available socket options
pub enum SocketOption {
	broadcast        = 6
	debug            = 1
	dont_route       = 5
	error            = 4
	keep_alive       = 9
	linger           = 13
	oob_inline       = 10
	reuse_addr       = 2
	receive_buf_size = 8
	receive_low_size = 18
	receive_timeout  = 20
	send_buf_size    = 7
	send_low_size    = 19
	send_timeout     = 21
	socket_type      = 3
	ipv6_only        = 26
	ip_proto_ipv6    = 41
}

// ShutdownDirection is used by shutdown for specifying the direction
pub enum ShutdownDirection {
	read
	write
	read_and_write
}

// Ip6 represents an IPv6 address
@[_pack: '1']
pub struct Ip6 {
	port      u16
	flow_info u32
	addr      [16]u8
	scope_id  u32
}

// str returns a string representation of the IPv6 address
pub fn (a Ip6) str() string {
	// Simplified representation for BEAM backend
	return '[::]:${a.port}'
}

// Ip represents an IPv4 address
@[_pack: '1']
pub struct Ip {
	port    u16
	addr    [4]u8
	sin_pad [8]u8
}

// str returns a string representation of the IPv4 address
pub fn (a Ip) str() string {
	return '${a.addr[0]}.${a.addr[1]}.${a.addr[2]}.${a.addr[3]}:${a.port}'
}

// Unix represents a Unix domain socket address
pub struct Unix {
	path [max_unix_path]char
}

// AddrData is a union type for different address types
union AddrData {
	Unix
	Ip
	Ip6
}

// Addr represents a network address
@[_pack: '1']
pub struct Addr {
pub:
	len  u8
	f    u8
	addr AddrData
}

// family returns the address family of this address
pub fn (a Addr) family() AddrFamily {
	return unsafe { AddrFamily(a.f) }
}

// port returns the port number for IP addresses
pub fn (a Addr) port() !u16 {
	match unsafe { AddrFamily(a.f) } {
		.ip {
			unsafe {
				return a.addr.Ip.port
			}
		}
		.ip6 {
			unsafe {
				return a.addr.Ip6.port
			}
		}
		.unix {
			return error('unix addr has no port')
		}
		.unspec {
			return error('cannot find port for unspec addr family')
		}
	}
}

// len returns the length in bytes of the address
pub fn (a Addr) len() u32 {
	match a.family() {
		.ip {
			return sizeof(Ip) + 2
		}
		.ip6 {
			return sizeof(Ip6) + 2
		}
		.unix {
			return sizeof(Unix) + 2
		}
		else {
			return 0
		}
	}
}

// str returns a string representation of the address
pub fn (a Addr) str() string {
	match unsafe { AddrFamily(a.f) } {
		.ip {
			unsafe {
				return a.addr.Ip.str()
			}
		}
		.ip6 {
			unsafe {
				return a.addr.Ip6.str()
			}
		}
		.unix {
			return '<unix>'
		}
		.unspec {
			return '<.unspec>'
		}
	}
}

// Socket represents a basic socket
pub struct Socket {
pub:
	handle int
}

// address gets the address of a socket
pub fn (s &Socket) address() !Addr {
	return Addr{}
}

// TcpSocket represents a TCP socket
struct TcpSocket {
	Socket
}

// UdpSocket represents a UDP socket
struct UdpSocket {
	Socket
	l Addr
mut:
	has_r bool
	r     Addr
}

// remote returns the remote address if set
pub fn (s &UdpSocket) remote() ?Addr {
	if s.has_r {
		return s.r
	}
	return none
}

// TcpConn represents a TCP connection
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

// close closes the TCP connection
pub fn (mut c TcpConn) close() ! {
	// BEAM backend: placeholder
}

// read reads data from the connection
pub fn (c TcpConn) read(mut buf []u8) !int {
	return error('TcpConn.read not implemented for BEAM backend')
}

// write writes data to the connection
pub fn (mut c TcpConn) write(bytes []u8) !int {
	return error('TcpConn.write not implemented for BEAM backend')
}

// write_string writes a string to the connection
pub fn (mut c TcpConn) write_string(s string) !int {
	return error('TcpConn.write_string not implemented for BEAM backend')
}

// peer_addr retrieves the peer address
pub fn (c &TcpConn) peer_addr() !Addr {
	return Addr{}
}

// addr retrieves the local address
pub fn (c &TcpConn) addr() !Addr {
	return Addr{}
}

// str returns a string representation
pub fn (c TcpConn) str() string {
	return 'TcpConn{}'
}

// set_read_timeout sets the read timeout
pub fn (mut c TcpConn) set_read_timeout(t time.Duration) {
	c.read_timeout = t
}

// set_write_timeout sets the write timeout
pub fn (mut c TcpConn) set_write_timeout(t time.Duration) {
	c.write_timeout = t
}

// UdpConn represents a UDP connection
pub struct UdpConn {
pub mut:
	sock UdpSocket
mut:
	write_deadline time.Time
	read_deadline  time.Time
	read_timeout   time.Duration
	write_timeout  time.Duration
}

// close closes the UDP connection
pub fn (mut c UdpConn) close() ! {
	// BEAM backend: placeholder
}

// read reads data from the connection
pub fn (mut c UdpConn) read(mut buf []u8) !(int, Addr) {
	return error('UdpConn.read not implemented for BEAM backend')
}

// write writes data to the connection
pub fn (mut c UdpConn) write(buf []u8) !int {
	return error('UdpConn.write not implemented for BEAM backend')
}

// write_to_ptr writes raw data to a specific address
pub fn (mut c UdpConn) write_to_ptr(addr Addr, b &u8, len int) !int {
	return error('UdpConn.write_to_ptr not implemented for BEAM backend')
}

// write_to writes the buffer to the specified address
pub fn (mut c UdpConn) write_to(addr Addr, buf []u8) !int {
	return error('UdpConn.write_to not implemented for BEAM backend')
}

// write_to_string writes a string to the specified address
pub fn (mut c UdpConn) write_to_string(addr Addr, s string) !int {
	return error('UdpConn.write_to_string not implemented for BEAM backend')
}

// write_string writes a string to the connection
pub fn (mut c UdpConn) write_string(s string) !int {
	return error('UdpConn.write_string not implemented for BEAM backend')
}

// str returns a string representation
pub fn (c &UdpConn) str() string {
	return 'UdpConn'
}

// TcpListener represents a TCP listener
pub struct TcpListener {
pub mut:
	sock            TcpSocket
	accept_timeout  time.Duration
	accept_deadline time.Time
	is_blocking     bool = true
}

// accept accepts a connection
pub fn (mut l TcpListener) accept() !&TcpConn {
	return error('TcpListener.accept not implemented for BEAM backend')
}

// close closes the listener
pub fn (mut c TcpListener) close() ! {
	// BEAM backend: placeholder
}

// addr retrieves the local address
pub fn (c &TcpListener) addr() !Addr {
	return Addr{}
}

// TCPDialer is a concrete instance of the Dialer interface
pub struct TCPDialer {}

// dial creates a new connection
pub fn (t TCPDialer) dial(address string) !Connection {
	return dial_tcp(address)!
}

// default_tcp_dialer returns a default TCP dialer
pub fn default_tcp_dialer() Dialer {
	return &TCPDialer{}
}

// dial_tcp creates a new TCP connection to the given address
pub fn dial_tcp(address string) !&TcpConn {
	return error('dial_tcp not implemented for BEAM backend')
}

// listen_tcp starts listening on the given address
pub fn listen_tcp(family AddrFamily, saddr string) !&TcpListener {
	return error('listen_tcp not implemented for BEAM backend')
}

// listen_udp starts listening for UDP on the given address
pub fn listen_udp(laddr string) !&UdpConn {
	return error('listen_udp not implemented for BEAM backend')
}

// dial_udp creates a new UDP connection
pub fn dial_udp(raddr string) !&UdpConn {
	return error('dial_udp not implemented for BEAM backend')
}

// new_ip creates a new IPv4 address
pub fn new_ip(port u16, addr [4]u8) Addr {
	return Addr{
		f: u8(AddrFamily.ip)
		addr: AddrData{
			Ip: Ip{
				port: port
			}
		}
	}
}

// new_ip6 creates a new IPv6 address
pub fn new_ip6(port u16, addr [16]u8) Addr {
	return Addr{
		f: u8(AddrFamily.ip6)
		addr: AddrData{
			Ip6: Ip6{
				port: port
			}
		}
	}
}

// resolve_addrs resolves an address string to a list of addresses
pub fn resolve_addrs(addr string, family AddrFamily, typ SocketType) ![]Addr {
	return error('resolve_addrs not implemented for BEAM backend')
}

// resolve_addrs_fuzzy resolves an address with automatic family detection
pub fn resolve_addrs_fuzzy(addr string, typ SocketType) ![]Addr {
	return error('resolve_addrs_fuzzy not implemented for BEAM backend')
}

// NOTE: split_address is defined in util.v and uses string methods (count, index, etc.)
// which are now available in string.beam.v

// error_code returns the last error code
pub fn error_code() int {
	return 0
}

// set_blocking sets the blocking mode of a socket
pub fn set_blocking(handle int, state bool) ! {
	// BEAM backend: placeholder
}

// shutdown shuts down a socket
pub fn shutdown(handle int) int {
	return 0
}

// close closes a socket handle
pub fn close(handle int) ! {
	// BEAM backend: placeholder
}

// Well defined errors
pub const errors_base = 0
pub const err_new_socket_failed = error_with_code('net: new_socket failed', errors_base + 1)
pub const err_option_not_settable = error_with_code('net: option not settable', errors_base + 2)
pub const err_option_wrong_type = error_with_code('net: option wrong type', errors_base + 3)
pub const err_port_out_of_range = error_with_code('net: port out of range', errors_base + 5)
pub const err_no_udp_remote = error_with_code('net: no udp remote', errors_base + 6)
pub const err_connect_failed = error_with_code('net: connect failed', errors_base + 7)
pub const err_connect_timed_out = error_with_code('net: connect timed out', errors_base + 8)
pub const err_timed_out = error_with_code('net: op timed out', errors_base + 9)
pub const err_timed_out_code = errors_base + 9
pub const err_connection_refused = error_with_code('net: connection refused', errors_base + 10)
