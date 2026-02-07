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

// set_option_int sets an integer socket option
pub fn (mut s TcpSocket) set_option_int(opt SocketOption, value int) ! {
	// BEAM: no-op stub
}

// set_option_bool sets a boolean socket option
pub fn (mut s TcpSocket) set_option_bool(opt SocketOption, value bool) ! {
	// BEAM: no-op stub
}

// set_option sets a raw socket option (stub on BEAM).
fn (mut s TcpSocket) set_option(level int, opt int, value int) ! {
	// BEAM: no-op stub
}

// set_dualstack enables or disables dual-stack (IPv4+IPv6) mode.
pub fn (mut s TcpSocket) set_dualstack(on bool) ! {
	// BEAM: no-op stub
}

// set_default_options sets default socket options (stub on BEAM).
fn (mut s TcpSocket) set_default_options() ! {
	// BEAM: no-op stub
}

// bind binds the socket to an address
pub fn (mut s TcpSocket) bind(addr string) ! {
	// BEAM: no-op stub
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
// BEAM codegen: gen_tcp:close(Socket)
// Requires codegen interception to call Erlang's gen_tcp:close/1
pub fn (mut c TcpConn) close() ! {
	// On BEAM, the actual socket handle would be an Erlang port reference
	// stored in c.handle. Codegen translates this to gen_tcp:close(Socket).
	// Without codegen, this is a no-op that clears the handle.
	c.handle = -1
}

// read reads data from the connection into the provided buffer.
// Returns the number of bytes read.
// BEAM codegen: gen_tcp:recv(Socket, 0, Timeout) -> {ok, Data} | {error, Reason}
// Requires codegen interception to call Erlang's gen_tcp:recv/3
pub fn (c TcpConn) read(mut buf []u8) !int {
	// On BEAM, this becomes:
	//   Timeout = case ReadTimeout of 0 -> infinity; T -> T end,
	//   case gen_tcp:recv(Socket, 0, Timeout) of
	//       {ok, Data} -> copy Data into buf, return byte count;
	//       {error, closed} -> return 0;
	//       {error, Reason} -> error(Reason)
	//   end
	return error('TcpConn.read requires codegen interception (gen_tcp:recv)')
}

// read_ptr reads data from the connection into a raw pointer buffer.
// BEAM note: Raw pointer operations are not idiomatic on BEAM.
// This delegates to the buffer-based read when codegen is available.
pub fn (c TcpConn) read_ptr(buf_ptr &u8, len int) !int {
	return error('TcpConn.read_ptr requires codegen interception (gen_tcp:recv)')
}

// write writes data to the connection.
// Returns the number of bytes written.
// BEAM codegen: gen_tcp:send(Socket, Data) -> ok | {error, Reason}
// Requires codegen interception to call Erlang's gen_tcp:send/2
pub fn (mut c TcpConn) write(bytes []u8) !int {
	// On BEAM, this becomes:
	//   case gen_tcp:send(Socket, list_to_binary(Bytes)) of
	//       ok -> byte_size(Data);
	//       {error, Reason} -> error(Reason)
	//   end
	return error('TcpConn.write requires codegen interception (gen_tcp:send)')
}

// write_string writes a string to the connection.
// BEAM codegen: gen_tcp:send(Socket, Binary)
pub fn (mut c TcpConn) write_string(s string) !int {
	// On BEAM, strings are already binaries, so this is efficient:
	//   case gen_tcp:send(Socket, S) of
	//       ok -> byte_size(S);
	//       {error, Reason} -> error(Reason)
	//   end
	return error('TcpConn.write_string requires codegen interception (gen_tcp:send)')
}

// write_ptr writes raw bytes from a pointer to the connection.
// BEAM note: Raw pointer operations are not idiomatic on BEAM.
pub fn (mut c TcpConn) write_ptr(b &u8, len int) !int {
	return error('TcpConn.write_ptr requires codegen interception (gen_tcp:send)')
}

// read_deadline returns the read deadline
pub fn (mut c TcpConn) read_deadline() !time.Time {
	return c.read_deadline
}

// write_deadline returns the write deadline
pub fn (mut c TcpConn) write_deadline() !time.Time {
	return c.write_deadline
}

// set_read_deadline sets the read deadline
pub fn (mut c TcpConn) set_read_deadline(deadline time.Time) {
	c.read_deadline = deadline
}

// set_write_deadline sets the write deadline
pub fn (mut c TcpConn) set_write_deadline(deadline time.Time) {
	c.write_deadline = deadline
}

// read_timeout returns the read timeout
pub fn (c &TcpConn) read_timeout() time.Duration {
	return c.read_timeout
}

// write_timeout returns the write timeout
pub fn (c &TcpConn) write_timeout() time.Duration {
	return c.write_timeout
}

// wait_for_read waits until data is available
pub fn (c TcpConn) wait_for_read() ! {
	// BEAM: no-op stub
}

// wait_for_write waits until write is possible
pub fn (mut c TcpConn) wait_for_write() ! {
	// BEAM: no-op stub
}

// set_sock configures the socket
pub fn (mut c TcpConn) set_sock() ! {
	// BEAM: no-op stub
}

// peer_addr retrieves the peer address.
// BEAM codegen: inet:peername(Socket) -> {ok, {Addr, Port}} | {error, Reason}
pub fn (c &TcpConn) peer_addr() !Addr {
	// Requires codegen: inet:peername(Socket)
	return Addr{}
}

// peer_ip retrieves the peer IP address as a string
pub fn (c &TcpConn) peer_ip() !string {
	addr := c.peer_addr()!
	return addr.str()
}

// addr retrieves the local address.
// BEAM codegen: inet:sockname(Socket) -> {ok, {Addr, Port}} | {error, Reason}
pub fn (c &TcpConn) addr() !Addr {
	// Requires codegen: inet:sockname(Socket)
	return Addr{}
}

// str returns a string representation of the TCP connection
pub fn (c TcpConn) str() string {
	return 'TcpConn{handle: ${c.handle}, blocking: ${c.is_blocking}}'
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
// BEAM codegen: gen_udp:close(Socket)
pub fn (mut c UdpConn) close() ! {
	// On BEAM, codegen translates to gen_udp:close(Socket)
}

// read reads data from the UDP connection.
// Returns the number of bytes read and the source address.
// BEAM codegen: gen_udp:recv(Socket, 0, Timeout) -> {ok, {Addr, Port, Data}} | {error, Reason}
pub fn (mut c UdpConn) read(mut buf []u8) !(int, Addr) {
	return error('UdpConn.read requires codegen interception (gen_udp:recv)')
}

// write writes data to the connected UDP peer.
// BEAM codegen: gen_udp:send(Socket, Data)
pub fn (mut c UdpConn) write(buf []u8) !int {
	if !c.sock.has_r {
		return err_no_udp_remote
	}
	return error('UdpConn.write requires codegen interception (gen_udp:send)')
}

// write_to_ptr writes raw data to a specific address
// BEAM note: Raw pointer operations are not idiomatic on BEAM.
pub fn (mut c UdpConn) write_to_ptr(addr Addr, b &u8, len int) !int {
	return error('UdpConn.write_to_ptr requires codegen interception (gen_udp:send)')
}

// write_to writes the buffer to the specified address.
// BEAM codegen: gen_udp:send(Socket, Addr, Port, Data)
pub fn (mut c UdpConn) write_to(addr Addr, buf []u8) !int {
	return error('UdpConn.write_to requires codegen interception (gen_udp:send)')
}

// write_to_string writes a string to the specified address.
// BEAM codegen: gen_udp:send(Socket, Addr, Port, Binary)
pub fn (mut c UdpConn) write_to_string(addr Addr, s string) !int {
	return error('UdpConn.write_to_string requires codegen interception (gen_udp:send)')
}

// write_string writes a string to the connected UDP peer.
// BEAM codegen: gen_udp:send(Socket, Binary)
pub fn (mut c UdpConn) write_string(s string) !int {
	if !c.sock.has_r {
		return err_no_udp_remote
	}
	return error('UdpConn.write_string requires codegen interception (gen_udp:send)')
}

// str returns a string representation
pub fn (c &UdpConn) str() string {
	s := if c.sock.has_r { c.sock.r.str() } else { 'unconnected' }
	return 'UdpConn(${s})'
}

// TcpListener represents a TCP listener
pub struct TcpListener {
pub mut:
	sock            TcpSocket
	accept_timeout  time.Duration
	accept_deadline time.Time
	is_blocking     bool = true
}

// accept accepts a connection and sets default socket options.
// BEAM codegen: gen_tcp:accept(ListenSocket, Timeout) -> {ok, Socket} | {error, Reason}
pub fn (mut l TcpListener) accept() !&TcpConn {
	// On BEAM, this becomes:
	//   Timeout = case AcceptTimeout of 0 -> infinity; T -> T end,
	//   case gen_tcp:accept(ListenSocket, Timeout) of
	//       {ok, Socket} ->
	//           inet:setopts(Socket, [{active, false}, {packet, raw}, binary]),
	//           #{'v.net.TcpConn' => #{sock => Socket, ...}};
	//       {error, Reason} -> error(Reason)
	//   end
	return error('TcpListener.accept requires codegen interception (gen_tcp:accept)')
}

// accept_only accepts a connection without setting additional options.
// BEAM codegen: gen_tcp:accept(ListenSocket, Timeout)
pub fn (mut l TcpListener) accept_only() !&TcpConn {
	return error('TcpListener.accept_only requires codegen interception (gen_tcp:accept)')
}

// accept_deadline returns the accept deadline
pub fn (c &TcpListener) accept_deadline() !time.Time {
	return c.accept_deadline
}

// set_accept_deadline sets the accept deadline
pub fn (mut c TcpListener) set_accept_deadline(deadline time.Time) {
	c.accept_deadline = deadline
}

// accept_timeout returns the accept timeout
pub fn (c &TcpListener) accept_timeout() time.Duration {
	return c.accept_timeout
}

// set_accept_timeout sets the accept timeout for the listener
pub fn (mut l TcpListener) set_accept_timeout(t time.Duration) {
	l.accept_timeout = t
}

// wait_for_accept waits until a connection is available
pub fn (mut c TcpListener) wait_for_accept() ! {
	// BEAM: no-op stub
}

// close closes the listening socket.
// BEAM codegen: gen_tcp:close(ListenSocket)
pub fn (mut c TcpListener) close() ! {
	// On BEAM, codegen translates to gen_tcp:close(ListenSocket)
}

// addr retrieves the local address
pub fn (c &TcpListener) addr() !Addr {
	return Addr{}
}

@[params]
pub struct ListenOptions {
pub:
	dualstack bool = true
	backlog   int  = 128
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

// new_tcp_socket creates a new TCP socket.
// BEAM note: On BEAM, sockets are created implicitly by gen_tcp:connect or gen_tcp:listen.
// This function returns a placeholder socket; the real Erlang socket is created during dial/listen.
pub fn new_tcp_socket(family AddrFamily) !TcpSocket {
	return TcpSocket{
		Socket: Socket{
			handle: 0
		}
	}
}

// tcp_socket_from_handle_raw creates a TcpSocket from a raw file descriptor
pub fn tcp_socket_from_handle_raw(sockfd int) TcpSocket {
	return TcpSocket{
		Socket: Socket{
			handle: sockfd
		}
	}
}

// dial_tcp creates a new TCP connection to the given address (host:port).
// BEAM codegen:
//   {Host, Port} = split_address(Address),
//   case gen_tcp:connect(Host, Port, [binary, {active, false}, {packet, raw}], Timeout) of
//       {ok, Socket} -> TcpConn#{sock => Socket, ...};
//       {error, econnrefused} -> error("connection refused");
//       {error, timeout} -> error("connect timed out");
//       {error, Reason} -> error(Reason)
//   end
pub fn dial_tcp(address string) !&TcpConn {
	// Validate the address format
	host, port := split_address(address)!
	if host.len == 0 {
		return error('dial_tcp: empty host')
	}
	if port == 0 {
		return error('dial_tcp: port 0 is not valid for TCP')
	}
	// The actual gen_tcp:connect call requires codegen interception.
	// The address parsing above is real V logic; the connection is Erlang.
	return error('dial_tcp requires codegen interception (gen_tcp:connect) for ${host}:${port}')
}

// dial_tcp_with_bind creates a new TCP connection with local address binding.
// BEAM codegen: gen_tcp:connect with {ip, LocalAddr} option
pub fn dial_tcp_with_bind(saddr string, laddr string) !&TcpConn {
	_, _ := split_address(saddr)!
	_, _ := split_address(laddr)!
	return error('dial_tcp_with_bind requires codegen interception (gen_tcp:connect with bind)')
}

// listen_tcp starts listening on the given address.
// BEAM codegen:
//   {_Host, Port} = split_address(Address),
//   case gen_tcp:listen(Port, [binary, {active, false}, {packet, raw},
//                              {reuseaddr, true}, {backlog, Backlog}]) of
//       {ok, ListenSocket} -> TcpListener#{sock => ListenSocket, ...};
//       {error, Reason} -> error(Reason)
//   end
pub fn listen_tcp(family AddrFamily, saddr string, options ListenOptions) !&TcpListener {
	_, port := split_address(saddr)!
	if port == 0 {
		return error('listen_tcp: port 0 is not valid')
	}
	return error('listen_tcp requires codegen interception (gen_tcp:listen) for port ${port}')
}

// listen_udp starts listening for UDP on the given address.
// BEAM codegen:
//   {_Host, Port} = split_address(Address),
//   case gen_udp:open(Port, [binary, {active, false}]) of
//       {ok, Socket} -> UdpConn#{sock => Socket, ...};
//       {error, Reason} -> error(Reason)
//   end
pub fn listen_udp(laddr string) !&UdpConn {
	_, _ := split_address(laddr)!
	return error('listen_udp requires codegen interception (gen_udp:open)')
}

// dial_udp creates a new UDP connection to the given address.
// BEAM codegen:
//   {Host, Port} = split_address(Address),
//   {ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
//   gen_udp:connect(Socket, Host, Port)
pub fn dial_udp(raddr string) !&UdpConn {
	_, _ := split_address(raddr)!
	return error('dial_udp requires codegen interception (gen_udp:open + gen_udp:connect)')
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

// resolve_addrs resolves an address string to a list of addresses.
// BEAM codegen: inet:gethostbyname(Host, Family) -> {ok, #hostent{h_addr_list=Addrs}} | {error, Reason}
// The address parsing is done in pure V; the DNS resolution requires Erlang's inet module.
pub fn resolve_addrs(addr string, family AddrFamily, typ SocketType) ![]Addr {
	host, port := split_address(addr)!
	// Check for literal IPv4 address (avoids DNS lookup)
	if is_ipv4_literal(host) {
		parts := host.split('.')
		if parts.len == 4 {
			a0 := u8(parts[0].int())
			a1 := u8(parts[1].int())
			a2 := u8(parts[2].int())
			a3 := u8(parts[3].int())
			mut ip_addr := Addr{
				f: u8(AddrFamily.ip)
				addr: AddrData{
					Ip: Ip{
						port: port
						addr: [a0, a1, a2, a3]!
					}
				}
			}
			return [ip_addr]
		}
	}
	// For hostname resolution, we need Erlang's inet:gethostbyname
	// Codegen translates to:
	//   case inet:gethostbyname(list_to_atom(Host), Family) of
	//       {ok, #hostent{h_addr_list = Addrs}} -> [make_addr(A, Port) || A <- Addrs];
	//       {error, Reason} -> error(Reason)
	//   end
	return error('resolve_addrs: DNS resolution requires codegen interception (inet:gethostbyname) for host ${host}')
}

// resolve_addrs_fuzzy resolves an address with automatic family detection.
// First tries IPv4, then IPv6 if that fails.
pub fn resolve_addrs_fuzzy(addr string, typ SocketType) ![]Addr {
	// Try IPv4 first
	addrs := resolve_addrs(addr, .ip, typ) or {
		// Then try IPv6
		return resolve_addrs(addr, .ip6, typ)
	}
	return addrs
}

// is_ipv4_literal checks whether a string looks like an IPv4 address (e.g. "127.0.0.1")
fn is_ipv4_literal(s string) bool {
	parts := s.split('.')
	if parts.len != 4 {
		return false
	}
	for part in parts {
		if part.len == 0 || part.len > 3 {
			return false
		}
		val := part.int()
		if val < 0 || val > 255 {
			return false
		}
		// Check for leading zeros (invalid in strict IPv4)
		if part.len > 1 && part[0] == `0` {
			return false
		}
	}
	return true
}

// NOTE: split_address is defined in util.v and uses string methods (count, index, etc.)
// which are now available in string.beam.v

// error_code returns the last error code
pub fn error_code() int {
	return 0
}

// set_blocking sets the blocking mode of a socket.
// BEAM note: On BEAM, socket blocking is controlled via {active, true|false} option.
// In passive mode ({active, false}), recv is blocking. This is the default for V.
// BEAM codegen: inet:setopts(Socket, [{active, Active}])
pub fn set_blocking(handle int, state bool) ! {
	// On BEAM, blocking mode maps to the {active, false} socket option
	// which is set by default. Non-blocking maps to {active, true} or {active, once}.
	// This requires codegen: inet:setopts(Socket, [{active, !state}])
}

// shutdown shuts down part of a full-duplex connection.
// BEAM codegen: gen_tcp:shutdown(Socket, Direction)
pub fn shutdown(handle int) int {
	// On BEAM: gen_tcp:shutdown(Socket, read_write)
	return 0
}

// close closes a socket handle.
// BEAM codegen: gen_tcp:close(Socket) or gen_udp:close(Socket)
pub fn close(handle int) ! {
	// On BEAM, codegen translates to the appropriate close call
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
