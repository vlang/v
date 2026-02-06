module net

import io.util
import net.conv
import os

union AddrData {
	Unix
	Ip
	Ip6
}

const addr_ip6_any = [16]u8{init: u8(0)}
const addr_ip_any = [4]u8{init: u8(0)}

// new_ip6 creates a new Addr from the IP6 address family, based on the given port and addr
pub fn new_ip6(port u16, addr [16]u8) Addr {
	n_port := conv.hton16(port)
	a := Addr{
		f:    u8(AddrFamily.ip6)
		addr: AddrData{
			Ip6: Ip6{
				port: n_port
			}
		}
	}
	unsafe { vmemcpy(&a.addr.Ip6.addr[0], &addr[0], 16) }
	return a
}

// new_ip creates a new Addr from the IPv4 address family, based on the given port and addr
pub fn new_ip(port u16, addr [4]u8) Addr {
	n_port := conv.hton16(port)
	a := Addr{
		f:    u8(AddrFamily.ip)
		addr: AddrData{
			Ip: Ip{
				port: n_port
			}
		}
	}
	unsafe { vmemcpy(&a.addr.Ip.addr[0], &addr[0], 4) }
	return a
}

fn temp_unix() !Addr {
	// create a temp file to get a filename
	// close it
	// remove it
	// then reuse the filename
	mut file, filename := util.temp_file()!
	file.close()
	os.rm(filename)!
	addrs := resolve_addrs(filename, .unix, .udp)!
	return addrs[0]
}

// family returns the family/kind of the given address `a`
pub fn (a Addr) family() AddrFamily {
	return unsafe { AddrFamily(a.f) }
}

// port returns the ip or ip6 port of the given address `a`
pub fn (a Addr) port() !u16 {
	match unsafe { AddrFamily(a.f) } {
		.ip {
			unsafe {
				return conv.ntoh16(a.addr.Ip.port)
			}
		}
		.ip6 {
			unsafe {
				return conv.ntoh16(a.addr.Ip6.port)
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

const max_ip_len = 24
const max_ip6_len = 46

// str returns a string representation of `a`
pub fn (a Ip) str() string {
	buf := [max_ip_len]char{}

	res := &char(C.inet_ntop(.ip, &a.addr, &buf[0], buf.len))

	if res == 0 {
		return '<Unknown>'
	}

	saddr := unsafe { cstring_to_vstring(&buf[0]) }
	port := conv.ntoh16(a.port)
	return '${saddr}:${port}'
}

// str returns a string representation of `a`
pub fn (a Ip6) str() string {
	buf := [max_ip6_len]char{}

	res := &char(C.inet_ntop(.ip6, &a.addr, &buf[0], buf.len))

	if res == 0 {
		return '<Unknown>'
	}

	saddr := unsafe { cstring_to_vstring(&buf[0]) }
	port := conv.ntoh16(a.port)
	return '[${saddr}]:${port}'
}

const aoffset = __offsetof(Addr, addr)

// len returns the length in bytes of the address `a`, depending on its family
pub fn (a Addr) len() u32 {
	match a.family() {
		.ip {
			return sizeof(Ip) + aoffset
		}
		.ip6 {
			return sizeof(Ip6) + aoffset
		}
		.unix {
			return sizeof(Unix) + aoffset
		}
		else {
			panic('Unknown address family')
		}
	}
}

// resolve_addrs converts the given `addr`, `family` and `typ` to a list of addresses
pub fn resolve_addrs(addr string, family AddrFamily, typ SocketType) ![]Addr {
	match family {
		.ip, .ip6, .unspec {
			return resolve_ipaddrs(addr, family, typ)
		}
		.unix {
			resolved := Unix{}

			if addr.len > max_unix_path {
				return error('net: resolve_addrs Unix socket address is too long')
			}

			// Copy the unix path into the address struct
			unsafe {
				C.memcpy(&resolved.path, addr.str, addr.len)
			}

			return [
				Addr{
					f:    u8(AddrFamily.unix)
					addr: AddrData{
						Unix: resolved
					}
				},
			]
		}
	}
}

// resolve_addrs converts the given `addr` and `typ` to a list of addresses
pub fn resolve_addrs_fuzzy(addr string, typ SocketType) ![]Addr {
	if addr.len == 0 {
		return error('none')
	}

	// Use a small heuristic to figure out what address family this is
	// (out of the ones that we support)

	if addr.contains(':') {
		// Colon is a reserved character in unix paths
		// so this must be an ip address
		return resolve_addrs(addr, .unspec, typ)
	}

	return resolve_addrs(addr, .unix, typ)
}

// resolve_ipaddrs converts the given `addr`, `family` and `typ` to a list of addresses
pub fn resolve_ipaddrs(addr string, family AddrFamily, typ SocketType) ![]Addr {
	address, port := split_address(addr)!

	if addr[0] == `:` {
		match family {
			.ip6 {
				return [new_ip6(port, addr_ip6_any)]
			}
			.ip, .unspec {
				return [new_ip(port, addr_ip_any)]
			}
			else {}
		}
	}

	mut hints := C.addrinfo{
		// ai_family: int(family)
		// ai_socktype: int(typ)
		// ai_flags: C.AI_PASSIVE
	}
	unsafe { vmemset(&hints, 0, int(sizeof(hints))) }
	hints.ai_family = int(family)
	hints.ai_socktype = int(typ)
	hints.ai_flags = C.AI_PASSIVE

	results := &C.addrinfo(unsafe { nil })

	sport := '${port}'

	// This might look silly but is recommended by MSDN
	$if windows {
		socket_error(0 - C.getaddrinfo(&char(address.str), &char(sport.str), &hints, &results))!
	} $else {
		x := C.getaddrinfo(&char(address.str), &char(sport.str), &hints, &results)
		wrap_error(x)!
	}

	defer {
		C.freeaddrinfo(results)
	}

	// Now that we have our linked list of addresses
	// convert them into an array
	mut addresses := []Addr{}

	for result := unsafe { results }; !isnil(result); result = result.ai_next {
		match unsafe { AddrFamily(result.ai_family) } {
			.ip {
				new_addr := Addr{
					addr: AddrData{
						Ip: Ip{}
					}
				}
				unsafe {
					C.memcpy(&new_addr, result.ai_addr, result.ai_addrlen)
				}
				addresses << new_addr
			}
			.ip6 {
				new_addr := Addr{
					addr: AddrData{
						Ip6: Ip6{}
					}
				}
				unsafe {
					C.memcpy(&new_addr, result.ai_addr, result.ai_addrlen)
				}
				addresses << new_addr
			}
			else {
				panic('Unexpected address family ' + result.ai_family.str())
			}
		}
	}

	return addresses
}

// str returns a string representation of the address `a`
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
			unsafe {
				return tos_clone(a.addr.Unix.path[0..max_unix_path].data)
			}
		}
		.unspec {
			return '<.unspec>'
		}
	}
}

// addr_from_socket_handle returns an address, based on the given integer socket `handle`
pub fn addr_from_socket_handle(handle int) Addr {
	mut addr := Addr{
		addr: AddrData{
			Ip6: Ip6{}
		}
	}
	mut size := sizeof(addr)
	C.getsockname(handle, voidptr(&addr), &size)
	return addr
}

// peer_addr_from_socket_handle retrieves the ip address and port number, given a socket handle
pub fn peer_addr_from_socket_handle(handle int) !Addr {
	mut addr := Addr{
		addr: AddrData{
			Ip6: Ip6{}
		}
	}
	mut size := sizeof(Addr)
	socket_error_message(C.getpeername(handle, voidptr(&addr), &size), 'peer_addr_from_socket_handle failed')!
	return addr
}
