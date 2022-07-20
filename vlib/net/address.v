module net

import io.util
import os

union AddrData {
	Unix
	Ip
	Ip6
}

const (
	addr_ip6_any = [16]u8{init: u8(0)}
	addr_ip_any  = [4]u8{init: u8(0)}
)

fn new_ip6(port u16, addr [16]u8) Addr {
	a := Addr{
		f: u8(AddrFamily.ip6)
		addr: AddrData{
			Ip6: Ip6{
				port: u16(C.htons(port))
			}
		}
	}

	unsafe { vmemcpy(&a.addr.Ip6.addr[0], &addr[0], 16) }

	return a
}

fn new_ip(port u16, addr [4]u8) Addr {
	a := Addr{
		f: u8(AddrFamily.ip)
		addr: AddrData{
			Ip: Ip{
				port: u16(C.htons(port))
			}
		}
	}

	unsafe { vmemcpy(&a.addr.Ip.addr[0], &addr[0], 4) }

	return a
}

fn temp_unix() ?Addr {
	// create a temp file to get a filename
	// close it
	// remove it
	// then reuse the filename
	mut file, filename := util.temp_file()?
	file.close()
	os.rm(filename)?
	addrs := resolve_addrs(filename, .unix, .udp)?
	return addrs[0]
}

pub fn (a Addr) family() AddrFamily {
	return AddrFamily(a.f)
}

const (
	max_ip_len  = 24
	max_ip6_len = 46
)

fn (a Ip) str() string {
	buf := [net.max_ip_len]char{}

	res := &char(C.inet_ntop(.ip, &a.addr, &buf[0], buf.len))

	if res == 0 {
		return '<Unknown>'
	}

	saddr := unsafe { cstring_to_vstring(&buf[0]) }
	port := C.ntohs(a.port)

	return '$saddr:$port'
}

fn (a Ip6) str() string {
	buf := [net.max_ip6_len]char{}

	res := &char(C.inet_ntop(.ip6, &a.addr, &buf[0], buf.len))

	if res == 0 {
		return '<Unknown>'
	}

	saddr := unsafe { cstring_to_vstring(&buf[0]) }
	port := C.ntohs(a.port)

	return '[$saddr]:$port'
}

const aoffset = __offsetof(Addr, addr)

fn (a Addr) len() u32 {
	match a.family() {
		.ip {
			return sizeof(Ip) + net.aoffset
		}
		.ip6 {
			return sizeof(Ip6) + net.aoffset
		}
		.unix {
			return sizeof(Unix) + net.aoffset
		}
		else {
			panic('Unknown address family')
		}
	}
}

pub fn resolve_addrs(addr string, family AddrFamily, @type SocketType) ?[]Addr {
	match family {
		.ip, .ip6, .unspec {
			return resolve_ipaddrs(addr, family, @type)
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
					f: u8(AddrFamily.unix)
					addr: AddrData{
						Unix: resolved
					}
				},
			]
		}
	}
}

pub fn resolve_addrs_fuzzy(addr string, @type SocketType) ?[]Addr {
	if addr.len == 0 {
		return none
	}

	// Use a small heuristic to figure out what address family this is
	// (out of the ones that we support)

	if addr.contains(':') {
		// Colon is a reserved character in unix paths
		// so this must be an ip address
		return resolve_addrs(addr, .unspec, @type)
	}

	return resolve_addrs(addr, .unix, @type)
}

pub fn resolve_ipaddrs(addr string, family AddrFamily, typ SocketType) ?[]Addr {
	address, port := split_address(addr)?

	if addr[0] == `:` {
		match family {
			.ip6 {
				return [new_ip6(port, net.addr_ip6_any)]
			}
			.ip, .unspec {
				return [new_ip(port, net.addr_ip_any)]
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

	results := &C.addrinfo(0)

	sport := '$port'

	// This might look silly but is recommended by MSDN
	$if windows {
		socket_error(0 - C.getaddrinfo(&char(address.str), &char(sport.str), &hints, &results))?
	} $else {
		x := C.getaddrinfo(&char(address.str), &char(sport.str), &hints, &results)
		wrap_error(x)?
	}

	defer {
		C.freeaddrinfo(results)
	}

	// Now that we have our linked list of addresses
	// convert them into an array
	mut addresses := []Addr{}

	for result := unsafe { results }; !isnil(result); result = result.ai_next {
		match AddrFamily(result.ai_family) {
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
				panic('Unexpected address family $result.ai_family')
			}
		}
	}

	return addresses
}

fn (a Addr) str() string {
	match AddrFamily(a.f) {
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

pub fn addr_from_socket_handle(handle int) Addr {
	addr := Addr{
		addr: AddrData{
			Ip6: Ip6{}
		}
	}
	size := sizeof(addr)

	C.getsockname(handle, voidptr(&addr), &size)

	return addr
}
