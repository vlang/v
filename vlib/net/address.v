module net

import os

interface Addr {
	addr SockaddrStorage
	len  int

	family SocketFamily
	@type SocketType
}

// IpAddr represents an ip address
pub struct  IpAddr {
	addr SockaddrStorage
	len  int

	family SocketFamily
	@type SocketType
}

const (
	max_ipv4_addr_len = 24
	max_ip_addr_len = 46
)

pub fn (i IpAddr) str() string {
	// Convert to string representation
	buf := []byte{len: max_ip_addr_len, init: 0}
	$if windows {
		res := C.WSAAddressToStringA(&i.addr, i.len, C.NULL, buf.data, &buf.len)
		if res != 0 {
			return '<Unknown>'
		}
		// Windows will return the port as part of the address
		return buf.bytestr()
	} $else {
		res := charptr(C.inet_ntop(addr.sa_family, addr, buf.data, buf.len))

		if res != 0 {
			return '<Unknown>'
		}
	}

	saddr := buf.bytestr()

	match i.family {
		.inet {
			hport := unsafe { i.addr.sockaddr_in.sin_port }
			port := C.ntohs(hport)

			return '$saddr:$port'
		}

		.inet6 {
			hport := unsafe { i.addr.sockaddr_in6.sin6_port }
			port := C.ntohs(hport)

			return '[$saddr]:$port'
		}

		else {
			panic('Address family is not inet or inet6?')
		}
	}
}

pub struct UnixAddr {
	addr SockaddrStorage
	len  int

	family SocketFamily
	@type SocketType
}

fn (a UnixAddr) str() string {
	return '<UnixAddr>'
}

const max_sun_path = 104

pub fn resolve_addrs(addr string, family SocketFamily, @type SocketType) ?[]Addr {
	// Do some heuristics on the address to figure
	// out what address type it should be

	match family {
		.inet, .inet6, .unspec {
			return resolve_ipaddr(addr, family, @type)
		}

		.unix {
			resolved := C.sockaddr_un {
				sun_family: .unix
			}

			if addr.len > max_sun_path {
				return error('net: resolve_addr2 Unix socket address is too long')
			}

			// Copy the unix path into the address struct
			unsafe {
				C.memcpy(&resolved.sun_path, addr.str, addr.len)
			}

			return [
				UnixAddr{
					SockaddrStorage{ sockaddr_un: resolved } 
					int(sizeof(resolved)) 
					(.unix)
					@type 
				}]
		}
	}
}

pub fn resolve_ipaddr(addr string, family SocketFamily, typ SocketType) ?[]Addr {
	address, port := split_address(addr) ?

	mut hints := C.addrinfo{
		// ai_family: int(family)
		// ai_socktype: int(typ)
		// ai_flags: C.AI_PASSIVE
	}
	hints.ai_family = int(family)
	hints.ai_socktype = int(typ)
	hints.ai_flags = C.AI_PASSIVE
	hints.ai_protocol = 0
	hints.ai_addrlen = 0
	hints.ai_addr = voidptr(0)
	hints.ai_canonname = voidptr(0)
	hints.ai_next = voidptr(0)
	info := &C.addrinfo(0)

	sport := '$port'

	// This might look silly but is recommended by MSDN
	$if windows {
		socket_error(0 - C.getaddrinfo(&char(address.str), &char(sport.str), &hints,
			&info)) ?
	} $else {
		x := C.getaddrinfo(&char(address.str), &char(sport.str), &hints, &info)
		wrap_error(x) ?
	}

	// Now that we have our linked list of addresses
	// convert them into an array
	mut addresses := []Addr{}

	for addrinfo := info; !isnil(addrinfo); addrinfo = addrinfo.ai_next {
		addresses << &IpAddr{ 
			addr: SockaddrStorage{sockaddr: *addrinfo.ai_addr }
			len: int(addrinfo.ai_addrlen)

			family: SocketFamily(addrinfo.ai_family)
			@type: SocketType(addrinfo.ai_socktype)
		}
	}

	return addresses
}
