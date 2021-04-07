module net

const max_unix_path = 104

struct Unix {
	path [max_unix_path]byte
}

union AddrData {
	Unix
	Ip
	Ip6
}

pub fn (a Addr) family() AddrFamily {
	return AddrFamily(a.f)
}

const (
	max_ip_len = 24
	max_ip6_len = 46
)

fn (a Ip) str() string {
	buf := []byte{len: max_ip_len, init: 0}

	$if windows {
		res := C.WSAAddressToStringA(&a, i.len, C.NULL, buf.data, &buf.len)
		if res != 0 {
			return '<Unknown>'
		}
		// Windows will return the port as part of the address
		return buf.bytestr()
	} $else {
		res := charptr(C.inet_ntop(.ip, &a.addr, buf.data, buf.len))

		if res == 0 {
			return '<Unknown>'
		}
	}
	saddr := buf.bytestr()
	port := C.ntohs(a.port)

	return '$saddr:$port'
}

fn (a Ip6) str() string {
	buf := []byte{len: max_ip6_len, init: 0}

	$if windows {
		res := C.WSAAddressToStringA(&i.addr, i.len, C.NULL, buf.data, &buf.len)
		if res != 0 {
			return '<Unknown>'
		}
		// Windows will return the port as part of the address
		return buf.bytestr()
	} $else {
		res := charptr(C.inet_ntop(.ip6, a.addr[0..a.addr.len].data, buf.data, buf.len))

		if res == 0 {
			return '<Unknown>'
		}
	}

	saddr := buf.bytestr()
	port := C.ntohs(a.port)

	return '[$saddr]:$port'
}

fn (a Addr) len() u32 {
	match a.family() {
		.ip {
			return sizeof(Ip)
		}

		.ip6 {
			return sizeof(Ip6)
		}
		
		.unix {
			return sizeof(Unix)
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
			resolved := Unix {}

			if addr.len > max_unix_path {
				return error('net: resolve_addr2 Unix socket address is too long')
			}

			// Copy the unix path into the address struct
			unsafe {
				C.memcpy(&resolved.path, addr.str, addr.len)
			}

			return [Addr{f: u16(AddrFamily.unix) addr: AddrData{Unix: resolved}}]
		}
	}
}

pub fn resolve_addrs_fuzzy(addr string, @type SocketType) ?[]Addr {
	// Use a small heuristic to figure out what address family this is
	// (out of the ones that we support)

	if (addr.contains(':')) {
		// Colon is a reserved character in unix paths
		// so this must be an ip address
		return resolve_addrs(addr, .unspec, @type)
	}

	return resolve_addrs(addr, unix, @type)
}

pub fn resolve_ipaddrs(addr string, family AddrFamily, typ SocketType) ?[]Addr {
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
	results := &C.addrinfo(0)

	sport := '$port'

	// This might look silly but is recommended by MSDN
	$if windows {
		socket_error(0 - C.getaddrinfo(address.str, sport.str, &hints, &results)) ?
	} $else {
		x := C.getaddrinfo(address.str, sport.str, &hints, &results)
		wrap_error(x) ?
	}

	defer { C.freeaddrinfo(results) }

	// Now that we have our linked list of addresses
	// convert them into an array
	mut addresses := []Addr{}

	for result := results; !isnil(result); result = result.ai_next {
		match AddrFamily(result.ai_family) {
			.ip, .ip6 {
				new_addr := Addr{}
				unsafe {
					C.memcpy(&new_addr, result.ai_addr, result.ai_addrlen)
				}
				addresses << new_addr
			}

			else {
				panic('Unexpected address family ${result.ai_family}')
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
	addr := Addr{}
	size := sizeof(addr)

	C.getsockname(handle, &addr, &size)

	return addr
}
