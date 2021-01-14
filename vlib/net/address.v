module net

// Addr represents an ip address
pub struct Addr {
	addr C.sockaddr
	len int
pub:
	saddr string
	port int
}

struct C.addrinfo {
}

pub fn (a Addr) str() string {
	return '${a.saddr}:${a.port}'
}

const (
	max_ipv4_addr_len = 24
	ipv4_addr_size = sizeof(C.sockaddr_in)
)

fn new_addr(addr C.sockaddr) ?Addr {
	addr_len := if addr.sa_family == int(SocketFamily.inet) {
		sizeof(C.sockaddr)
	} else {
		// TODO NOOOOOOOOOOOO
		0
	}
	// Convert to string representation
	buf := []byte{ len: max_ipv4_addr_len, init: 0 }
	$if windows {
		res := C.WSAAddressToStringA(&addr, addr_len, C.NULL, buf.data, &buf.len)
		if res != 0 {
			socket_error(-1)?
		}
	} $else {
		res := C.inet_ntop(SocketFamily.inet, &addr, buf.data, buf.len)
		if res == 0 {
			socket_error(-1)?	
		}
	}
	mut saddr := buf.bytestr()

	hport := unsafe {&C.sockaddr_in(&addr)}.sin_port
	port := C.ntohs(hport)

	$if windows {
		// strip the port from the address string
		saddr = saddr.split(':')[0]
	}

	return Addr {
		addr int(addr_len) saddr port
	}
}

pub fn resolve_addr(addr string, family SocketFamily, typ SocketType) ?Addr {
	address, port := split_address(addr)?

	mut hints := C.addrinfo{}
	hints.ai_family = int(family)
	hints.ai_socktype = int(typ)
	hints.ai_flags = C.AI_PASSIVE
	hints.ai_protocol = 0
	hints.ai_addrlen = 0
	hints.ai_canonname = C.NULL
	hints.ai_addr = C.NULL
	hints.ai_next = C.NULL
	info := &C.addrinfo(0)

	sport := '$port'

	// This might look silly but is recommended by MSDN
	$if windows {
		socket_error(0-C.getaddrinfo(address.str, sport.str, &hints, &info))?
	} $else {
		x := C.getaddrinfo(address.str, sport.str, &hints, &info)    
		wrap_error(x)?
	}

	return new_addr(*info.ai_addr)
}
