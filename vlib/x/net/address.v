module net

// Addr represents an ip address
pub struct Addr {
	addr C.sockaddr
	len int
pub:
	saddr string
	port int
}

pub fn (a Addr) str() string {
	return '${a.saddr}:${a.port}'
}

const (
	max_ipv4_addr_len = 16
)

fn new_addr(addr C.sockaddr, _saddr string, _port int) ?Addr {
	mut saddr := _saddr
	if saddr == '' {
		// Convert to string representation
		buf := []byte{ len: max_ipv4_addr_len, init: 0 }
		$if windows {
			res := C.WSAStringToAddress(&addr, SocketFamily.inet, C.NULL, &buf.data, &buf.len)
			if res == 0 {
				socket_error(-1)?
			}
		} $else {
			res := C.inet_ntop(SocketFamily.inet, &addr, buf.data, buf.len)
			if res == 0 {
				socket_error(-1)?
			}
		}
		saddr = buf.bytestr()
	}

	mut port := _port
	if port == 0 {
		hport := (&C.sockaddr_in(&addr)).sin_port
		port = C.ntohs(hport)
	}

	return Addr {
		addr int(sizeof(C.sockaddr)) saddr port
	}
}

pub fn resolve_addr(addr string, family SocketFamily, typ SocketType) ?Addr {
	address, port := split_address(addr)?

	mut hints := C.addrinfo{}
	hints.ai_family = family
	hints.ai_socktype = typ
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
		wrap_error(C.getaddrinfo(address.str, sport.str, &hints, &info))
	}

	return new_addr(*info.ai_addr, address, port)
}
