module net

fn C.getnameinfo(sa voidptr, salen u32, host &char, hostlen u32, serv &char, servlen u32, flags int) int

$if windows {
	// ws2tcpip.h numbering differs from POSIX netdb.h.
	const ni_namereqd = 4
} $else {
	const ni_namereqd = 8
}

const ni_maxhost = 1025

// lookup_addr performs a reverse DNS lookup of the IP literal `addr`
// (dotted IPv4 or `::` IPv6, with optional brackets) and returns the
// host names the resolver reports, in the order returned.
// It is the V-idiomatic equivalent of Go's `net.LookupAddr`.
// Only IP literals are accepted; host names are rejected without
// touching DNS.
pub fn lookup_addr(addr string) ![]string {
	if addr.len == 0 {
		return error('net.lookup_addr: empty address')
	}
	if addr.contains('\x00') {
		return error('net.lookup_addr: address contains a NUL byte')
	}
	clean := addr.trim_space()
	bare := if clean.starts_with('[') && clean.ends_with(']') && clean.len >= 2 {
		clean[1..clean.len - 1]
	} else {
		clean
	}
	is_v6 := bare.contains(':')
	if is_v6 {
		canonical_ipv6(bare) or { return error('net.lookup_addr: invalid IP address `${addr}`') }
	} else {
		parse_ip(bare) or { return error('net.lookup_addr: invalid IP address `${addr}`') }
	}
	// Numeric literals resolve without touching DNS.
	q := if is_v6 { '[${bare}]' } else { bare }
	addrs := resolve_ipaddrs(q, .unspec, .tcp)!
	mut out := []string{}
	for a in addrs {
		host := reverse_lookup_name(a)!
		if host !in out {
			out << host
		}
	}
	if out.len == 0 {
		return error('net.lookup_addr: no names for `${addr}`')
	}
	return out
}

fn reverse_lookup_name(a Addr) !string {
	buf := [ni_maxhost]char{}
	rc := C.getnameinfo(voidptr(&a), a.len(), &buf[0], u32(buf.len), &char(unsafe { nil }),
		0, ni_namereqd)
	wrap_getaddrinfo_error(rc)!
	unsafe {
		return cstring_to_vstring(&buf[0])
	}
}
