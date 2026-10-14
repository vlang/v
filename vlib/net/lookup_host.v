module net

// lookup_host resolves a host name to the list of its IP address literals,
// in the order the resolver returned them. Numeric inputs resolve without
// touching DNS, so the function is safe on values that may or may not need
// resolution. name is a host name, not a host:port pair. IPv6 literals may
// optionally be bracketed; a trailing port is rejected.
// It is the V-idiomatic equivalent of Node's `dns.lookup` / Go's
// `net.LookupHost`.
pub fn lookup_host(name string) ![]string {
	if name.len == 0 {
		return error('net.lookup_host: empty host name')
	}
	if name.contains('\x00') {
		return error('net.lookup_host: host name contains a NUL byte')
	}
	if name.contains_any('[]') {
		if !name.starts_with('[') || !name.ends_with(']') || name.count('[') != 1
			|| name.count(']') != 1 || !name.contains(':') {
			return error('net.lookup_host: invalid bracketed host name `${name}`')
		}
	}
	// Bare IPv6 literals collide with vlib's `addr[0] == ':'` any-address
	// shortcut in resolve_ipaddrs, so bracket them first (split_address
	// understands the bracketed form). Host names never contain ':'.
	qname := if name.contains(':') && name[0] != `[` { '[${name}]' } else { name }
	addrs := resolve_ipaddrs(qname, .unspec, .tcp)!
	mut out := []string{}
	for a in addrs {
		if lit := addr_literal(a) {
			out << lit
		}
	}
	if out.len == 0 {
		return error('net.lookup_host: no addresses for `${name}`')
	}
	return out
}

// resolve_ip resolves a host name and returns its first address, the common
// convenience case.
pub fn resolve_ip(name string) !string {
	addrs := lookup_host(name)!
	if addrs.len == 0 {
		return error('net.resolve_ip: no addresses for `${name}`')
	}
	return addrs[0]
}

// addr_literal renders the IP part of an Addr without its port:
// `Addr.str()` yields `ip:port` for IPv4 and `[ip]:port` for IPv6.
fn addr_literal(a Addr) ?string {
	match unsafe { AddrFamily(a.f) } {
		.ip {
			s := a.str()
			if _ := s.index(':') {
				return s.all_before_last(':')
			}
			return none
		}
		.ip6 {
			s := a.str()
			if s.starts_with('[') {
				if _ := s.index(']:') {
					return s[1..].all_before_last(']:')
				}
			}
			return none
		}
		else {
			return none
		}
	}
}
