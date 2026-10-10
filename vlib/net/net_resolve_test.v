import net

// Only the `":port"` forms are exercised here: they return a locally built
// any-address and never reach getaddrinfo, so the tests need no network and
// no name resolution.
fn test_resolve_ipaddrs_maps_a_bare_port_to_the_ipv4_any_address() {
	for family in [net.AddrFamily.ip, net.AddrFamily.unspec] {
		addrs := net.resolve_ipaddrs(':80', family, .tcp)!
		assert addrs.len == 1
		assert addrs[0].str() == '0.0.0.0:80'
		assert addrs[0].port()! == 80
		assert addrs[0].family() == .ip
	}
}

fn test_resolve_ipaddrs_maps_a_bare_port_to_the_ipv6_any_address() {
	addrs := net.resolve_ipaddrs(':0', .ip6, .tcp)!
	assert addrs.len == 1
	assert addrs[0].str() == '[::]:0'
	assert addrs[0].port()! == 0
	assert addrs[0].family() == .ip6
}

fn test_resolve_ipaddrs_rejects_a_port_out_of_range() {
	net.resolve_ipaddrs(':99999', .ip, .tcp) or {
		assert err.msg() == 'net: port out of range'
		assert err.code() == 5
		return
	}
	assert false, 'resolve_ipaddrs() should have rejected the out of range port'
}

// NOTE: `resolve_ipaddrs('')` panics with `string index out of range`, because
// `addr[0]` is read before the empty string is handled, while
// `resolve_addrs_fuzzy('')` does guard it. Left as is, only reported.

fn test_peer_addr_from_socket_handle_annotates_the_failure() {
	// A listening socket has no peer, so getpeername fails and
	// peer_addr_from_socket_handle has to report why. Nothing is dialled and
	// no name resolution happens.
	mut l := net.listen_tcp(.ip, ':0')!
	defer {
		l.close() or {}
	}
	net.peer_addr_from_socket_handle(l.sock.handle) or {
		assert err.msg().starts_with('net: socket error:')
		assert err.msg().ends_with('; peer_addr_from_socket_handle failed')
		return
	}
	assert false, 'peer_addr_from_socket_handle() on a listening socket should have failed'
}
