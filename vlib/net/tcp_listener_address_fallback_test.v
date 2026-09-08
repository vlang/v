module net

fn test_listen_tcp_tries_resolved_addresses_in_order() ! {
	unavailable := resolve_addrs('192.0.2.1:0', .ip, .tcp)![0]
	available := resolve_addrs('127.0.0.1:0', .ip, .tcp)![0]
	mut listener :=
		listen_tcp_with_addresses([unavailable, available], 'resolved addresses', ListenOptions{})!
	defer {
		listener.close() or {}
	}
	assert listener.addr()!.family() == .ip
}
