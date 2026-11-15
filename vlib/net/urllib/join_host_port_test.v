module urllib

fn test_join_host_port() {
	assert join_host_port('localhost', '8080') == 'localhost:8080'
	assert join_host_port('::1', '80') == '[::1]:80'
	assert join_host_port('example.com', '') == 'example.com:'
}

fn test_join_host_port_empty_components() {
	assert join_host_port('', '') == ':'
	assert join_host_port('', '8080') == ':8080'
	assert join_host_port('::1', '') == '[::1]:'
}

fn test_join_host_port_ipv6_and_zone() {
	assert join_host_port('2001:db8::1', '443') == '[2001:db8::1]:443'
	assert join_host_port('::ffff:192.0.2.1', '80') == '[::ffff:192.0.2.1]:80'
	assert join_host_port('fe80::1%ETH0', '0') == '[fe80::1%ETH0]:0'
}

fn test_join_host_port_accepts_bracketed_ipv6() {
	assert join_host_port('[::1]', '80') == '[::1]:80'
	assert join_host_port('[fe80::1%eth0]', '') == '[fe80::1%eth0]:'
}

fn test_join_host_port_round_trip() {
	for host in ['', 'example.com', '127.0.0.1', '::1', 'fe80::1%eth0'] {
		for port in ['', '0', '443', '65535'] {
			got_host, got_port := split_host_port(join_host_port(host, port))
			assert got_host == host
			assert got_port == port
		}
	}
}

fn test_join_host_port_preserves_host_and_service_spelling() {
	assert join_host_port('Example.COM', 'http') == 'Example.COM:http'
	assert join_host_port('::1', 'https') == '[::1]:https'
}
