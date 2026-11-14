module net

fn test_interfaces_have_loopback() {
	ifaces := interfaces()!
	assert ifaces.len > 0
	mut found_loopback := false
	for f in ifaces {
		assert f.name != ''
		for a in f.addrs {
			if a == '127.0.0.1' || a == '::1' {
				found_loopback = true
			}
		}
	}
	assert found_loopback, 'expected a loopback address on some interface'
}
