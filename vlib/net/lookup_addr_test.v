module net

fn test_lookup_addr_rejects_empty() {
	if _ := lookup_addr('') {
		assert false, 'expected error for empty address'
	} else {
		assert err.msg().contains('empty address')
	}
}

fn test_lookup_addr_rejects_nul() {
	if _ := lookup_addr('127.0.0\x001') {
		assert false, 'expected error for NUL byte'
	} else {
		assert err.msg().contains('NUL byte')
	}
}

fn test_lookup_addr_rejects_hostname() {
	// Host names must be rejected without touching DNS.
	if _ := lookup_addr('example.com') {
		assert false, 'expected error for host name'
	} else {
		assert err.msg().contains('invalid IP address')
	}
}

fn test_lookup_addr_rejects_garbage() {
	if _ := lookup_addr('999.1.1.1') {
		assert false, 'expected error for invalid IPv4'
	} else {
		assert err.msg().contains('invalid IP address')
	}
	if _ := lookup_addr(':::') {
		assert false, 'expected error for invalid IPv6'
	} else {
		assert err.msg().contains('invalid IP address')
	}
}
