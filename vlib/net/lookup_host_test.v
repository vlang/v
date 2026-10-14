import net

fn test_lookup_host_localhost() {
	addrs := net.lookup_host('localhost')!
	assert addrs.len > 0, 'localhost must resolve without network access'
	for a in addrs {
		// literals must be idempotent: no ports or brackets may leak in
		assert net.lookup_host(a)! == [a], 'literal `${a}` must re-resolve to itself'
	}
}

fn test_lookup_host_numeric_passthrough() {
	assert net.lookup_host('127.0.0.1')! == ['127.0.0.1']
	assert net.lookup_host('::1')! == ['::1']
	assert net.lookup_host('[::1]')! == ['::1']
	assert net.lookup_host('2001:db8::1')! == ['2001:db8::1']
	assert net.lookup_host('::')! == ['::']
	assert net.resolve_ip('127.0.0.1')! == '127.0.0.1'
}

fn test_lookup_host_empty_errors() {
	net.lookup_host('') or {
		assert err.msg() == 'net.lookup_host: empty host name'
		return
	}
	assert false, 'empty host name must error'
}

fn test_lookup_host_unknown_name_errors() {
	net.lookup_host('invalid.invalid') or {
		return
	}
	assert false, 'unresolvable name must error'
}

fn test_lookup_host_rejects_ports_and_malformed_brackets() {
	for name in ['127.0.0.1:80', '[::1]:80', '[::1]suffix', '[localhost]', '[::1', '::1]', '[[::1]]',
		'localhost\x00.invalid'] {
		assert_lookup_host_rejected(name)
	}
}

fn assert_lookup_host_rejected(name string) {
	net.lookup_host(name) or { return }
	assert false, 'host name `${name}` must be rejected'
}
