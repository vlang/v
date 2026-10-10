import net

// resolve_ipaddrs indexed `addr[0]` before validating, so an empty host
// panicked with `string index out of range(idx,s.len):0, 0` instead of
// returning an error. resolve_addrs_fuzzy already guarded this case.
fn test_resolve_ipaddrs_rejects_an_empty_host_with_an_error() {
	if _ := net.resolve_ipaddrs('', .ip, .tcp) {
		assert false, 'net.resolve_ipaddrs("") should have failed'
	} else {
		assert err.msg().contains('empty')
	}
}

// The `":port"` forms must keep resolving to the any-addresses.
fn test_resolve_ipaddrs_bare_port_still_resolves() {
	addrs := net.resolve_ipaddrs(':8080', .ip, .tcp)!
	assert addrs.len == 1
	assert addrs[0].str() == '0.0.0.0:8080'
}
