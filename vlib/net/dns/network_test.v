module dns

import os

// Real-network integration tests. Skipped unless the env var
// DNS_NETWORK_TESTS=1 is set, because they require:
//   - outbound UDP/53 connectivity
//   - reachability to public resolvers (1.1.1.1, 8.8.8.8)
//   - network conditions stable enough that timeouts do not flake
//
// They are the final smoke check that the whole stack — encode +
// transport + decode + resolver — works against unmocked servers.

fn skip_if_disabled() bool {
	if os.getenv('DNS_NETWORK_TESTS') != '1' {
		eprintln('  [skipped] set DNS_NETWORK_TESTS=1 to enable real-network tests')
		return true
	}
	return false
}

fn test_network_lookup_a_example_com() {
	if skip_if_disabled() {
		return
	}
	mut r := Resolver{
		servers: ['1.1.1.1:53', '8.8.8.8:53']
	}
	addrs := r.lookup_a('example.com')!
	assert addrs.len >= 1
	for a in addrs {
		assert a.split('.').len == 4
	}
}

fn test_network_lookup_aaaa_example_com() {
	if skip_if_disabled() {
		return
	}
	mut r := Resolver{
		servers: ['1.1.1.1:53', '8.8.8.8:53']
	}
	addrs := r.lookup_aaaa('example.com')!
	assert addrs.len >= 1
	for a in addrs {
		assert a.contains(':')
	}
}

fn test_network_lookup_mx_gmail_com() {
	if skip_if_disabled() {
		return
	}
	mut r := Resolver{
		servers: ['1.1.1.1:53', '8.8.8.8:53']
	}
	mxs := r.lookup_mx('gmail.com')!
	assert mxs.len >= 1
	// Sorted by preference ascending.
	for i in 1 .. mxs.len {
		assert mxs[i].preference >= mxs[i - 1].preference
	}
}

fn test_network_new_resolver_uses_resolv_conf() {
	if skip_if_disabled() {
		return
	}
	mut r := new_resolver()!
	assert r.servers.len >= 1
	// We don't assert the exact server (depends on /etc/resolv.conf),
	// but it must answer.
	addrs := r.lookup_a('a.root-servers.net')!
	assert addrs.len >= 1
}

fn test_network_nxdomain_surfaces_as_error() {
	if skip_if_disabled() {
		return
	}
	mut r := Resolver{
		servers: ['1.1.1.1:53']
	}
	if _ := r.lookup_a('this-domain-must-not-exist.example.') {
		assert false, 'NXDOMAIN must error'
	}
}
