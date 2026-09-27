module fasthttp

// An empty host must keep the historical wildcard bind, so existing servers
// that never set a host stay reachable on every interface.
fn test_empty_host_binds_wildcard() {
	a6 := resolve_bind_addr('', .ip6, 8081) or {
		assert false, 'ip6 wildcard failed: ${err}'
		return
	}
	assert a6.family() == .ip6
	assert a6.str() == '[::]:8081'

	a4 := resolve_bind_addr('', .ip, 8082) or {
		assert false, 'ip wildcard failed: ${err}'
		return
	}
	assert a4.family() == .ip
	assert a4.str() == '0.0.0.0:8082'
}

// An IPv4 literal must bind that address only, even with the default
// family: .ip6 (the exact configuration from vlang/v issue #28119).
fn test_ipv4_literal_overrides_default_ip6_family() {
	a := resolve_bind_addr('127.0.0.1', .ip6, 8083) or {
		assert false, 'ipv4 literal failed: ${err}'
		return
	}
	assert a.family() == .ip
	assert a.str() == '127.0.0.1:8083'
	assert a.port()! == 8083
}

fn test_ipv4_literal_with_matching_family() {
	a := resolve_bind_addr('127.0.0.1', .ip, 8084) or {
		assert false, 'ipv4 literal failed: ${err}'
		return
	}
	assert a.family() == .ip
	assert a.str() == '127.0.0.1:8084'
}

// A bare IPv6 literal must be bracketed internally so the whole address is
// kept and it binds loopback IPv6 only.
fn test_ipv6_literal_binds_that_address() {
	a := resolve_bind_addr('::1', .ip6, 8085) or {
		assert false, 'ipv6 literal failed: ${err}'
		return
	}
	assert a.family() == .ip6
	assert a.str() == '[::1]:8085'
	assert a.port()! == 8085
}

fn test_unresolvable_host_errors() {
	if _ := resolve_bind_addr('definitely.not.a.real.host.invalid', .ip, 8086) {
		assert false, 'expected an error for an unresolvable host'
	}
}

// The startup "listening on ..." host must be a valid URL host component: the
// wildcard reflects the family that is actually bound, and IPv6 literals are
// bracketed.
fn test_listen_host_display() {
	assert listen_host_display('', .ip) == '0.0.0.0'
	assert listen_host_display('', .ip6) == '[::]'
	assert listen_host_display('127.0.0.1', .ip) == '127.0.0.1'
	assert listen_host_display('localhost', .ip6) == 'localhost'
	assert listen_host_display('::1', .ip6) == '[::1]'
	assert listen_host_display('[::1]', .ip6) == '[::1]'
}

fn unresolvable_host_handler(_ HttpRequest) !HttpResponse {
	return HttpResponse{
		content: 'HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n'.bytes()
	}
}

// run() must surface a bind-address resolution failure as an error instead of
// returning successfully with no listener (regression guard for the Linux path,
// which previously converted the failure to a bare success return).
fn test_run_errors_on_unresolvable_host() {
	mut server := new_server(ServerConfig{
		family:  .ip
		host:    'definitely.not.a.real.host.invalid'
		port:    0
		handler: unresolvable_host_handler
	}) or {
		assert false, 'new_server failed: ${err}'
		return
	}
	server.run() or {
		// Expected: resolution fails before any listener/worker is created.
		assert err.msg().len > 0
		return
	}
	assert false, 'run() should have returned an error for an unresolvable host'
}
