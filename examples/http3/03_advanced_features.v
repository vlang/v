// HTTP/3 Features via Unified API
// Demonstrates HTTP/3 capabilities through the unified net.http API:
// Alt-Svc discovery, automatic protocol upgrade, and multi-protocol serving.
//
// Note: For advanced QUIC-level features (QPACK compression, 0-RTT
// connection resumption, connection migration), use net.http.v3 and
// net.quic directly for protocol-level access.

module main

import net.http

fn main() {
	println('=== HTTP/3 Features via Unified API ===\n')

	// Demo 1: Alt-Svc based HTTP/3 discovery
	demo_alt_svc_discovery()

	// Demo 2: Reusable client with protocol upgrade
	demo_client_upgrade()

	// Demo 3: Multi-protocol server configuration
	demo_multi_protocol_server()

	println('\n=== Demo Complete ===')
}

fn demo_alt_svc_discovery() {
	println('Demo 1: Alt-Svc HTTP/3 Discovery')
	println('==================================================')

	// The Alt-Svc cache stores HTTP/3 service discovery information.
	// When a server responds with Alt-Svc headers advertising h3 support,
	// subsequent requests can automatically upgrade to HTTP/3.
	cache := http.new_alt_svc_cache()
	println('  Created Alt-Svc cache for HTTP/3 discovery')

	// Use FetchConfig with Alt-Svc cache
	response := http.fetch(
		url:    'https://cloudflare-quic.com/'
		method: .get
		header: http.new_header_from_map({
			.user_agent: 'V-HTTP3-Client/1.0'
			.accept:     'text/html'
		})
		alt_svc_cache: cache
	) or {
		println('  Request: ${err}')
		println('  (Network connectivity required for live demo)')
		println('')
		return
	}

	println('  Status: ${response.status_code}')
	alt_svc := response.header.get_custom('alt-svc') or { 'not present' }
	println('  Alt-Svc header: ${alt_svc}')
	println('  Subsequent requests may use HTTP/3 if Alt-Svc advertises h3')
	println('')
}

fn demo_client_upgrade() {
	println('Demo 2: Reusable Client with Protocol Upgrade')
	println('==================================================')

	// http.Client maintains a shared Alt-Svc cache across requests.
	// This allows automatic HTTP/3 upgrade after discovery.
	mut client := http.new_client()
	println('  Created reusable HTTP client')

	// First request — discovers HTTP/3 support
	println('  Request 1 (initial, discovers available protocols)...')
	resp1 := client.get('https://cloudflare-quic.com/') or {
		println('    Result: ${err}')
		println('    (Network connectivity required)')
		println('')
		return
	}
	println('    Status: ${resp1.status_code}')

	// Second request — may automatically use HTTP/3
	println('  Request 2 (may use HTTP/3 via cached Alt-Svc)...')
	resp2 := client.get('https://cloudflare-quic.com/') or {
		println('    Result: ${err}')
		println('')
		return
	}
	println('    Status: ${resp2.status_code}')
	println('  Protocol upgrade is transparent to the application')
	println('')
}

fn demo_multi_protocol_server() {
	println('Demo 3: Multi-Protocol Server Configuration')
	println('==================================================')

	// The unified http.Server can serve all protocols simultaneously.
	// This demonstrates the configuration — actual serving requires
	// TLS certificates and is typically started with listen_and_serve_all().
	server := http.Server{
		addr:      '0.0.0.0:8080'
		tls_addr:  ':8443'
		h3_addr:   ':8443'
		cert_file: 'cert.pem'
		key_file:  'key.pem'
		enable_h3: true
	}

	println('  Server configured for multi-protocol serving:')
	println('    HTTP/1.1:  ${server.addr} (plain TCP)')
	println('    HTTP/2:    ${server.tls_addr} (TLS with ALPN h2)')
	println('    HTTP/3:    ${server.h3_addr} (QUIC/UDP)')
	println('    H3 enabled: ${server.enable_h3}')
	println('')
	println('  Start with: server.listen_and_serve_all()')
	println('  This uses a single ServerHandler for all protocols.')
	println('')
	println('  For advanced HTTP/3 features, use directly:')
	println('    net.http.v3 — QPACK compression, stream management')
	println('    net.quic    — 0-RTT resumption, connection migration,')
	println('                  path validation, session tickets')
}
