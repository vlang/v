module mbedtls

fn test_shutdown_releases_unconnected_client_context() ! {
	mut conn := new_ssl_conn(alpn_protocols: ['http/1.1'])!
	assert conn.alpn_list != unsafe { nil }
	conn.shutdown()!
	assert !conn.opened
	assert conn.alpn_list == unsafe { nil }
	mut rejected := false
	conn.shutdown() or {
		rejected = true
		assert err.msg().contains('connection was not open')
	}
	assert rejected
}
