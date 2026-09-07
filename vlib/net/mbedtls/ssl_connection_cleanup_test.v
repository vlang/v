module mbedtls

fn test_unconnected_ssl_conn_shutdown_is_safe_and_idempotent() {
	mut conn := new_ssl_conn(validate: false)!
	conn.shutdown()!
	conn.shutdown()!
	assert conn.cleanup_done
}
