module mbedtls

import net

fn test_unconnected_ssl_conn_shutdown_is_safe_and_idempotent() {
	mut conn := new_ssl_conn(validate: false)!
	conn.shutdown()!
	conn.shutdown()!
	assert conn.cleanup_done
}

fn test_client_certificate_failure_uses_nonretryable_net_code() {
	err := mbedtls_client_handshake_error('certificate rejected', C.MBEDTLS_ERR_X509_CERT_VERIFY_FAILED)
	assert err.code() == net.err_tls_certificate_invalid_code
	assert err.msg() == 'certificate rejected'
}

fn test_client_non_certificate_failure_keeps_mbedtls_code() {
	err := mbedtls_client_handshake_error('handshake timed out', C.MBEDTLS_ERR_SSL_TIMEOUT)
	assert err.code() == C.MBEDTLS_ERR_SSL_TIMEOUT
	assert err.msg() == 'handshake timed out'
}
