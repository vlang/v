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

fn test_incomplete_client_credentials_are_nonretryable() {
	new_ssl_conn(SSLConnectConfig{
		cert: 'unused because the pair is incomplete'
		validate: false
		in_memory_verification: true
	}) or {
		assert err.code() == net.err_tls_certificate_invalid_code
		assert err.msg().contains('both cert and cert_key are required')
		return
	}
	assert false, 'expected an incomplete client certificate pair to be rejected'
}

fn test_client_default_ca_bundle_rejects_partial_parse() {
	mut cacert := C.mbedtls_x509_crt{}
	C.mbedtls_x509_crt_init(&cacert)
	defer {
		C.mbedtls_x509_crt_free(&cacert)
	}
	partially_malformed_bundle := default_ca_bundle_pem + '\n-----BEGIN CERTIFICATE-----\nAAAA\n-----END CERTIFICATE-----\n'
	parse_client_ca_bundle(&cacert, partially_malformed_bundle, 'system/default CA bundle') or {
		assert err.code() == net.err_tls_certificate_invalid_code
		assert err.msg().contains('system/default CA bundle')
		assert err.msg().contains('mbedtls ret: 1'), 'expected the positive skipped-certificate count in the message: ${err.msg()}'
		return
	}
	assert false, 'expected a partially malformed default CA bundle to be rejected'
}
