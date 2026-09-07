// vtest build: present_openssl?
module openssl

import net

fn test_incomplete_client_credentials_use_nonretryable_net_code() {
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

fn test_in_memory_tls_file_write_failure_uses_nonretryable_net_code() {
	write_in_memory_tls_file('/path/that/does/not/exist/vlang-openssl-ca.pem', 'CA data', 'CA bundle') or {
		assert err.code() == net.err_tls_certificate_invalid_code
		assert err.msg().contains('failed to write in-memory CA bundle')
		return
	}
	assert false, 'expected writing an in-memory TLS file to fail'
}
