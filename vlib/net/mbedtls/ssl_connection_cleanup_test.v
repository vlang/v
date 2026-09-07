module mbedtls

import net
import os
import rand

fn test_unconnected_ssl_conn_shutdown_is_safe_and_idempotent() {
	mut conn := new_ssl_conn(validate: false)!
	conn.shutdown()!
	conn.shutdown()!
	assert conn.cleanup_done
}

fn test_failed_dial_frees_alpn_storage_before_marking_cleanup_done() {
	mut conn := new_ssl_conn(
		validate: false
		alpn_protocols: ['h2', 'http/1.1']
	)!
	assert conn.alpn_list != unsafe { nil }
	conn.dial('127.0.0.1', 0) or {
		assert conn.cleanup_done
		assert conn.alpn_list == unsafe { nil }
		conn.shutdown()!
		return
	}
	assert false, 'expected a TLS dial to port 0 to fail'
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

// test_client_ca_file_rejects_empty_file is the h1/h2 sibling of HTTP/3's
// own regression test for the same class of finding (external review,
// vlang/v#28406, issuecomment-5572430232): an explicitly configured but
// empty CA file must be rejected, not silently treated as "no CA
// configured". Unlike HTTP/3 (which reads the file into a string before
// net.quic ever sees it, so an empty read result is ambiguous with an
// unset field), this path hands the file PATH straight to mbedTLS's own
// `mbedtls_x509_crt_parse_file`, which parse_client_ca_file already
// treats as a hard failure on any nonzero return -- this test empirically
// confirms mbedTLS itself returns nonzero for a genuinely empty (0-byte)
// file, rather than assuming it from reading the C API's documentation.
fn test_client_ca_file_rejects_empty_file() {
	workdir := os.join_path(os.vtmp_dir(), 'v_mbedtls_empty_ca_${rand.ulid()}')
	os.mkdir_all(workdir) or { panic(err) }
	defer {
		os.rmdir_all(workdir) or {}
	}
	empty_ca_path := os.join_path(workdir, 'empty-ca.pem')
	os.write_file(empty_ca_path, '') or { panic(err) }

	new_ssl_conn(SSLConnectConfig{
		verify: empty_ca_path
		validate: true
	}) or {
		assert err.code() == net.err_tls_certificate_invalid_code
		assert err.msg().contains('failed to parse configured CA file')
		return
	}
	assert false, 'expected an empty configured CA file to be rejected'
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
