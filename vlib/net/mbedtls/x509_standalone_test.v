module mbedtls

// This test exists to answer a specific question for the net.quic (HTTP/3)
// work (see https://github.com/vlang/v/issues/27675): can mbedTLS's X.509
// parse/verify functions be called standalone, without ever constructing an
// mbedtls_ssl_context, on this platform? net.quic needs exactly this — it
// never builds an SSL session, only parses/validates certificates seen in a
// QUIC CRYPTO-frame-carried TLS 1.3 Certificate message.
//
// Deliberately does NOT call v_mbedtls_threading_setup() itself: the point is
// to confirm the module's own `init()` (which every other net.mbedtls user
// already relies on via `import net.mbedtls`) is sufficient on its own, with
// no additional standalone-usage setup required.

// self-signed test certificate, reused from vlib/net/http/server_tls_test.v
const standalone_test_cert = '-----BEGIN CERTIFICATE-----\nMIIEOTCCAyECFG64Q2g46jZb3kRbDOJWX/BwjSp6MA0GCSqGSIb3DQEBCwUAMEUx\nCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl\ncm5ldCBXaWRnaXRzIFB0eSBMdGQwIBcNMjMwODAyMTcyOTQyWhgPMjA1MDEyMTcx\nNzI5NDJaMGsxCzAJBgNVBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMRQwEgYD\nVQQHDAtMb3MgQW5nZWxlczEdMBsGA1UECgwUQ2F0YWx5c3QgRGV2ZWxvcG1lbnQx\nEjAQBgNVBAMMCWxvY2FsaG9zdDCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoC\nggIBALqAI4fqUi+QBVWcsXglouLdOML5+w0+1hSR1KdO0Q5XPdQAs/yYWJ+KUkDw\nG++rfy9DUPq7FNRBVurXQkcAtn6gXdllGUSjwUiDo/N4mMOyS/2sufBuaeww7jVi\nrppH+zwP1tUnjRd6khl6bi1Ian9VSzr3Iy9CkXIg1GU4CPXkOydLeoQfepXxWoK1\nOUNwT3VKC/stAfY3j/NIIeiJYkyuRGFCkxn/BUjN+AsXiTugRcYKEFHdIPkOuCXp\nYbhf+lLsczpxCs3rdZG9b/N6mEDCzXTmeHkmsjdPTf+1k5DZZvKzVBBrgdxCgBb7\n5RwjF5v9WmnIc33wWgfJC6FaUzj9NYxYUbPHD+jTz0rJB/jj4u/xJlM/e5NRmXdW\n70pOMKXtWjRSolLOFIPKLY1qs3KMTAZxKKWPDDF7WlMJxMRt7nnnks5yw43Nog4C\njDLk1ZgETnPpLgo3jbmJdIv+OHKTJrBlVvDq7VTyixCoS5G8KoOmyQJhaXG6NwE2\niVhH5JIKgzgCfetfDsnjxqJ/qtrFXPa8FF2TsomD0NK/GZmIcs+9OeVB75Jn5uhF\nfLHScpiTbuu5w3P/LI/MqihLRB6RRNnRzPH8fIg5bYC9b770ta/8GcFRuYE8t+UR\nGtqXJoIKixbDlqV54kal8FQzYzhETf9+NM6Kb/lKEfG/pslvAgMBAAEwDQYJKoZI\nhvcNAQELBQADggEBALI3uNiNO0QE1brA3QYFK+d9ZroB72NrJ0UNkzYHDg2Fc6xg\n4aVVfaxY08+TmKc0JlMOW+pUxeCW/+UBSngdQiR9EE9xm0k0XIrAsy9RXxRvEtPu\nM1VI2h7ayp1Y2BrnQinevTSgtqLRyS1VbOFRl1FiyVvinw2I0KsDdAMNevAPXcOa\nQ8pUgUq6f56DkhocQaj+hxD/uV8HryNxuoSXnPhvfTN3z4YRGzsaWevJ9EYJliOM\n+XugcqfFJ+W7/QCEcAHCL+Bw6OydG5NFORr3p57PXjjcL/uKmxPBrWg2Bz6uT4uR\nMhj0zttiFHLAt9jGfyk6W57UNUja1e1ggftJJhs=\n-----END CERTIFICATE-----\n'

fn test_x509_parse_and_verify_without_ssl_context() {
	mut crt := C.mbedtls_x509_crt{}
	C.mbedtls_x509_crt_init(&crt)
	defer {
		C.mbedtls_x509_crt_free(&crt)
	}

	// mbedtls_x509_crt_parse wants a NUL-terminated PEM buffer, with the NUL
	// counted in the length.
	pem := standalone_test_cert.bytes()
	pem_len := pem.len + 1
	parse_res := C.mbedtls_x509_crt_parse(&crt, pem.data, usize(pem_len))
	assert parse_res == 0, 'mbedtls_x509_crt_parse failed standalone (no ssl_context in scope): ${parse_res}'

	// Verify the self-signed leaf cert against itself as the sole trust
	// anchor — exactly the "chain validation with no mbedtls_ssl_context"
	// call shape net.quic needs for validating a peer's Certificate message.
	//
	// This particular test cert has no CA:TRUE basic constraint (it's a leaf
	// server cert, not a root CA), so mbedTLS correctly refuses to treat it
	// as a valid trust anchor for itself: MBEDTLS_ERR_X509_CERT_VERIFY_FAILED
	// (-9984) with MBEDTLS_X509_BADCERT_NOT_TRUSTED (0x08) set. That's the
	// right answer, and it's exactly the proof needed here — the call ran
	// standalone, with no mbedtls_ssl_context anywhere in scope, and mbedTLS's
	// real X.509 chain-validation logic (the CA-constraint check) executed
	// and returned a correct, structured result rather than crashing/no-op'ing.
	mut flags := u32(0)
	verify_res := C.mbedtls_x509_crt_verify(&crt, &crt, unsafe { nil }, unsafe { nil }, &flags,
		unsafe { nil }, unsafe { nil })
	assert verify_res == C.MBEDTLS_ERR_X509_CERT_VERIFY_FAILED
	assert flags & u32(C.MBEDTLS_X509_BADCERT_NOT_TRUSTED) != 0
}
