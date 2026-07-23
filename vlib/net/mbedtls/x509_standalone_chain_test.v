module mbedtls

import encoding.base64

// Duplicated locally (rather than referenced from x509_standalone_test.v)
// deliberately -- V's single-file test compilation only pulls in regular
// (non-_test.v) sibling files, never another _test.v file's symbols, so
// each _test.v file needs to be self-contained regardless of what's
// already defined elsewhere in the same module.
const standalone_test_cert_local = '-----BEGIN CERTIFICATE-----\nMIIEOTCCAyECFG64Q2g46jZb3kRbDOJWX/BwjSp6MA0GCSqGSIb3DQEBCwUAMEUx\nCzAJBgNVBAYTAkFVMRMwEQYDVQQIDApTb21lLVN0YXRlMSEwHwYDVQQKDBhJbnRl\ncm5ldCBXaWRnaXRzIFB0eSBMdGQwIBcNMjMwODAyMTcyOTQyWhgPMjA1MDEyMTcx\nNzI5NDJaMGsxCzAJBgNVBAYTAlVTMRMwEQYDVQQIDApDYWxpZm9ybmlhMRQwEgYD\nVQQHDAtMb3MgQW5nZWxlczEdMBsGA1UECgwUQ2F0YWx5c3QgRGV2ZWxvcG1lbnQx\nEjAQBgNVBAMMCWxvY2FsaG9zdDCCAiIwDQYJKoZIhvcNAQEBBQADggIPADCCAgoC\nggIBALqAI4fqUi+QBVWcsXglouLdOML5+w0+1hSR1KdO0Q5XPdQAs/yYWJ+KUkDw\nG++rfy9DUPq7FNRBVurXQkcAtn6gXdllGUSjwUiDo/N4mMOyS/2sufBuaeww7jVi\nrppH+zwP1tUnjRd6khl6bi1Ian9VSzr3Iy9CkXIg1GU4CPXkOydLeoQfepXxWoK1\nOUNwT3VKC/stAfY3j/NIIeiJYkyuRGFCkxn/BUjN+AsXiTugRcYKEFHdIPkOuCXp\nYbhf+lLsczpxCs3rdZG9b/N6mEDCzXTmeHkmsjdPTf+1k5DZZvKzVBBrgdxCgBb7\n5RwjF5v9WmnIc33wWgfJC6FaUzj9NYxYUbPHD+jTz0rJB/jj4u/xJlM/e5NRmXdW\n70pOMKXtWjRSolLOFIPKLY1qs3KMTAZxKKWPDDF7WlMJxMRt7nnnks5yw43Nog4C\njDLk1ZgETnPpLgo3jbmJdIv+OHKTJrBlVvDq7VTyixCoS5G8KoOmyQJhaXG6NwE2\niVhH5JIKgzgCfetfDsnjxqJ/qtrFXPa8FF2TsomD0NK/GZmIcs+9OeVB75Jn5uhF\nfLHScpiTbuu5w3P/LI/MqihLRB6RRNnRzPH8fIg5bYC9b770ta/8GcFRuYE8t+UR\nGtqXJoIKixbDlqV54kal8FQzYzhETf9+NM6Kb/lKEfG/pslvAgMBAAEwDQYJKoZI\nhvcNAQELBQADggEBALI3uNiNO0QE1brA3QYFK+d9ZroB72NrJ0UNkzYHDg2Fc6xg\n4aVVfaxY08+TmKc0JlMOW+pUxeCW/+UBSngdQiR9EE9xm0k0XIrAsy9RXxRvEtPu\nM1VI2h7ayp1Y2BrnQinevTSgtqLRyS1VbOFRl1FiyVvinw2I0KsDdAMNevAPXcOa\nQ8pUgUq6f56DkhocQaj+hxD/uV8HryNxuoSXnPhvfTN3z4YRGzsaWevJ9EYJliOM\n+XugcqfFJ+W7/QCEcAHCL+Bw6OydG5NFORr3p57PXjjcL/uKmxPBrWg2Bz6uT4uR\nMhj0zttiFHLAt9jGfyk6W57UNUja1e1ggftJJhs=\n-----END CERTIFICATE-----\n'

// pem_to_der extracts the DER bytes from a single-certificate PEM block.
// build_certificate_chain takes DER, not PEM (see its own doc comment for
// why the two are not interchangeable here), so tests need real DER
// input, not the PEM string this file's own local constant provides.
fn pem_to_der(pem string) []u8 {
	body :=
		pem.replace('-----BEGIN CERTIFICATE-----', '').replace('-----END CERTIFICATE-----', '').replace('\n', '').trim_space()
	return base64.decode(body)
}

fn test_build_certificate_chain_single_cert_then_verify_fails_not_a_ca() {
	der := pem_to_der(standalone_test_cert_local)
	chain := build_certificate_chain([der])!
	defer {
		free_certificate_chain(chain)
	}
	// Same real-world answer as x509_standalone_test.v's own direct
	// C.mbedtls_x509_crt_verify call against this same cert: it has no
	// CA:TRUE basic constraint (it's a leaf server cert), so verifying it
	// against itself as the sole trust anchor must fail with "not
	// trusted", not succeed and not crash. This proves
	// build_certificate_chain's parsed chain behaves identically to a
	// directly-constructed one -- the wrapper isn't silently producing a
	// differently-shaped chain.
	verify_certificate_chain(chain, standalone_test_cert_local) or {
		assert err.msg().contains('verification failed')
		return
	}
	assert false, 'expected verification to fail: standalone_test_cert has no CA:TRUE basic constraint'
}

fn test_build_certificate_chain_rejects_empty_list() {
	build_certificate_chain([][]u8{}) or {
		assert err.msg().contains('empty')
		return
	}
	assert false, 'expected an error for an empty certificate list'
}

fn test_build_certificate_chain_rejects_malformed_der() {
	build_certificate_chain([[u8(0xff), 0xff, 0xff, 0xff]]) or { return }
	assert false, 'expected an error for malformed DER data'
}

fn test_verify_certificate_chain_rejects_unparseable_ca_bundle() {
	der := pem_to_der(standalone_test_cert_local)
	chain := build_certificate_chain([der])!
	defer {
		free_certificate_chain(chain)
	}
	verify_certificate_chain(chain, 'not a real PEM bundle') or {
		assert err.msg().contains('failed to parse CA bundle')
		return
	}
	assert false, 'expected an error for an unparseable CA bundle'
}
