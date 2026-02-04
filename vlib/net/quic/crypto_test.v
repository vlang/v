module quic

import os

// test_load_certificate_file_not_found tests error when file doesn't exist
fn test_load_certificate_file_not_found() {
	result := load_certificate('/nonexistent/cert.pem') or {
		assert err.msg().contains('not found')
		return
	}
	assert false, 'Should have returned error for nonexistent file'
}

// test_load_private_key_file_not_found tests error when file doesn't exist
fn test_load_private_key_file_not_found() {
	result := load_private_key('/nonexistent/key.pem') or {
		assert err.msg().contains('not found')
		return
	}
	assert false, 'Should have returned error for nonexistent file'
}

// test_load_certificate_invalid_format tests error for non-PEM files
fn test_load_certificate_invalid_format() {
	// Create a temporary file with invalid content
	temp_file := os.join_path(os.temp_dir(), 'test_invalid_cert.pem')
	os.write_file(temp_file, 'This is not a valid PEM certificate') or {
		assert false, 'Failed to create temp file'
		return
	}
	defer {
		os.rm(temp_file) or {}
	}

	result := load_certificate(temp_file) or {
		assert err.msg().contains('invalid PEM format')
		return
	}
	assert false, 'Should have returned error for invalid PEM format'
}

// test_load_private_key_invalid_format tests error for non-PEM files
fn test_load_private_key_invalid_format() {
	temp_file := os.join_path(os.temp_dir(), 'test_invalid_key.pem')
	os.write_file(temp_file, 'This is not a valid PEM private key') or {
		assert false, 'Failed to create temp file'
		return
	}
	defer {
		os.rm(temp_file) or {}
	}

	result := load_private_key(temp_file) or {
		assert err.msg().contains('invalid PEM format')
		return
	}
	assert false, 'Should have returned error for invalid PEM format'
}

// test_load_certificate_valid_pem tests loading a valid PEM certificate
fn test_load_certificate_valid_pem() {
	temp_file := os.join_path(os.temp_dir(), 'test_valid_cert.pem')
	valid_cert := '-----BEGIN CERTIFICATE-----
MIICXDCCAcWgAwIBAgIBADANBgkqhkiG9w0BAQsFADCBgzELMAkGA1UEBhMCVVMx
CzAJBgNVBAgMAkNBMRYwFAYDVQQHDA1TYW4gRnJhbmNpc2NvMRMwEQYDVQQKDApN
eSBDb21wYW55MRMwEQYDVQQLDApNeSBEaXZpc2lvbjElMCMGA1UEAwwcdGVzdC5l
eGFtcGxlLmNvbSBbVEVTVCBPTkxZXTAeFw0yNDAyMDMwMDAwMDBaFw0yNTAyMDMw
-----END CERTIFICATE-----'

	os.write_file(temp_file, valid_cert) or {
		assert false, 'Failed to create temp file'
		return
	}
	defer {
		os.rm(temp_file) or {}
	}

	result := load_certificate(temp_file) or {
		assert false, 'Failed to load valid certificate: ${err}'
		return
	}

	assert result.len > 0, 'Certificate data should not be empty'
	assert result.bytestr().contains('BEGIN CERTIFICATE')
	println('✓ Valid certificate loading test passed')
}

// test_load_private_key_valid_pem tests loading various valid PEM private key formats
fn test_load_private_key_valid_pem() {
	// Test RSA private key format
	temp_file := os.join_path(os.temp_dir(), 'test_valid_rsa_key.pem')
	valid_rsa_key := '-----BEGIN RSA PRIVATE KEY-----
MIIEpAIBAAKCAQEAu7jSEqUfWxJD8jMpUJZVkXLfPNvE8gvJYXcGXMhTqHQpZTgO
8F2hLfLwNqfVd7wkX9cpVL/5BvXzQJXQPfKlGJQP8lbwEYBT3U6kQZF9F/uKLBsI
-----END RSA PRIVATE KEY-----'

	os.write_file(temp_file, valid_rsa_key) or {
		assert false, 'Failed to create temp file'
		return
	}
	defer {
		os.rm(temp_file) or {}
	}

	result := load_private_key(temp_file) or {
		assert false, 'Failed to load valid RSA private key: ${err}'
		return
	}
	assert result.len > 0
	assert result.bytestr().contains('BEGIN RSA PRIVATE KEY')

	// Test EC private key format
	temp_file2 := os.join_path(os.temp_dir(), 'test_valid_ec_key.pem')
	valid_ec_key := '-----BEGIN EC PRIVATE KEY-----
MHcCAQEEIKbFObJ8iJR7LVQx1vXQGH3cXZLKlEzXMKfZwXNXH8XwoAoGCCqGSM49
AwEHoUQDQgAE8LJvXl/Fz8HwVgJTQPPZxDz8EhZ8Y8CLXWK3sxdZaV8KZnBPVB4Z
-----END EC PRIVATE KEY-----'

	os.write_file(temp_file2, valid_ec_key) or {
		assert false, 'Failed to create temp file'
		return
	}
	defer {
		os.rm(temp_file2) or {}
	}

	result2 := load_private_key(temp_file2) or {
		assert false, 'Failed to load valid EC private key: ${err}'
		return
	}
	assert result2.len > 0
	assert result2.bytestr().contains('BEGIN EC PRIVATE KEY')

	// Test generic PRIVATE KEY format
	temp_file3 := os.join_path(os.temp_dir(), 'test_valid_key.pem')
	valid_key := '-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQC7uNISpR9bEkPy
MylQllWRct8828TyC8lhdwZcyFOodCllOA7wXaEt8vA2p9V3vCRf1ylUv/kG9fNA
-----END PRIVATE KEY-----'

	os.write_file(temp_file3, valid_key) or {
		assert false, 'Failed to create temp file'
		return
	}
	defer {
		os.rm(temp_file3) or {}
	}

	result3 := load_private_key(temp_file3) or {
		assert false, 'Failed to load valid private key: ${err}'
		return
	}
	assert result3.len > 0
	assert result3.bytestr().contains('BEGIN PRIVATE KEY')

	println('✓ Valid private key loading tests passed (RSA, EC, and generic formats)')
}
