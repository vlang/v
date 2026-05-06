module quic

import os

// PEM file loading utilities for TLS certificates and private keys.

// load_certificate loads a certificate from a PEM file
pub fn load_certificate(path string) ![]u8 {
	if !os.exists(path) {
		return error('certificate file not found: ${path}')
	}

	data := os.read_file(path) or { return error('failed to read certificate file: ${err}') }

	if !data.contains('-----BEGIN CERTIFICATE-----') {
		return error('invalid PEM format: missing BEGIN CERTIFICATE marker')
	}

	if !data.contains('-----END CERTIFICATE-----') {
		return error('invalid PEM format: missing END CERTIFICATE marker')
	}

	return data.bytes()
}

// load_private_key loads a private key from a PEM file
pub fn load_private_key(path string) ![]u8 {
	if !os.exists(path) {
		return error('private key file not found: ${path}')
	}

	data := os.read_file(path) or { return error('failed to read private key file: ${err}') }

	has_rsa := data.contains('-----BEGIN RSA PRIVATE KEY-----')
	has_ec := data.contains('-----BEGIN EC PRIVATE KEY-----')
	has_private := data.contains('-----BEGIN PRIVATE KEY-----')

	if !has_rsa && !has_ec && !has_private {
		return error('invalid PEM format: missing BEGIN PRIVATE KEY marker')
	}

	has_end_rsa := data.contains('-----END RSA PRIVATE KEY-----')
	has_end_ec := data.contains('-----END EC PRIVATE KEY-----')
	has_end_private := data.contains('-----END PRIVATE KEY-----')

	if !has_end_rsa && !has_end_ec && !has_end_private {
		return error('invalid PEM format: missing END PRIVATE KEY marker')
	}

	return data.bytes()
}
