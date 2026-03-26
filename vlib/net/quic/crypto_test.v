// Tests for QUIC crypto operations.
module quic

import os

fn test_load_certificate_file_not_found() {
	result := load_certificate('/nonexistent/cert.pem') or {
		assert err.msg().contains('not found')
		return
	}
	assert false, 'Should have returned error for nonexistent file'
}

fn test_load_private_key_file_not_found() {
	result := load_private_key('/nonexistent/key.pem') or {
		assert err.msg().contains('not found')
		return
	}
	assert false, 'Should have returned error for nonexistent file'
}

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

fn test_derive_traffic_keys_produces_correct_lengths() {
	// Derive initial secrets from a known DCID as key material
	dcid := [u8(0x83), 0x94, 0xc8, 0xf0, 0x3e, 0x51, 0x57, 0x08]
	tx_secret, rx_secret := derive_initial_secrets(dcid, true) or {
		assert false, 'derive_initial_secrets failed: ${err}'
		return
	}

	mut ctx := CryptoContext{
		tx_secret:     tx_secret
		rx_secret:     rx_secret
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
	defer {
		C.EVP_CIPHER_CTX_free(ctx.tx_cipher_ctx)
		C.EVP_CIPHER_CTX_free(ctx.rx_cipher_ctx)
	}

	ctx.derive_traffic_keys() or {
		assert false, 'derive_traffic_keys failed: ${err}'
		return
	}

	assert ctx.tx_key.len == 16, 'tx_key must be 16 bytes (AES-128), got ${ctx.tx_key.len}'
	assert ctx.rx_key.len == 16, 'rx_key must be 16 bytes (AES-128), got ${ctx.rx_key.len}'
	assert ctx.tx_iv.len == 12, 'tx_iv must be 12 bytes (GCM nonce), got ${ctx.tx_iv.len}'
	assert ctx.rx_iv.len == 12, 'rx_iv must be 12 bytes (GCM nonce), got ${ctx.rx_iv.len}'
}

fn test_derive_traffic_keys_deterministic() {
	dcid := [u8(0x01), 0x02, 0x03, 0x04]
	tx_secret, rx_secret := derive_initial_secrets(dcid, false) or {
		assert false, 'derive_initial_secrets failed: ${err}'
		return
	}

	mut ctx1 := CryptoContext{
		tx_secret:     tx_secret
		rx_secret:     rx_secret
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
	defer {
		C.EVP_CIPHER_CTX_free(ctx1.tx_cipher_ctx)
		C.EVP_CIPHER_CTX_free(ctx1.rx_cipher_ctx)
	}
	ctx1.derive_traffic_keys() or {
		assert false, 'first derive_traffic_keys failed: ${err}'
		return
	}

	mut ctx2 := CryptoContext{
		tx_secret:     tx_secret
		rx_secret:     rx_secret
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
	defer {
		C.EVP_CIPHER_CTX_free(ctx2.tx_cipher_ctx)
		C.EVP_CIPHER_CTX_free(ctx2.rx_cipher_ctx)
	}
	ctx2.derive_traffic_keys() or {
		assert false, 'second derive_traffic_keys failed: ${err}'
		return
	}

	assert ctx1.tx_key == ctx2.tx_key, 'tx_key derivation must be deterministic'
	assert ctx1.tx_iv == ctx2.tx_iv, 'tx_iv derivation must be deterministic'
	assert ctx1.rx_key == ctx2.rx_key, 'rx_key derivation must be deterministic'
	assert ctx1.rx_iv == ctx2.rx_iv, 'rx_iv derivation must be deterministic'
}

fn test_derive_traffic_keys_empty_secret_errors() {
	mut ctx := CryptoContext{
		tx_secret:     []u8{}
		rx_secret:     []u8{len: 32, init: 0xaa}
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
	defer {
		C.EVP_CIPHER_CTX_free(ctx.tx_cipher_ctx)
		C.EVP_CIPHER_CTX_free(ctx.rx_cipher_ctx)
	}

	ctx.derive_traffic_keys() or {
		assert err.msg().contains('secret'), 'error should mention secret: ${err}'
		return
	}
	assert false, 'derive_traffic_keys should error on empty tx_secret'
}

fn test_encrypt_decrypt_roundtrip_with_derived_keys() {
	dcid := [u8(0x83), 0x94, 0xc8, 0xf0, 0x3e, 0x51, 0x57, 0x08]

	// Server: tx_secret = server_secret, rx_secret = client_secret
	server_tx, server_rx := derive_initial_secrets(dcid, true) or {
		assert false, 'derive_initial_secrets (server) failed: ${err}'
		return
	}

	// Client: tx_secret = client_secret, rx_secret = server_secret
	client_tx, client_rx := derive_initial_secrets(dcid, false) or {
		assert false, 'derive_initial_secrets (client) failed: ${err}'
		return
	}

	// server_tx == client_rx, server_rx == client_tx
	assert server_tx == client_rx, 'server tx_secret must equal client rx_secret'

	mut server_ctx := CryptoContext{
		tx_secret:     server_tx
		rx_secret:     server_rx
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
	defer {
		C.EVP_CIPHER_CTX_free(server_ctx.tx_cipher_ctx)
		C.EVP_CIPHER_CTX_free(server_ctx.rx_cipher_ctx)
	}
	server_ctx.derive_traffic_keys() or {
		assert false, 'server derive_traffic_keys failed: ${err}'
		return
	}

	mut client_ctx := CryptoContext{
		tx_secret:     client_tx
		rx_secret:     client_rx
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
	defer {
		C.EVP_CIPHER_CTX_free(client_ctx.tx_cipher_ctx)
		C.EVP_CIPHER_CTX_free(client_ctx.rx_cipher_ctx)
	}
	client_ctx.derive_traffic_keys() or {
		assert false, 'client derive_traffic_keys failed: ${err}'
		return
	}

	// Server encrypts → client decrypts
	plaintext := 'Hello QUIC from server'.bytes()
	pkt_num := u64(42)

	encrypted := server_ctx.encrypt_packet(plaintext, []u8{}, server_ctx.tx_iv, pkt_num) or {
		assert false, 'encrypt_packet failed: ${err}'
		return
	}
	assert encrypted.len > plaintext.len, 'ciphertext must be larger than plaintext (GCM tag)'

	decrypted := client_ctx.decrypt_packet(encrypted, []u8{}, client_ctx.rx_iv, pkt_num) or {
		assert false, 'decrypt_packet failed: ${err}'
		return
	}
	assert decrypted == plaintext, 'round-trip must recover original plaintext'
}

fn test_extract_packet_number_short_header_1byte() {
	dcid_len := 8
	// Short header: 0b0100_0000 (form=0, fixed=1, pn_len bits = 0b00 → 1 byte PN)
	mut pkt := []u8{len: 1 + dcid_len + 1}
	pkt[0] = 0x40 // short header, pn_length_bits = 0 → PN len = 1
	// Fill DCID with dummy bytes
	for i in 0 .. dcid_len {
		pkt[1 + i] = u8(0xAA)
	}
	pkt[1 + dcid_len] = 0x07 // packet number = 7

	pn, pn_len := extract_packet_number(pkt, dcid_len) or {
		assert false, 'extract_packet_number failed: ${err}'
		return
	}
	assert pn_len == 1, 'expected pn_len 1, got ${pn_len}'
	assert pn == 7, 'expected packet_number 7, got ${pn}'
}

fn test_extract_packet_number_short_header_2byte() {
	dcid_len := 4
	// pn_length_bits = 0b01 → PN len = 2
	mut pkt := []u8{len: 1 + dcid_len + 2}
	pkt[0] = 0x41 // 0b0100_0001
	for i in 0 .. dcid_len {
		pkt[1 + i] = u8(0xBB)
	}
	// PN = 0x0102 = 258
	pkt[1 + dcid_len] = 0x01
	pkt[1 + dcid_len + 1] = 0x02

	pn, pn_len := extract_packet_number(pkt, dcid_len) or {
		assert false, 'extract_packet_number failed: ${err}'
		return
	}
	assert pn_len == 2, 'expected pn_len 2, got ${pn_len}'
	assert pn == 258, 'expected packet_number 258, got ${pn}'
}

fn test_extract_packet_number_short_header_4byte() {
	dcid_len := 8
	// pn_length_bits = 0b11 → PN len = 4
	mut pkt := []u8{len: 1 + dcid_len + 4}
	pkt[0] = 0x43 // 0b0100_0011
	for i in 0 .. dcid_len {
		pkt[1 + i] = u8(0xCC)
	}
	// PN = 0x00010203 = 66051
	pkt[1 + dcid_len] = 0x00
	pkt[1 + dcid_len + 1] = 0x01
	pkt[1 + dcid_len + 2] = 0x02
	pkt[1 + dcid_len + 3] = 0x03

	pn, pn_len := extract_packet_number(pkt, dcid_len) or {
		assert false, 'extract_packet_number failed: ${err}'
		return
	}
	assert pn_len == 4, 'expected pn_len 4, got ${pn_len}'
	assert pn == 66051, 'expected packet_number 66051, got ${pn}'
}

fn test_extract_packet_number_long_header() {
	dcid_len := 8
	scid_len := 8
	// Long header: 0b1100_0001 (form=1, fixed=1, type=Initial(00), pn_len bits=01 → 2 bytes)
	// Layout: [first_byte][version:4][dcid_len:1][dcid:N][scid_len:1][scid:N][pn:2]
	header_len := 1 + 4 + 1 + dcid_len + 1 + scid_len + 2
	mut pkt := []u8{len: header_len}
	pkt[0] = 0xC1 // 0b1100_0001, pn_length_bits = 0b01 → 2 bytes
	// Version (4 bytes) = 0x00000001 (QUIC v1)
	pkt[1] = 0x00
	pkt[2] = 0x00
	pkt[3] = 0x00
	pkt[4] = 0x01
	// DCID length
	pkt[5] = u8(dcid_len)
	// DCID
	for i in 0 .. dcid_len {
		pkt[6 + i] = u8(0xDD)
	}
	// SCID length
	pkt[6 + dcid_len] = u8(scid_len)
	// SCID
	for i in 0 .. scid_len {
		pkt[7 + dcid_len + i] = u8(0xEE)
	}
	// PN offset = 1 + 4 + 1 + dcid_len + 1 + scid_len = 7 + dcid_len + scid_len
	pn_offset := 7 + dcid_len + scid_len
	// PN = 0x0305 = 773
	pkt[pn_offset] = 0x03
	pkt[pn_offset + 1] = 0x05

	pn, pn_len := extract_packet_number(pkt, dcid_len) or {
		assert false, 'extract_packet_number failed: ${err}'
		return
	}
	assert pn_len == 2, 'expected pn_len 2, got ${pn_len}'
	assert pn == 773, 'expected packet_number 773, got ${pn}'
}

fn test_extract_packet_number_empty_data_errors() {
	extract_packet_number([]u8{}, 0) or {
		assert err.msg().len > 0, 'error should have a message'
		return
	}
	assert false, 'extract_packet_number should error on empty data'
}

fn test_extract_and_unprotect_pn_zero_hp_key_errors() {
	ctx := CryptoContext{
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
	defer {
		C.EVP_CIPHER_CTX_free(ctx.tx_cipher_ctx)
		C.EVP_CIPHER_CTX_free(ctx.rx_cipher_ctx)
	}

	// Build a minimal packet (short header, 8-byte DCID, 1-byte PN, +20 bytes
	// of dummy payload so the sample region is present).
	dcid_len := 8
	mut pkt := []u8{len: 1 + dcid_len + 1 + 20}
	pkt[0] = 0x40

	ctx.extract_and_unprotect_pn(pkt, dcid_len) or {
		assert err.msg().contains('rx_hp_key'), 'error should mention rx_hp_key: ${err}'
		return
	}
	assert false, 'extract_and_unprotect_pn should error when rx_hp_key is empty'
}

fn test_extract_and_unprotect_pn_roundtrip() {
	dcid_len := 8
	pn_offset := 1 + dcid_len // = 9 for short header
	expected_pn := u64(42)

	// Build an unprotected short-header packet:
	// [first_byte:1][dcid:8][pn:1][dummy_payload:20]
	// first_byte = 0x40: short header (bit7=0), fixed bit (bit6=1), pn_len_bits=00 → 1 byte
	total_len := pn_offset + 4 + 16 // pn_offset + max_pn(4) + sample_size(16)
	mut pkt := []u8{len: total_len}
	pkt[0] = 0x40
	// DCID
	for i in 0 .. dcid_len {
		pkt[1 + i] = u8(0xAA)
	}
	// PN = 42 (1 byte)
	pkt[pn_offset] = u8(expected_pn)
	// Fill remaining bytes with dummy payload (used as HP sample source)
	for i in pn_offset + 1 .. total_len {
		pkt[i] = u8(0x55)
	}

	// 16-byte HP key
	hp_key := [u8(0x01), 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c, 0x0d,
		0x0e, 0x0f, 0x10]

	// Compute the mask that HP would apply (sample starts at pn_offset + 4)
	sample := pkt[pn_offset + 4..pn_offset + 4 + 16]
	mask := aes_ecb_encrypt(hp_key, sample) or {
		assert false, 'aes_ecb_encrypt failed: ${err}'
		return
	}

	// Apply HP manually: mask byte 0 (short header → low 5 bits), mask PN bytes
	pkt[0] ^= mask[0] & 0x1f
	pn_len := 1 // we know PN is 1 byte
	for i in 0 .. pn_len {
		pkt[pn_offset + i] ^= mask[1 + i]
	}

	// Now pkt has HP applied. Create CryptoContext with the HP key.
	ctx := CryptoContext{
		rx_hp_key:     hp_key
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
	defer {
		C.EVP_CIPHER_CTX_free(ctx.tx_cipher_ctx)
		C.EVP_CIPHER_CTX_free(ctx.rx_cipher_ctx)
	}

	pn, extracted_pn_len, unprotected_header := ctx.extract_and_unprotect_pn(pkt, dcid_len) or {
		assert false, 'extract_and_unprotect_pn failed: ${err}'
		return
	}

	assert pn == expected_pn, 'expected PN ${expected_pn}, got ${pn}'
	assert extracted_pn_len == 1, 'expected pn_len 1, got ${extracted_pn_len}'
	assert unprotected_header[0] == 0x40, 'first byte should be restored to 0x40, got 0x${unprotected_header[0]:02x}'
	assert unprotected_header[pn_offset] == u8(expected_pn), 'PN byte should be restored'
}
