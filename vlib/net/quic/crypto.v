// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module quic

import os
import crypto.aes

// TLS 1.3 crypto callbacks for ngtcp2
// This implements the cryptographic operations needed for QUIC

#flag -lssl
#flag -lcrypto

#include "@VEXEROOT/vlib/net/quic/quic_stubs.c"
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/kdf.h>
#include <openssl/rand.h>

// OpenSSL types
type SSL = voidptr
type SSL_CTX = voidptr
type SSL_METHOD = voidptr
type EVP_CIPHER = voidptr
type EVP_MD = voidptr
type EVP_CIPHER_CTX = voidptr
type EVP_PKEY_CTX = voidptr

// TLS 1.3 constants
pub const tls1_3_version = 0x0304
pub const ssl_verify_none = 0
pub const ssl_verify_peer = 1

// Crypto level (from ngtcp2)
pub enum CryptoLevel {
	initial     = 0
	handshake   = 1
	application = 2
	early       = 3
}

// CryptoContext holds cryptographic state
pub struct CryptoContext {
pub mut:
	ssl_ctx SSL_CTX
	ssl     SSL
	// Encryption keys
	tx_secret []u8
	rx_secret []u8
	// Cipher contexts
	tx_cipher_ctx EVP_CIPHER_CTX
	rx_cipher_ctx EVP_CIPHER_CTX
	// Header protection
	tx_hp_key []u8
	rx_hp_key []u8
}

// OpenSSL function declarations
fn C.SSL_CTX_new(method &SSL_METHOD) SSL_CTX
fn C.SSL_CTX_free(ctx SSL_CTX)
fn C.SSL_CTX_set_min_proto_version(ctx SSL_CTX, version int) int
fn C.SSL_CTX_set_max_proto_version(ctx SSL_CTX, version int) int
fn C.SSL_CTX_set_alpn_protos(ctx SSL_CTX, protos &u8, protos_len u32) int
fn C.SSL_CTX_set_default_verify_paths(ctx SSL_CTX) int
fn C.SSL_CTX_set_verify(ctx SSL_CTX, mode int, callback voidptr)

fn C.TLS_client_method() &SSL_METHOD
fn C.TLS_server_method() &SSL_METHOD
fn C.SSL_CTX_use_certificate_file(ctx SSL_CTX, file &char, typ int) int
fn C.SSL_CTX_use_PrivateKey_file(ctx SSL_CTX, file &char, typ int) int

// SSL file types
pub const ssl_filetype_pem = 1
pub const ssl_filetype_asn1 = 2

fn C.SSL_new(ctx SSL_CTX) SSL
fn C.SSL_free(ssl SSL)
fn C.SSL_set_connect_state(ssl SSL)
fn C.SSL_set_accept_state(ssl SSL)
fn C.SSL_do_handshake(ssl SSL) int
fn C.SSL_get_error(ssl SSL, ret int) int
fn C.SSL_provide_quic_data(ssl SSL, level int, data &u8, len u64) int
fn C.SSL_process_quic_post_handshake(ssl SSL) int
fn C.SSL_is_init_finished(ssl SSL) int

fn C.EVP_aes_128_gcm() &EVP_CIPHER
fn C.EVP_aes_256_gcm() &EVP_CIPHER
fn C.EVP_sha256() &EVP_MD
fn C.EVP_sha384() &EVP_MD

fn C.EVP_CIPHER_CTX_new() EVP_CIPHER_CTX
fn C.EVP_CIPHER_CTX_free(ctx EVP_CIPHER_CTX)
fn C.EVP_EncryptInit_ex(ctx EVP_CIPHER_CTX, cipher &EVP_CIPHER, impl voidptr, key &u8, iv &u8) int
fn C.EVP_DecryptInit_ex(ctx EVP_CIPHER_CTX, cipher &EVP_CIPHER, impl voidptr, key &u8, iv &u8) int
fn C.EVP_EncryptUpdate(ctx EVP_CIPHER_CTX, out &u8, outl &int, in_ &u8, inl int) int
fn C.EVP_DecryptUpdate(ctx EVP_CIPHER_CTX, out &u8, outl &int, in_ &u8, inl int) int
fn C.EVP_EncryptFinal_ex(ctx EVP_CIPHER_CTX, out &u8, outl &int) int
fn C.EVP_DecryptFinal_ex(ctx EVP_CIPHER_CTX, out &u8, outl &int) int

fn C.EVP_PKEY_CTX_new_id(id int, e voidptr) EVP_PKEY_CTX
fn C.EVP_PKEY_CTX_free(ctx EVP_PKEY_CTX)
fn C.EVP_PKEY_derive_init(ctx EVP_PKEY_CTX) int
fn C.EVP_PKEY_CTX_hkdf_mode(ctx EVP_PKEY_CTX, mode int) int
fn C.EVP_PKEY_CTX_set_hkdf_md(ctx EVP_PKEY_CTX, md &EVP_MD) int
fn C.EVP_PKEY_CTX_set1_hkdf_salt(ctx EVP_PKEY_CTX, salt &u8, saltlen int) int
fn C.EVP_PKEY_CTX_set1_hkdf_key(ctx EVP_PKEY_CTX, key &u8, keylen int) int
fn C.EVP_PKEY_CTX_add1_hkdf_info(ctx EVP_PKEY_CTX, info &u8, infolen int) int
fn C.EVP_PKEY_derive(ctx EVP_PKEY_CTX, key &u8, keylen &u64) int

fn C.RAND_bytes(buf &u8, num int) int

// new_crypto_context creates a new crypto context for client
pub fn new_crypto_context_client(alpn []string) !CryptoContext {
	// Create SSL context
	method := C.TLS_client_method()
	ssl_ctx := C.SSL_CTX_new(method)
	if ssl_ctx == unsafe { nil } {
		return error('failed to create SSL context')
	}

	// Set TLS 1.3 only
	C.SSL_CTX_set_min_proto_version(ssl_ctx, tls1_3_version)
	C.SSL_CTX_set_max_proto_version(ssl_ctx, tls1_3_version)

	// Set ALPN protocols
	if alpn.len > 0 {
		// Format: length-prefixed strings
		mut alpn_data := []u8{}
		for proto in alpn {
			alpn_data << u8(proto.len)
			alpn_data << proto.bytes()
		}
		C.SSL_CTX_set_alpn_protos(ssl_ctx, alpn_data.data, u32(alpn_data.len))
	}

	// Set default verify paths
	C.SSL_CTX_set_default_verify_paths(ssl_ctx)
	C.SSL_CTX_set_verify(ssl_ctx, ssl_verify_peer, unsafe { nil })

	// Create SSL object
	ssl := C.SSL_new(ssl_ctx)
	if ssl == unsafe { nil } {
		C.SSL_CTX_free(ssl_ctx)
		return error('failed to create SSL object')
	}

	// Set client mode
	C.SSL_set_connect_state(ssl)

	return CryptoContext{
		ssl_ctx:       ssl_ctx
		ssl:           ssl
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
}

// new_crypto_context_server creates a new crypto context for server
pub fn new_crypto_context_server(cert_file string, key_file string, alpn []string) !CryptoContext {
	// Create SSL context
	method := C.TLS_server_method()
	ssl_ctx := C.SSL_CTX_new(method)
	if ssl_ctx == unsafe { nil } {
		return error('failed to create SSL context')
	}

	// Set TLS 1.3 only
	C.SSL_CTX_set_min_proto_version(ssl_ctx, tls1_3_version)
	C.SSL_CTX_set_max_proto_version(ssl_ctx, tls1_3_version)

	// Set ALPN protocols
	if alpn.len > 0 {
		mut alpn_data := []u8{}
		for proto in alpn {
			alpn_data << u8(proto.len)
			alpn_data << proto.bytes()
		}
		C.SSL_CTX_set_alpn_protos(ssl_ctx, alpn_data.data, u32(alpn_data.len))
	}

	// Load certificate and key
	if cert_file != '' {
		cert_result := C.SSL_CTX_use_certificate_file(ssl_ctx, &char(cert_file.str), ssl_filetype_pem)
		if cert_result != 1 {
			C.SSL_CTX_free(ssl_ctx)
			return error('failed to load certificate file: ${cert_file}')
		}
	}

	if key_file != '' {
		key_result := C.SSL_CTX_use_PrivateKey_file(ssl_ctx, &char(key_file.str), ssl_filetype_pem)
		if key_result != 1 {
			C.SSL_CTX_free(ssl_ctx)
			return error('failed to load private key file: ${key_file}')
		}
	}

	// Create SSL object
	ssl := C.SSL_new(ssl_ctx)
	if ssl == unsafe { nil } {
		C.SSL_CTX_free(ssl_ctx)
		return error('failed to create SSL object')
	}

	// Set server mode
	C.SSL_set_accept_state(ssl)

	return CryptoContext{
		ssl_ctx:       ssl_ctx
		ssl:           ssl
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
}

// free frees the crypto context
pub fn (mut ctx CryptoContext) free() {
	if ctx.tx_cipher_ctx != unsafe { nil } {
		C.EVP_CIPHER_CTX_free(ctx.tx_cipher_ctx)
	}
	if ctx.rx_cipher_ctx != unsafe { nil } {
		C.EVP_CIPHER_CTX_free(ctx.rx_cipher_ctx)
	}
	if ctx.ssl != unsafe { nil } {
		// Free the per-connection conn_ref before freeing the SSL object
		C.quic_cleanup_crypto(ctx.ssl)
		C.SSL_free(ctx.ssl)
	}
	if ctx.ssl_ctx != unsafe { nil } {
		C.SSL_CTX_free(ctx.ssl_ctx)
	}
}

// provide_data provides crypto data to TLS
pub fn (mut ctx CryptoContext) provide_data(level CryptoLevel, data []u8) ! {
	rv := C.SSL_provide_quic_data(ctx.ssl, int(level), data.data, u64(data.len))
	if rv != 1 {
		return error('failed to provide crypto data')
	}
}

// do_handshake performs TLS handshake
pub fn (mut ctx CryptoContext) do_handshake() !bool {
	rv := C.SSL_do_handshake(ctx.ssl)
	if rv == 1 {
		// Handshake complete
		return true
	}

	err := C.SSL_get_error(ctx.ssl, rv)
	if err == 2 || err == 3 { // SSL_ERROR_WANT_READ or SSL_ERROR_WANT_WRITE
		// Need more data
		return false
	}

	return error('handshake failed: error ${err}')
}

// is_handshake_complete checks if handshake is complete
pub fn (ctx CryptoContext) is_handshake_complete() bool {
	return C.SSL_is_init_finished(ctx.ssl) != 0
}

// derive_initial_secrets derives initial secrets for QUIC
// RFC 9001 Section 5.2: Initial Secrets
pub fn derive_initial_secrets(dcid []u8, is_server bool) !([]u8, []u8) {
	// QUIC initial salt per RFC 9001 Section 5.2 (QUIC version 1)
	initial_salt := [u8(0x38), 0x76, 0x2c, 0xf7, 0xf5, 0x59, 0x34, 0xb3, 0x4d, 0x17, 0x9a, 0xe6,
		0xa4, 0xc8, 0x0c, 0xad, 0xcc, 0xbb, 0x7f, 0x0a]

	// Extract initial secret using HKDF-Extract
	initial_secret := hkdf_extract(initial_salt, dcid)!

	// Derive client and server secrets
	client_label := 'client in'.bytes()
	server_label := 'server in'.bytes()

	client_secret := hkdf_expand_label(initial_secret, client_label, []u8{}, 32)!
	server_secret := hkdf_expand_label(initial_secret, server_label, []u8{}, 32)!

	if is_server {
		return server_secret, client_secret
	} else {
		return client_secret, server_secret
	}
}

// hkdf_extract performs HKDF-Extract
fn hkdf_extract(salt []u8, ikm []u8) ![]u8 {
	pctx := C.EVP_PKEY_CTX_new_id(1087, unsafe { nil }) // EVP_PKEY_HKDF
	if pctx == unsafe { nil } {
		return error('failed to create PKEY context')
	}
	defer {
		C.EVP_PKEY_CTX_free(pctx)
	}

	if C.EVP_PKEY_derive_init(pctx) != 1 {
		return error('failed to init derive')
	}

	if C.EVP_PKEY_CTX_hkdf_mode(pctx, 1) != 1 { // EVP_PKEY_HKDEF_MODE_EXTRACT_ONLY
		return error('failed to set HKDF mode')
	}

	md := C.EVP_sha256()
	if C.EVP_PKEY_CTX_set_hkdf_md(pctx, md) != 1 {
		return error('failed to set hash')
	}

	if C.EVP_PKEY_CTX_set1_hkdf_salt(pctx, salt.data, salt.len) != 1 {
		return error('failed to set salt')
	}

	if C.EVP_PKEY_CTX_set1_hkdf_key(pctx, ikm.data, ikm.len) != 1 {
		return error('failed to set key')
	}

	mut out := []u8{len: 32}
	mut outlen := u64(32)
	if C.EVP_PKEY_derive(pctx, out.data, &outlen) != 1 {
		return error('failed to derive')
	}

	return out[..int(outlen)]
}

// hkdf_expand_label performs HKDF-Expand-Label for TLS 1.3
fn hkdf_expand_label(secret []u8, label []u8, context []u8, length int) ![]u8 {
	// Build HkdfLabel structure
	mut hkdf_label := []u8{}

	// Length (2 bytes)
	hkdf_label << u8(length >> 8)
	hkdf_label << u8(length)

	// Label length and label (prefixed with "tls13 ")
	mut full_label := 'tls13 '.bytes()
	full_label << label
	hkdf_label << u8(full_label.len)
	hkdf_label << full_label

	// Context length and context
	hkdf_label << u8(context.len)
	if context.len > 0 {
		hkdf_label << context
	}

	// Perform HKDF-Expand
	pctx := C.EVP_PKEY_CTX_new_id(1087, unsafe { nil }) // EVP_PKEY_HKDF
	if pctx == unsafe { nil } {
		return error('failed to create PKEY context')
	}
	defer {
		C.EVP_PKEY_CTX_free(pctx)
	}

	if C.EVP_PKEY_derive_init(pctx) != 1 {
		return error('failed to init derive')
	}

	if C.EVP_PKEY_CTX_hkdf_mode(pctx, 2) != 1 { // EVP_PKEY_HKDEF_MODE_EXPAND_ONLY
		return error('failed to set HKDF mode')
	}

	md := C.EVP_sha256()
	if C.EVP_PKEY_CTX_set_hkdf_md(pctx, md) != 1 {
		return error('failed to set hash')
	}

	if C.EVP_PKEY_CTX_set1_hkdf_key(pctx, secret.data, secret.len) != 1 {
		return error('failed to set key')
	}

	if C.EVP_PKEY_CTX_add1_hkdf_info(pctx, hkdf_label.data, hkdf_label.len) != 1 {
		return error('failed to set info')
	}

	mut out := []u8{len: length}
	mut outlen := u64(length)
	if C.EVP_PKEY_derive(pctx, out.data, &outlen) != 1 {
		return error('failed to derive')
	}

	return out[..int(outlen)]
}

// encrypt_packet encrypts a QUIC packet using AES-128-GCM.
// RFC 9001 Section 5.3: Per-Packet Keys and IVs.
// The nonce is derived by XORing the base IV (from traffic secret) with the
// left-zero-padded packet number. base_iv must be 12 bytes (GCM IV length).
pub fn (mut ctx CryptoContext) encrypt_packet(plaintext []u8, ad []u8, base_iv []u8, packet_number u64) ![]u8 {
	if base_iv.len != 12 {
		return error('base_iv must be 12 bytes for AES-128-GCM')
	}

	// Derive nonce: base_iv XOR left-padded packet_number (RFC 9001 §5.3)
	nonce := derive_nonce(base_iv, packet_number)

	// Use AES-128-GCM for encryption
	cipher := C.EVP_aes_128_gcm()

	// Initialize encryption with derived nonce
	if C.EVP_EncryptInit_ex(ctx.tx_cipher_ctx, cipher, unsafe { nil }, ctx.tx_secret.data,
		nonce.data) != 1 {
		return error('failed to init encryption')
	}

	// Set additional authenticated data
	mut outlen := 0
	if ad.len > 0 {
		if C.EVP_EncryptUpdate(ctx.tx_cipher_ctx, unsafe { nil }, &outlen, ad.data, ad.len) != 1 {
			return error('failed to set AAD')
		}
	}

	// Encrypt
	mut ciphertext := []u8{len: plaintext.len + 16} // +16 for GCM tag
	if C.EVP_EncryptUpdate(ctx.tx_cipher_ctx, ciphertext.data, &outlen, plaintext.data,
		plaintext.len) != 1 {
		return error('failed to encrypt')
	}

	mut final_len := 0
	unsafe {
		if C.EVP_EncryptFinal_ex(ctx.tx_cipher_ctx, &u8(ciphertext.data) + outlen, &final_len) != 1 {
			return error('failed to finalize encryption')
		}
	}
	return ciphertext[..outlen + final_len]
}

// decrypt_packet decrypts a QUIC packet using AES-128-GCM.
// RFC 9001 Section 5.3: Per-Packet Keys and IVs.
// The nonce is derived by XORing the base IV (from traffic secret) with the
// left-zero-padded packet number, matching encrypt_packet's derivation.
// base_iv must be 12 bytes (GCM IV length).
pub fn (mut ctx CryptoContext) decrypt_packet(ciphertext []u8, ad []u8, base_iv []u8, packet_number u64) ![]u8 {
	if base_iv.len != 12 {
		return error('base_iv must be 12 bytes for AES-128-GCM')
	}

	// Derive nonce using the same method as encrypt_packet (RFC 9001 §5.3)
	nonce := derive_nonce(base_iv, packet_number)

	// Use AES-128-GCM for decryption
	cipher := C.EVP_aes_128_gcm()

	// Initialize decryption with derived nonce
	if C.EVP_DecryptInit_ex(ctx.rx_cipher_ctx, cipher, unsafe { nil }, ctx.rx_secret.data,
		nonce.data) != 1 {
		return error('failed to init decryption')
	}

	// Set additional authenticated data
	mut outlen := 0
	if ad.len > 0 {
		if C.EVP_DecryptUpdate(ctx.rx_cipher_ctx, unsafe { nil }, &outlen, ad.data, ad.len) != 1 {
			return error('failed to set AAD')
		}
	}

	// Decrypt
	mut plaintext := []u8{len: ciphertext.len}
	if C.EVP_DecryptUpdate(ctx.rx_cipher_ctx, plaintext.data, &outlen, ciphertext.data,
		ciphertext.len) != 1 {
		return error('failed to decrypt')
	}

	mut final_len := 0
	unsafe {
		if C.EVP_DecryptFinal_ex(ctx.rx_cipher_ctx, &u8(plaintext.data) + outlen, &final_len) != 1 {
			return error('failed to finalize decryption')
		}
	}
	return plaintext[..outlen + final_len]
}

// apply_header_protection applies QUIC header protection per RFC 9001 Section 5.4.1.
// For AES-based ciphers, the mask is produced by AES-ECB encryption of the sample
// using the header protection key (hp_key). The mask is applied as follows:
//   - mask[0] is applied to the first header byte (packet number length bits / key phase bit)
//   - mask[1..5] are XORed onto the packet number bytes
// The hp_key and sample must each be 16 bytes (AES block size).
pub fn (ctx CryptoContext) apply_header_protection(header []u8, sample []u8) ![]u8 {
	if sample.len < aes.block_size {
		return error('sample must be at least ${aes.block_size} bytes')
	}
	if ctx.tx_hp_key.len != aes.block_size {
		return error('tx_hp_key must be ${aes.block_size} bytes')
	}

	// Compute mask = AES-ECB(hp_key, sample[0..16]) per RFC 9001 §5.4.1
	mask := aes_ecb_encrypt(ctx.tx_hp_key, sample[..aes.block_size])!

	return apply_hp_mask(header, mask)
}

// remove_header_protection removes QUIC header protection per RFC 9001 Section 5.4.1.
// Uses the rx_hp_key. The mask derivation is identical to apply_header_protection;
// header protection is self-inverse when the same mask is applied twice.
pub fn (ctx CryptoContext) remove_header_protection(header []u8, sample []u8) ![]u8 {
	if sample.len < aes.block_size {
		return error('sample must be at least ${aes.block_size} bytes')
	}
	if ctx.rx_hp_key.len != aes.block_size {
		return error('rx_hp_key must be ${aes.block_size} bytes')
	}

	// Compute mask = AES-ECB(hp_key, sample[0..16]) per RFC 9001 §5.4.1
	mask := aes_ecb_encrypt(ctx.rx_hp_key, sample[..aes.block_size])!

	return apply_hp_mask(header, mask)
}

// aes_ecb_encrypt encrypts a single 16-byte block using AES-ECB.
// This is the core primitive for QUIC header protection per RFC 9001 §5.4.1.
fn aes_ecb_encrypt(key []u8, block []u8) ![]u8 {
	if block.len != aes.block_size {
		return error('aes_ecb_encrypt: block must be exactly ${aes.block_size} bytes')
	}
	cipher_block := aes.new_cipher(key)
	mut dst := []u8{len: aes.block_size}
	cipher_block.encrypt(mut dst, block)
	return dst
}

// apply_hp_mask applies the header protection mask per RFC 9001 §5.4.
// mask[0] is applied to first-byte bits; mask[1..5] XOR the packet number bytes.
fn apply_hp_mask(header []u8, mask []u8) ![]u8 {
	if header.len == 0 {
		return error('header must not be empty')
	}
	mut protected := header.clone()

	// Determine long vs short header from the high bit of the first byte
	is_long_header := (protected[0] & 0x80) != 0

	if is_long_header {
		// Long header: mask the low 4 bits of the first byte (packet number length)
		protected[0] ^= mask[0] & 0x0f
	} else {
		// Short header: mask the low 5 bits (key phase + packet number length bits)
		protected[0] ^= mask[0] & 0x1f
	}

	// XOR mask[1..5] onto the packet number bytes (last 4 bytes of header prefix)
	// Packet number starts right after the first byte in the simplest case;
	// for a full implementation the caller is responsible for passing the correct slice.
	pn_len := int(protected[0] & 0x03) + 1 // encoded packet number length
	pn_offset := header.len - pn_len
	for i in 0 .. pn_len {
		if pn_offset + i < protected.len && i + 1 < mask.len {
			protected[pn_offset + i] ^= mask[i + 1]
		}
	}

	return protected
}

// derive_nonce derives the per-packet nonce per RFC 9001 Section 5.3.
// nonce = base_iv XOR left-zero-padded packet_number
// base_iv must be 12 bytes (GCM IV length).
fn derive_nonce(base_iv []u8, packet_number u64) []u8 {
	mut nonce := base_iv.clone()
	// Encode packet_number big-endian into the last 8 bytes of the 12-byte nonce
	nonce[4] ^= u8(packet_number >> 56)
	nonce[5] ^= u8(packet_number >> 48)
	nonce[6] ^= u8(packet_number >> 40)
	nonce[7] ^= u8(packet_number >> 32)
	nonce[8] ^= u8(packet_number >> 24)
	nonce[9] ^= u8(packet_number >> 16)
	nonce[10] ^= u8(packet_number >> 8)
	nonce[11] ^= u8(packet_number)
	return nonce
}

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
