// Copyright (c) 2019-2024 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module quic

// TLS 1.3 crypto callbacks for ngtcp2
// This implements the cryptographic operations needed for QUIC

#flag -lssl
#flag -lcrypto

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/kdf.h>

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
	// TODO: Add SSL_CTX_use_certificate_file and SSL_CTX_use_PrivateKey_file

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
pub fn derive_initial_secrets(dcid []u8, is_server bool) !([]u8, []u8) {
	// QUIC initial salt (RFC 9001)
	initial_salt := [u8(0x38), 0xb7, 0x2a, 0x50, 0xc5, 0x8c, 0x6f, 0x8a, 0x8b, 0x8d, 0x6c, 0x8f,
		0x9b, 0x8e, 0x7c, 0x8a, 0x8d, 0x7e, 0x8c, 0x6d]

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

// encrypt_packet encrypts a QUIC packet
pub fn (mut ctx CryptoContext) encrypt_packet(plaintext []u8, ad []u8) ![]u8 {
	// Use AES-128-GCM for encryption
	cipher := C.EVP_aes_128_gcm()

	// Generate IV (12 bytes for GCM)
	mut iv := []u8{len: 12}
	C.RAND_bytes(iv.data, 12)

	// Initialize encryption
	if C.EVP_EncryptInit_ex(ctx.tx_cipher_ctx, cipher, unsafe { nil }, ctx.tx_secret.data,
		iv.data) != 1 {
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

// decrypt_packet decrypts a QUIC packet
pub fn (mut ctx CryptoContext) decrypt_packet(ciphertext []u8, ad []u8) ![]u8 {
	// Use AES-128-GCM for decryption
	cipher := C.EVP_aes_128_gcm()

	// Extract IV (first 12 bytes)
	if ciphertext.len < 12 {
		return error('ciphertext too short')
	}
	iv := ciphertext[..12]
	encrypted_data := ciphertext[12..]

	// Initialize decryption
	if C.EVP_DecryptInit_ex(ctx.rx_cipher_ctx, cipher, unsafe { nil }, ctx.rx_secret.data,
		iv.data) != 1 {
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
	mut plaintext := []u8{len: encrypted_data.len}
	if C.EVP_DecryptUpdate(ctx.rx_cipher_ctx, plaintext.data, &outlen, encrypted_data.data,
		encrypted_data.len) != 1 {
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

// apply_header_protection applies header protection
pub fn (ctx CryptoContext) apply_header_protection(header []u8, sample []u8) ![]u8 {
	// Simplified header protection
	// In a real implementation, this would use AES-ECB
	mut protected := []u8{len: header.len}

	// Copy and XOR in one pass
	for i in 0 .. header.len {
		if i < sample.len {
			protected[i] = header[i] ^ sample[i]
		} else {
			protected[i] = header[i]
		}
	}

	return protected
}

// remove_header_protection removes header protection
pub fn (ctx CryptoContext) remove_header_protection(header []u8, sample []u8) ![]u8 {
	// Header protection is symmetric
	return ctx.apply_header_protection(header, sample)
}
