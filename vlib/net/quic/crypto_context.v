module quic

// TLS 1.3 crypto context management for QUIC connections.
import time

#flag -lssl
#flag -lcrypto

#include "@VEXEROOT/vlib/net/quic/quic_stubs.c"
#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/evp.h>
#include <openssl/kdf.h>
#include <openssl/rand.h>

// OpenSSL opaque types
type SSL = voidptr
type SSL_CTX = voidptr
type SSL_METHOD = voidptr
type EVP_CIPHER = voidptr
type EVP_MD = voidptr
type EVP_CIPHER_CTX = voidptr
type EVP_PKEY_CTX = voidptr

// tls1_3_version is the TLS 1.3 version identifier (RFC 8446).
pub const tls1_3_version = 0x0304
// ssl_verify_none disables SSL peer certificate verification.
pub const ssl_verify_none = 0
// ssl_verify_peer enables SSL peer certificate verification.
pub const ssl_verify_peer = 1

// ssl_filetype_pem selects PEM-encoded certificate/key files.
pub const ssl_filetype_pem = 1
// ssl_filetype_asn1 selects ASN.1/DER-encoded certificate/key files.
pub const ssl_filetype_asn1 = 2

// CryptoLevel represents the QUIC encryption level (from ngtcp2)
pub enum CryptoLevel {
	initial     = 0
	handshake   = 1
	application = 2
	early       = 3
}

// CryptoContext holds cryptographic state for a QUIC connection
pub struct CryptoContext {
pub mut:
	ssl_ctx SSL_CTX
	ssl     SSL
	// Traffic secrets (from TLS handshake)
	tx_secret []u8
	rx_secret []u8
	// Derived AES-128-GCM keys (16 bytes each, from HKDF-Expand-Label)
	tx_key []u8
	rx_key []u8
	// Derived IVs / base_iv (12 bytes each, from HKDF-Expand-Label)
	tx_iv []u8
	rx_iv []u8
	// Cipher contexts
	tx_cipher_ctx EVP_CIPHER_CTX
	rx_cipher_ctx EVP_CIPHER_CTX
	// Header protection
	tx_hp_key []u8
	rx_hp_key []u8
}

// Session ticket extraction (RFC 9001 §8)
fn C.SSL_get1_session(ssl SSL) voidptr
fn C.SSL_SESSION_get_timeout(session voidptr) i64
fn C.SSL_SESSION_get_max_early_data(session voidptr) u32
fn C.i2d_SSL_SESSION(session voidptr, pp &&u8) int
fn C.SSL_SESSION_free(session voidptr)

// SSL/TLS C function declarations

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

fn C.SSL_new(ctx SSL_CTX) SSL
fn C.SSL_free(ssl SSL)
fn C.SSL_set_connect_state(ssl SSL)
fn C.SSL_set_accept_state(ssl SSL)
fn C.SSL_do_handshake(ssl SSL) int
fn C.SSL_get_error(ssl SSL, ret int) int
fn C.SSL_provide_quic_data(ssl SSL, level int, data &u8, len u64) int
fn C.SSL_process_quic_post_handshake(ssl SSL) int
fn C.SSL_is_init_finished(ssl SSL) int

fn C.SSL_get0_alpn_selected(ssl SSL, data &&u8, len_ &u32)

fn C.EVP_CIPHER_CTX_new() EVP_CIPHER_CTX
fn C.EVP_CIPHER_CTX_free(ctx EVP_CIPHER_CTX)

fn C.RAND_bytes(buf &u8, num int) int

// new_crypto_context_client creates a new crypto context for a QUIC client
pub fn new_crypto_context_client(alpn []string) !CryptoContext {
	method := C.TLS_client_method()
	ssl_ctx := C.SSL_CTX_new(method)
	if ssl_ctx == unsafe { nil } {
		return error('failed to create SSL context')
	}

	C.SSL_CTX_set_min_proto_version(ssl_ctx, tls1_3_version)
	C.SSL_CTX_set_max_proto_version(ssl_ctx, tls1_3_version)

	if alpn.len > 0 {
		mut alpn_data := []u8{}
		for proto in alpn {
			alpn_data << u8(proto.len)
			alpn_data << proto.bytes()
		}
		C.SSL_CTX_set_alpn_protos(ssl_ctx, alpn_data.data, u32(alpn_data.len))
	}

	C.SSL_CTX_set_default_verify_paths(ssl_ctx)
	C.SSL_CTX_set_verify(ssl_ctx, ssl_verify_peer, unsafe { nil })

	ssl := C.SSL_new(ssl_ctx)
	if ssl == unsafe { nil } {
		C.SSL_CTX_free(ssl_ctx)
		return error('failed to create SSL object')
	}

	C.SSL_set_connect_state(ssl)

	return CryptoContext{
		ssl_ctx:       ssl_ctx
		ssl:           ssl
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
}

// new_crypto_context_server creates a new crypto context for a QUIC server
pub fn new_crypto_context_server(cert_file string, key_file string, alpn []string) !CryptoContext {
	method := C.TLS_server_method()
	ssl_ctx := C.SSL_CTX_new(method)
	if ssl_ctx == unsafe { nil } {
		return error('failed to create SSL context')
	}

	C.SSL_CTX_set_min_proto_version(ssl_ctx, tls1_3_version)
	C.SSL_CTX_set_max_proto_version(ssl_ctx, tls1_3_version)

	if alpn.len > 0 {
		mut alpn_data := []u8{}
		for proto in alpn {
			alpn_data << u8(proto.len)
			alpn_data << proto.bytes()
		}
		C.SSL_CTX_set_alpn_protos(ssl_ctx, alpn_data.data, u32(alpn_data.len))
	}

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

	ssl := C.SSL_new(ssl_ctx)
	if ssl == unsafe { nil } {
		C.SSL_CTX_free(ssl_ctx)
		return error('failed to create SSL object')
	}

	C.SSL_set_accept_state(ssl)

	return CryptoContext{
		ssl_ctx:       ssl_ctx
		ssl:           ssl
		tx_cipher_ctx: C.EVP_CIPHER_CTX_new()
		rx_cipher_ctx: C.EVP_CIPHER_CTX_new()
	}
}

// free releases all OpenSSL resources held by the crypto context
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
		return true
	}

	err := C.SSL_get_error(ctx.ssl, rv)
	if err == 2 || err == 3 { // SSL_ERROR_WANT_READ or SSL_ERROR_WANT_WRITE
		return false
	}

	return error('handshake failed: error ${err}')
}

// is_handshake_complete checks if handshake is complete
pub fn (ctx CryptoContext) is_handshake_complete() bool {
	return C.SSL_is_init_finished(ctx.ssl) != 0
}

// extract_session_ticket extracts a TLS session ticket for 0-RTT resumption (RFC 9001 §8).
// Returns none if the SSL handle is nil or no session is available.
pub fn (ctx &CryptoContext) extract_session_ticket(server_name string) ?SessionTicket {
	if ctx.ssl == unsafe { nil } {
		return none
	}

	session := C.SSL_get1_session(ctx.ssl)
	if session == unsafe { nil } {
		return none
	}

	timeout := C.SSL_SESSION_get_timeout(session)
	max_early := C.SSL_SESSION_get_max_early_data(session)

	// Serialize session to DER format
	der_len := C.i2d_SSL_SESSION(session, unsafe { nil })
	if der_len <= 0 {
		C.SSL_SESSION_free(session)
		return none
	}

	mut der_buf := []u8{len: int(der_len)}
	mut p := &u8(der_buf.data)
	C.i2d_SSL_SESSION(session, &p)
	C.SSL_SESSION_free(session)

	return SessionTicket{
		ticket:          der_buf
		creation_time:   time.now()
		max_early_data:  max_early
		server_name:     server_name
		ticket_lifetime: u32(timeout)
	}
}

// get_alpn_selected returns the ALPN protocol selected during TLS handshake.
// Returns none if no SSL context exists or no ALPN was negotiated.
pub fn (ctx &CryptoContext) get_alpn_selected() ?string {
	if ctx.ssl == unsafe { nil } {
		return none
	}
	data := &u8(unsafe { nil })
	len_ := u32(0)
	C.SSL_get0_alpn_selected(ctx.ssl, &data, &len_)
	if data == unsafe { nil } || len_ == 0 {
		return none
	}
	return unsafe { tos(data, int(len_)) }
}
