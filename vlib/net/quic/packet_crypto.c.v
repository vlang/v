module quic

import net.mbedtls as _

// AES-128-GCM and AES-128 single-block primitives for QUIC packet and header
// protection (RFC 9001 §5.3, §5.4.3), backed by the mbedTLS build net.quic
// already links for X.509 (see tls13_certificate_chain.c.v). mbedTLS uses
// ARMv8 Crypto Extensions or AES-NI where available; the generic pure-V
// crypto.aes path these replace cost hundreds of microseconds per packet.
//
// Every helper is one-shot: its mbedTLS context lives on the stack for a
// single call and is freed before returning, so no C state outlives the call
// and QuicPacketProtectionKeys stays a plain, freely copyable value.

#include "mbedtls/gcm.h"
#include "mbedtls/aes.h"

@[typedef]
struct C.mbedtls_gcm_context {}

@[typedef]
struct C.mbedtls_aes_context {}

fn C.mbedtls_gcm_init(ctx &C.mbedtls_gcm_context)
fn C.mbedtls_gcm_setkey(ctx &C.mbedtls_gcm_context, cipher int, key &u8, keybits u32) int
fn C.mbedtls_gcm_crypt_and_tag(ctx &C.mbedtls_gcm_context, mode int, length uint, iv &u8, iv_len uint, add &u8, add_len uint, input &u8, output &u8, tag_len uint, tag &u8) int
fn C.mbedtls_gcm_auth_decrypt(ctx &C.mbedtls_gcm_context, length uint, iv &u8, iv_len uint, add &u8, add_len uint, tag &u8, tag_len uint, input &u8, output &u8) int
fn C.mbedtls_gcm_free(ctx &C.mbedtls_gcm_context)
fn C.mbedtls_aes_init(ctx &C.mbedtls_aes_context)
fn C.mbedtls_aes_setkey_enc(ctx &C.mbedtls_aes_context, key &u8, keybits u32) int
fn C.mbedtls_aes_crypt_ecb(ctx &C.mbedtls_aes_context, mode int, input &u8, output &u8) int
fn C.mbedtls_aes_free(ctx &C.mbedtls_aes_context)

const aead_tag_length = 16

// aes128_gcm_seal returns `plaintext` encrypted under `key` with `nonce`,
// authenticating `aad`, followed by the 16-byte tag.
fn aes128_gcm_seal(key []u8, nonce []u8, aad []u8, plaintext []u8) ![]u8 {
	if key.len != aead_key_length {
		return error('quic: AES-128-GCM key must be ${aead_key_length} bytes, got ${key.len}')
	}
	if nonce.len == 0 {
		return error('quic: AES-128-GCM nonce must not be empty')
	}
	mut out := []u8{len: plaintext.len + aead_tag_length}
	ctx := C.mbedtls_gcm_context{}
	C.mbedtls_gcm_init(&ctx)
	defer {
		C.mbedtls_gcm_free(&ctx)
	}
	if C.mbedtls_gcm_setkey(&ctx, C.MBEDTLS_CIPHER_ID_AES, key.data, 128) != 0 {
		return error('quic: AES-128-GCM key setup failed')
	}
	// The tag is written directly after the ciphertext in `out`.
	tag := unsafe { &u8(out.data) + plaintext.len }
	if C.mbedtls_gcm_crypt_and_tag(&ctx, C.MBEDTLS_GCM_ENCRYPT, uint(plaintext.len), nonce.data,
		uint(nonce.len), aad.data, uint(aad.len), plaintext.data, out.data, uint(aead_tag_length),
		tag) != 0 {
		return error('quic: AES-128-GCM encryption failed')
	}
	return out
}

// aes128_gcm_open authenticates and decrypts `ciphertext` (payload followed
// by its 16-byte tag) under `key` with `nonce` and `aad`. Any authentication
// failure is reported as an error and no plaintext is returned.
fn aes128_gcm_open(key []u8, nonce []u8, aad []u8, ciphertext []u8) ![]u8 {
	if key.len != aead_key_length {
		return error('quic: AES-128-GCM key must be ${aead_key_length} bytes, got ${key.len}')
	}
	if nonce.len == 0 {
		return error('quic: AES-128-GCM nonce must not be empty')
	}
	if ciphertext.len < aead_tag_length {
		return error('quic: AES-128-GCM ciphertext shorter than its ${aead_tag_length}-byte tag')
	}
	body_len := ciphertext.len - aead_tag_length
	mut out := []u8{len: body_len}
	ctx := C.mbedtls_gcm_context{}
	C.mbedtls_gcm_init(&ctx)
	defer {
		C.mbedtls_gcm_free(&ctx)
	}
	if C.mbedtls_gcm_setkey(&ctx, C.MBEDTLS_CIPHER_ID_AES, key.data, 128) != 0 {
		return error('quic: AES-128-GCM key setup failed')
	}
	tag := unsafe { &u8(ciphertext.data) + body_len }
	if C.mbedtls_gcm_auth_decrypt(&ctx, uint(body_len), nonce.data, uint(nonce.len), aad.data,
		uint(aad.len), tag, uint(aead_tag_length), ciphertext.data, out.data) != 0 {
		return error('quic: AES-128-GCM authentication failed')
	}
	return out
}

// aes128_encrypt_block encrypts one 16-byte `block` under the 16-byte `key`
// (AES-ECB on a single block, as RFC 9001 §5.4.3 header protection uses).
fn aes128_encrypt_block(key []u8, block []u8) ![]u8 {
	if key.len != 16 || block.len != 16 {
		return error('quic: AES-128 block encryption needs a 16-byte key and block')
	}
	mut out := []u8{len: 16}
	ctx := C.mbedtls_aes_context{}
	C.mbedtls_aes_init(&ctx)
	defer {
		C.mbedtls_aes_free(&ctx)
	}
	if C.mbedtls_aes_setkey_enc(&ctx, key.data, 128) != 0 {
		return error('quic: AES-128 key setup failed')
	}
	if C.mbedtls_aes_crypt_ecb(&ctx, C.MBEDTLS_AES_ENCRYPT, block.data, out.data) != 0 {
		return error('quic: AES-128 block encryption failed')
	}
	return out
}
