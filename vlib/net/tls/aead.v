module tls

import crypto.aes
import x.crypto.chacha20
import x.crypto.chacha20poly1305

// seal_record encrypts a TLS 1.3 record.
// seal_record returns the full ciphertext record bytes (5-byte header +
// encrypted fragment including tag). `seq` is the record sequence number
// used to derive the nonce. The AEAD follows the negotiated suite.
pub fn seal_record(suite CipherSuite, key []u8, iv []u8, seq u64, content_type ContentType, plaintext []u8) ![]u8 {
	if key.len != cipher_suite_key_len(suite) {
		return error('tls: invalid key length')
	}
	if iv.len != 12 {
		return error('tls: invalid iv length')
	}
	if plaintext.len > max_plaintext_len {
		return error('tls: plaintext too large')
	}
	// nonce = iv XOR seq (big-endian in the last 8 bytes)
	mut nonce := iv.clone()
	for i in 0 .. 8 {
		nonce[4 + i] = iv[4 + i] ^ u8((seq >> (8 * (7 - i))) & 0xff)
	}
	// inner plaintext
	inner := encode_inner(InnerPlaintext{
		content:      plaintext
		content_type: content_type
		padding_len:  0
	})
	// additional_data = opaque_type || legacy_version || TLSCiphertext.length
	// (RFC 8446 §5.2): the length is the on-wire ciphertext length, i.e.
	// inner length plus the 16-byte AEAD tag.
	ct_len := inner.len + aead_tag_size
	mut ad := []u8{cap: 5}
	ad << u8(ContentType.application_data)
	ad << u8((tls_version_1_3 >> 8) & 0xff)
	ad << u8(tls_version_1_3 & 0xff)
	ad << u8(ct_len >> 8)
	ad << u8(ct_len & 0xff)
	ct := aead_encrypt(suite, key, inner, nonce, ad)!
	// build ciphertext record
	mut rec := CipherRecord{
		content_type:   .application_data
		legacy_version: tls_version_1_3
		fragment:       ct
	}
	return encode_cipher_record(rec)
}

// aead_encrypt encrypts with the AEAD for suite.
fn aead_encrypt(suite CipherSuite, key []u8, plaintext []u8, nonce []u8, ad []u8) ![]u8 {
	if suite == .tls_chacha20_poly1305_sha256 {
		aead := chacha20poly1305.new(key, chacha20poly1305.nonce_size, chacha20.Options{}) or {
			return err
		}
		return aead.encrypt(plaintext, nonce, ad) or { return err }
	}
	gcm := aes.new_aes_gcm(key) or { return err }
	return gcm.encrypt(plaintext, nonce, ad) or { return err }
}

// open_record decrypts a TLS 1.3 ciphertext record.
// open_record returns the inner plaintext content and content type.
pub fn open_record(suite CipherSuite, key []u8, iv []u8, seq u64, rec CipherRecord) !(ContentType, []u8) {
	if key.len != cipher_suite_key_len(suite) {
		return error('tls: invalid key length')
	}
	if iv.len != 12 {
		return error('tls: invalid iv length')
	}
	if rec.content_type != .application_data || rec.legacy_version != tls_version_1_3 {
		return error('tls: invalid outer record')
	}
	mut nonce := iv.clone()
	for i in 0 .. 8 {
		nonce[4 + i] = iv[4 + i] ^ u8((seq >> (8 * (7 - i))) & 0xff)
	}
	// rebuild ad from the on-wire ciphertext length (fragment includes the tag).
	ct_len := rec.fragment.len
	if ct_len < aead_tag_size {
		return error('tls: truncated cipher record body')
	}
	mut aad := []u8{cap: 5}
	aad << u8(rec.content_type)
	aad << u8((rec.legacy_version >> 8) & 0xff)
	aad << u8(rec.legacy_version & 0xff)
	aad << u8(ct_len >> 8)
	aad << u8(ct_len & 0xff)
	inner := aead_decrypt(suite, key, rec.fragment, nonce, aad)!
	ip := decode_inner(inner) or { return err }
	return ip.content_type, ip.content
}

// aead_decrypt decrypts with the AEAD for suite.
fn aead_decrypt(suite CipherSuite, key []u8, ciphertext []u8, nonce []u8, ad []u8) ![]u8 {
	if suite == .tls_chacha20_poly1305_sha256 {
		aead := chacha20poly1305.new(key, chacha20poly1305.nonce_size, chacha20.Options{}) or {
			return err
		}
		return aead.decrypt(ciphertext, nonce, ad) or { return err }
	}
	gcm := aes.new_aes_gcm(key) or { return err }
	return gcm.decrypt(ciphertext, nonce, ad) or { return err }
}
