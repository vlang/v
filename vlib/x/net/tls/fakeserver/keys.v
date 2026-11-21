// Copyright (c) 2026 blackshirt.
//
// keys.v is the fakeserver's private copy of the RFC 8446 §7.1 key-schedule
// primitives, pinned to SHA-256 — the hash its two suites share, differing only in
// AEAD key length. It does NOT import tls.handshake, which would form an import
// cycle with the handshake tests.
module fakeserver

import crypto.hmac
import crypto.sha256

// hash_len is the SHA-256 digest size; the suite pins the hash (RFC 8446 §9.1).
const hash_len = 32

// iv_len is the 12-byte AEAD nonce base every suite the fakeserver can select
// shares (RFC 8446 §7.1). The AEAD key length is suite-dependent, so traffic_keys
// takes it explicitly instead of pinning a single value here.
const iv_len = 12

// hmac_block_size is the SHA-256 HMAC block size (RFC 2104).
const hmac_block_size = 64

// sha256_digest hashes data with SHA-256.
fn sha256_digest(data []u8) []u8 {
	return sha256.sum256(data)
}

// hmac_sha256 computes HMAC-SHA-256 over data with key.
fn hmac_sha256(key []u8, data []u8) []u8 {
	return hmac.new(key, data, sha256.sum256, hmac_block_size)
}

// hkdf_extract implements HKDF-Extract(salt, IKM) (RFC 5869 §2.2); an empty salt
// means Hash.length zero bytes, the default the TLS 1.3 Early Secret relies on.
fn hkdf_extract(salt []u8, ikm []u8) []u8 {
	effective_salt := if salt.len == 0 { []u8{len: hash_len} } else { salt }
	return hmac_sha256(effective_salt, ikm)
}

// hkdf_expand implements HKDF-Expand(prk, info, length) (RFC 5869 §2.3):
// T(i) = HMAC(PRK, T(i-1) || info || i).
fn hkdf_expand(prk []u8, info []u8, length int) ![]u8 {
	if length < 0 || length > 255 * hash_len {
		return error('fakeserver: HKDF-Expand length ${length} out of range 0..${255 * hash_len}')
	}
	mut out := []u8{cap: length}
	mut prev := []u8{}
	mut counter := u8(1)
	for out.len < length {
		mut step_input := []u8{cap: prev.len + info.len + 1}
		step_input << prev
		step_input << info
		step_input << counter
		prev = hmac_sha256(prk, step_input)
		needed := length - out.len
		to_copy := if needed < prev.len { needed } else { prev.len }
		out << prev[..to_copy]
		counter++
	}
	return out
}

// hkdf_expand_label implements RFC 8446 §7.1 HKDF-Expand-Label: the label is
// prefixed with "tls13 ", domain-separating TLS 1.3 derivations.
fn hkdf_expand_label(secret []u8, label string, context []u8, length int) ![]u8 {
	full_label := 'tls13 ${label}'
	if full_label.len > 255 {
		return error('fakeserver: HKDF-Expand-Label label "${full_label}" exceeds 255 bytes')
	}
	if context.len > 255 {
		return error('fakeserver: HKDF-Expand-Label context is ${context.len} bytes, exceeds 255')
	}
	mut hkdf_label := []u8{cap: 2 + 1 + full_label.len + 1 + context.len}
	hkdf_label << u8((length >> 8) & 0xff)
	hkdf_label << u8(length & 0xff)
	hkdf_label << u8(full_label.len)
	hkdf_label << full_label.bytes()
	hkdf_label << u8(context.len)
	hkdf_label << context
	return hkdf_expand(secret, hkdf_label, length)
}

// derive_secret implements Derive-Secret(Secret, Label, Messages) =
// HKDF-Expand-Label(Secret, Label, Transcript-Hash(Messages), Hash.length) (RFC 8446 §7.1).
fn derive_secret(secret []u8, label string, transcript_hash []u8) ![]u8 {
	return hkdf_expand_label(secret, label, transcript_hash, hash_len)
}

// finished_verify_data computes a Finished verify_data (RFC 8446 §4.4.4):
// HMAC(finished_key, Transcript-Hash), finished_key = Expand-Label(base_key, "finished", "").
fn finished_verify_data(base_key []u8, transcript_hash []u8) ![]u8 {
	finished_key := hkdf_expand_label(base_key, 'finished', []u8{}, hash_len)!
	return hmac_sha256(finished_key, transcript_hash)
}

// traffic_keys expands a traffic secret into the AEAD key and 12-byte IV for
// one direction (RFC 8446 §7.1 "key" / "iv" labels); klen is the selected suite's
// key length (16 for AES-128-GCM, 32 for ChaCha20-Poly1305), both on SHA-256.
fn traffic_keys(secret []u8, klen int) !([]u8, []u8) {
	key := hkdf_expand_label(secret, 'key', []u8{}, klen)!
	iv := hkdf_expand_label(secret, 'iv', []u8{}, iv_len)!
	return key, iv
}
