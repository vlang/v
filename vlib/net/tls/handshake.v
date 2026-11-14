module tls

import crypto.hkdf
import crypto.hmac
import crypto.sha256
import hash

// sha256_hash is the Hash constructor used throughout the TLS 1.3 key schedule.
fn sha256_hash() hash.Hash {
	return sha256.new()
}

// hkdf_expand_label implements TLS 1.3's HKDF-Expand-Label (RFC 8446 §7.1).
pub fn hkdf_expand_label(secret []u8, label string, context []u8, length int) ![]u8 {
	if length < 0 || length > 0xffff {
		return error('tls: hkdf_expand_label: length ${length} out of range')
	}
	full_label := 'tls13 ${label}'
	if full_label.len < 7 || full_label.len > 255 {
		return error('tls: hkdf_expand_label: label length ${full_label.len} out of range')
	}
	if context.len > 255 {
		return error('tls: hkdf_expand_label: context length ${context.len} out of range')
	}
	mut hkdf_label := []u8{cap: 2 + 1 + full_label.len + 1 + context.len}
	hkdf_label << u8(length >> 8)
	hkdf_label << u8(length)
	hkdf_label << u8(full_label.len)
	hkdf_label << full_label.bytes()
	hkdf_label << u8(context.len)
	hkdf_label << context
	return hkdf.expand(sha256_hash, secret, hkdf_label.bytestr(), length)
}

// empty_transcript_hash is Transcript-Hash("") (RFC 8446 §7.1).
pub fn empty_transcript_hash() []u8 {
	return sha256.sum256([]u8{})
}

// derive_secret implements RFC 8446 §7.1 Derive-Secret.
pub fn derive_secret(secret []u8, label string, transcript_hash []u8) ![]u8 {
	return hkdf_expand_label(secret, label, transcript_hash, sha256.size)
}

// derive_early_secret: no-PSK branch.
pub fn derive_early_secret() ![]u8 {
	zero_ikm := []u8{len: sha256.size}
	return hkdf.extract(sha256_hash, zero_ikm, []u8{})
}

pub struct HandshakeSecrets {
pub:
	handshake_secret []u8
	client_secret    []u8
	server_secret    []u8
}

pub fn derive_handshake_secrets(early_secret []u8, ecdhe_shared_secret []u8, transcript_hash_ch_sh []u8) !HandshakeSecrets {
	derived := derive_secret(early_secret, 'derived', empty_transcript_hash())!
	handshake_secret := hkdf.extract(sha256_hash, ecdhe_shared_secret, derived)!
	client_secret := derive_secret(handshake_secret, 'c hs traffic', transcript_hash_ch_sh)!
	server_secret := derive_secret(handshake_secret, 's hs traffic', transcript_hash_ch_sh)!
	return HandshakeSecrets{
		handshake_secret: handshake_secret
		client_secret:    client_secret
		server_secret:    server_secret
	}
}

pub struct ApplicationSecrets {
pub:
	master_secret []u8
	client_secret []u8
	server_secret []u8
}

pub fn derive_application_secrets(handshake_secret []u8, transcript_hash_ch_sfin []u8) !ApplicationSecrets {
	derived := derive_secret(handshake_secret, 'derived', empty_transcript_hash())!
	zero_ikm := []u8{len: sha256.size}
	master_secret := hkdf.extract(sha256_hash, zero_ikm, derived)!
	client_secret := derive_secret(master_secret, 'c ap traffic', transcript_hash_ch_sfin)!
	server_secret := derive_secret(master_secret, 's ap traffic', transcript_hash_ch_sfin)!
	return ApplicationSecrets{
		master_secret: master_secret
		client_secret: client_secret
		server_secret: server_secret
	}
}

// TrafficKeys holds the AEAD key/iv and (for TLS 1.3, no separate header
// protection key is used for TCP record layer; QUIC reuses these via
// hkdf_expand_label with "quic ..." labels).
pub struct TrafficKeys {
pub:
	key []u8 // 16 bytes for AES-128-GCM
	iv  []u8 // 12 bytes
}

// derive_traffic_keys derives the AEAD key and IV from a traffic secret.
pub fn derive_traffic_keys(traffic_secret []u8) !TrafficKeys {
	key := hkdf_expand_label(traffic_secret, 'key', []u8{}, 16)!
	iv := hkdf_expand_label(traffic_secret, 'iv', []u8{}, 12)!
	return TrafficKeys{
		key: key
		iv:  iv
	}
}

// finished_verify_data computes the Finished verify_data (RFC 8446 §4.4.4):
// HMAC(finished_key, Transcript-Hash) truncated to Hash.length, where
// finished_key = HKDF-Expand-Label(base_key, "finished", "", Hash.length).
pub fn finished_verify_data(base_secret []u8, transcript_hash []u8) ![]u8 {
	finished_key := hkdf_expand_label(base_secret, 'finished', []u8{}, sha256.size)!
	mut mac := hmac.new_hmac(sha256.new, finished_key)
	mac.write(transcript_hash) or { return err }
	mut out := []u8{len: sha256.size}
	mac.sum_into(mut out)
	return out
}
