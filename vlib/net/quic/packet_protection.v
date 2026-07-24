module quic

import crypto.aes

// RFC 9001 §5 — QUIC packet protection. v1 pins TLS_AES_128_GCM_SHA256 (see
// tls13_client_hello.v), so packet protection here always uses AES-128-GCM.

// aead_key_length and aead_iv_length are AES-128-GCM's fixed key/nonce sizes
// (RFC 9001 §5.1) — the only AEAD v1 supports. AES-128-GCM's header
// protection key is also 16 bytes (RFC 9001 §5.4.3), the same as
// aead_key_length, so both `key` and `hp` below share this length.
const aead_key_length = 16
const aead_iv_length = 12

// QuicPacketProtectionKeys holds one encryption level's, one direction's
// derived key material (RFC 9001 §5.1): `key`/`iv` protect the AEAD payload,
// `hp` protects the header. A single traffic secret (e.g. one level's
// client_secret) yields exactly one QuicPacketProtectionKeys — client and
// server secrets are always different PRKs (see tls13_keyschedule.v /
// initial_secrets.v), so deriving from the correct side's secret is what
// keeps client-write and server-write keys distinct; this struct itself has
// no notion of "which direction" beyond whatever secret it was derived from.
pub struct QuicPacketProtectionKeys {
pub:
	key []u8 // quic_key -- AEAD encryption key (16 bytes)
	iv  []u8 // quic_iv -- AEAD nonce base (12 bytes)
	hp  []u8 // quic_hp -- header protection key (16 bytes)
}

// derive_packet_protection_keys computes quic_key/quic_iv/quic_hp (RFC 9001
// §5.1) from one encryption level's one-directional traffic secret (an
// Initial secret, a handshake traffic secret, or an application traffic
// secret — this function is level-agnostic, since HKDF-Expand-Label's
// "quic key"/"quic iv"/"quic hp" labels are identical at every level).
pub fn derive_packet_protection_keys(secret []u8) !QuicPacketProtectionKeys {
	key := hkdf_expand_label(secret, 'quic key', []u8{}, aead_key_length)!
	iv := hkdf_expand_label(secret, 'quic iv', []u8{}, aead_iv_length)!
	hp := hkdf_expand_label(secret, 'quic hp', []u8{}, aead_key_length)!
	return QuicPacketProtectionKeys{
		key: key
		iv:  iv
		hp:  hp
	}
}

// packet_protection_nonce computes the per-packet AEAD nonce (RFC 9001
// §5.3): the packet's FULL, RECONSTRUCTED packet number (never the
// truncated on-the-wire bytes — using the truncated value would collide
// across every packet sharing the same low-order bytes) left-padded with
// zeros to the IV's length, XORed with `iv`. `iv` must be at least 8 bytes
// (long enough to hold a full u64 packet number) -- the only current
// producer, derive_packet_protection_keys, always returns exactly
// aead_iv_length (12) bytes, but QuicPacketProtectionKeys's fields are
// public and externally constructible, so this is checked explicitly
// rather than trusted, mirroring header_protection_mask's analogous
// `hp_key.len` check on the same struct's sibling field.
fn packet_protection_nonce(iv []u8, packet_number u64) ![]u8 {
	if iv.len < 8 {
		return error('quic: packet protection IV must be at least 8 bytes, got ${iv.len}')
	}
	mut nonce := iv.clone()
	for i in 0 .. 8 {
		nonce[nonce.len - 1 - i] ^= u8(packet_number >> (u32(i) * 8))
	}
	return nonce
}

// encrypt_packet_payload AEAD-encrypts one packet's payload (RFC 9001 §5.3).
// `header` must be the ENTIRE unprotected header, up to and including the
// plaintext packet number bytes — it is the AEAD associated data in full,
// not just a prefix of it. `packet_number` must already be the full,
// reconstructed value (see packet_protection_nonce).
pub fn encrypt_packet_payload(keys QuicPacketProtectionKeys, packet_number u64, header []u8, payload []u8) ![]u8 {
	aead := aes.new_aes_gcm(keys.key)!
	nonce := packet_protection_nonce(keys.iv, packet_number)!
	return aead.encrypt(payload, nonce, header)
}

// decrypt_packet_payload AEAD-decrypts and authenticates one packet's
// payload. On authentication failure, callers MUST silently drop the packet
// rather than tear down the connection: a single AEAD failure is
// indistinguishable from ordinary network corruption or off-path garbage
// UDP data, and RFC 9001's security guidance is that it must never, by
// itself, be escalated to a connection close. This function only reports the
// failure as an error; enforcing "drop, don't close" is the caller's
// (Phase 4+ packet-receive loop's) responsibility.
pub fn decrypt_packet_payload(keys QuicPacketProtectionKeys, packet_number u64, header []u8, ciphertext []u8) ![]u8 {
	aead := aes.new_aes_gcm(keys.key)!
	nonce := packet_protection_nonce(keys.iv, packet_number)!
	return aead.decrypt(ciphertext, nonce, header)
}

// protect_packet assembles one fully protected QUIC packet from its
// unprotected header, full packet number, and plaintext payload, applying
// RFC 9001 §5's protection steps in the ONLY correct order: AEAD-encrypt the
// payload FIRST, THEN sample the resulting ciphertext to derive the header
// protection mask, THEN apply that mask to the header. Reversing this order
// (protecting the header before the payload exists, or sampling plaintext
// instead of ciphertext) is the single most common QUIC packet-protection
// implementation bug — callers should use this function rather than
// sequencing encrypt_packet_payload/protect_header themselves.
//
// `header` is the complete unprotected header bytes, ending with the
// plaintext packet number encoded in exactly `pn_length` bytes (see
// encode_packet_number). `form` selects which low bits of the first byte
// header protection covers.
pub fn protect_packet(header []u8, form HeaderForm, packet_number u64, pn_length int, payload []u8, keys QuicPacketProtectionKeys) ![]u8 {
	if pn_length < 1 || pn_length > pn_max_length {
		return error('quic: invalid packet number length ${pn_length}, must be 1-4')
	}
	if header.len < pn_length {
		return error('quic: header shorter than its own packet number length')
	}
	ciphertext := encrypt_packet_payload(keys, packet_number, header, payload)!
	mut packet := []u8{cap: header.len + ciphertext.len}
	packet << header
	packet << ciphertext
	pn_offset := header.len - pn_length
	protect_header(mut packet, pn_offset, pn_length, keys.hp, form)!
	return packet
}

// UnprotectedPacket is the result of successfully removing both header and
// packet protection from one received QUIC packet.
pub struct UnprotectedPacket {
pub:
	header        []u8 // the full header, through the now-plaintext packet number
	packet_number u64  // full, reconstructed packet number
	payload       []u8 // decrypted plaintext payload
}

// unprotect_packet reverses `protect_packet` on a received packet: removes
// header protection FIRST — which is the only way to learn the packet's real
// packet-number length and value at all — THEN AEAD-decrypts the payload
// using that recovered packet number and the now-unprotected header as
// associated data. This is the mirror image of protect_packet's ordering,
// not the same steps run backwards; see unprotect_header's own doc comment
// for why header-then-payload is mandatory on this side specifically.
//
// `packet` is mutated in place (header protection removal happens
// destructively). `pn_offset` is where the (still-protected) packet number
// field begins — the caller gets this from parsing the always-visible
// header fields (parse_long_header/parse_short_header) that precede it.
// `largest_pn` is the largest packet number already processed in this
// packet's number space (none if this is the first packet processed in that
// space) — see decode_packet_number.
//
// On AEAD authentication failure, the caller MUST drop the packet silently
// rather than close the connection (see decrypt_packet_payload's doc
// comment) — this function only surfaces the error.
pub fn unprotect_packet(mut packet []u8, pn_offset int, form HeaderForm, keys QuicPacketProtectionKeys, largest_pn ?u64) !UnprotectedPacket {
	pn_length := unprotect_header(mut packet, pn_offset, keys.hp, form)!
	if pn_offset + pn_length > packet.len {
		return error('quic: packet number field runs past end of packet')
	}
	mut truncated := u64(0)
	for i in 0 .. pn_length {
		truncated = (truncated << 8) | u64(packet[pn_offset + i])
	}
	full_pn := decode_packet_number(truncated, pn_length, largest_pn)!
	header := packet[..pn_offset + pn_length].clone()
	ciphertext := unsafe { packet[pn_offset + pn_length..] }
	payload := decrypt_packet_payload(keys, full_pn, header, ciphertext)!
	return UnprotectedPacket{
		header:        header
		packet_number: full_pn
		payload:       payload
	}
}
