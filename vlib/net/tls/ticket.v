module tls

import crypto.aes
import crypto.hkdf
import crypto.rand
import crypto.sha256
import crypto.sha512
import crypto.subtle
import time

// Session tickets and PSK resumption (RFC 8446 §4.2.11, §4.6.1).
//
// Wire result after a full handshake: the server sends one NewSessionTicket
// (encrypted with the server application traffic keys). The ticket itself is
// an AES-256-GCM box holding the resumption master secret, so servers stay
// stateless; the ticket key lives in Config.ticket_key (generated at listen()
// when empty and reused for every connection on that listener).
// The client keeps the ticket + derived PSK in SessionState and offers it on
// the next connection via Config.resume_from. No early data (0-RTT) is sent:
// tickets only authenticate the resumed handshake (psk_dhe_ke).

pub const ext_pre_shared_key = u16(41)
pub const ext_psk_key_exchange_modes = u16(45)

pub const psk_mode_dhe_ke = u8(1)

// ticket_lifetime is how long a ticket stays valid (24h, RFC 8446 §4.6.1
// caps at 7 days).
pub const ticket_lifetime = u32(86400)

// SessionState is what a client keeps from one connection to resume the next.
// ticket is the opaque server blob, psk the derived pre-shared key, suite the
// negotiated suite that must be offered again for binder verification.
pub struct SessionState {
pub mut:
	ticket       []u8
	psk          []u8
	suite        CipherSuite
	created_unix i64
	lifetime     u32
	age_add      u32
}

// NewSessionTicket is the parsed ticket message body (RFC 8446 §4.6.1).
pub struct NewSessionTicket {
pub:
	lifetime u32
	age_add  u32
	nonce    []u8
	ticket   []u8
}

// encode_new_session_ticket encodes a NewSessionTicket body.
pub fn encode_new_session_ticket(lifetime u32, age_add u32, nonce []u8, ticket []u8) ![]u8 {
	if nonce.len > 255 {
		return error('tls: ticket nonce too long')
	}
	if ticket.len == 0 || ticket.len > 0xffff {
		return error('tls: ticket length out of range')
	}
	mut out := []u8{}
	out << u8(lifetime >> 24)
	out << u8(lifetime >> 16)
	out << u8(lifetime >> 8)
	out << u8(lifetime)
	out << u8(age_add >> 24)
	out << u8(age_add >> 16)
	out << u8(age_add >> 8)
	out << u8(age_add)
	out << u8(nonce.len)
	out << nonce
	out << u8(ticket.len >> 8)
	out << u8(ticket.len)
	out << ticket
	// empty extensions
	out << u8(0)
	out << u8(0)
	return out
}

// parse_new_session_ticket parses a NewSessionTicket body.
pub fn parse_new_session_ticket(body []u8) !NewSessionTicket {
	if body.len < 12 {
		return error('tls: truncated ticket message')
	}
	lifetime := (u32(body[0]) << 24) | (u32(body[1]) << 16) | (u32(body[2]) << 8) | u32(body[3])
	age_add := (u32(body[4]) << 24) | (u32(body[5]) << 16) | (u32(body[6]) << 8) | u32(body[7])
	nonce_len := int(body[8])
	if 9 + nonce_len + 2 > body.len {
		return error('tls: truncated ticket nonce')
	}
	nonce := body[9..9 + nonce_len].clone()
	mut off := 9 + nonce_len
	ticket_len := (int(body[off]) << 8) | int(body[off + 1])
	off += 2
	if off + ticket_len + 2 > body.len {
		return error('tls: truncated ticket')
	}
	ticket := body[off..off + ticket_len].clone()
	off += ticket_len
	ext_len := (int(body[off]) << 8) | int(body[off + 1])
	off += 2
	if off + ext_len != body.len {
		return error('tls: trailing data after ticket extensions')
	}
	return NewSessionTicket{
		lifetime: lifetime
		age_add:  age_add
		nonce:    nonce
		ticket:   ticket
	}
}

// seal_ticket encrypts a resumption master secret into an opaque ticket blob.
// Layout: suite(2) || created_unix(8) || age_add(4) || nonce_len(1) || nonce
// || master_len(1) || master.
pub fn seal_ticket(master []u8, suite CipherSuite, age_add u32, nonce []u8, key []u8) ![]u8 {
	if key.len != 32 {
		return error('tls: invalid ticket key length')
	}
	if nonce.len == 0 || nonce.len > 255 || master.len == 0 || master.len > 255 {
		return error('tls: ticket material out of range')
	}
	mut plain := []u8{}
	plain << u8(int(suite) >> 8)
	plain << u8(int(suite))
	now := time.now().unix()
	for i in 0 .. 8 {
		plain << u8((u64(now) >> (8 * (7 - i))) & 0xff)
	}
	plain << u8(age_add >> 24)
	plain << u8(age_add >> 16)
	plain << u8(age_add >> 8)
	plain << u8(age_add)
	plain << u8(nonce.len)
	plain << nonce
	plain << u8(master.len)
	plain << master
	ticket_nonce := rand.bytes(12)!
	gcm := aes.new_aes_gcm(key)!
	ct := gcm.encrypt(plain, ticket_nonce, []u8{})!
	mut out := []u8{}
	out << ticket_nonce
	out << ct
	return out
}

// open_ticket decrypts a ticket blob, returning the resumption master secret,
// ticket nonce, suite, creation time and age_add.
pub fn open_ticket(blob []u8, key []u8) !([]u8, []u8, CipherSuite, i64, u32) {
	if key.len != 32 {
		return error('tls: invalid ticket key length')
	}
	if blob.len < 12 + 2 + 8 + 4 + 1 + 16 {
		return error('tls: truncated ticket blob')
	}
	nonce := blob[..12].clone()
	ct := blob[12..].clone()
	gcm := aes.new_aes_gcm(key)!
	plain := gcm.decrypt(ct, nonce, []u8{})!
	if plain.len < 16 {
		return error('tls: truncated ticket plaintext')
	}
	suite_val := (int(plain[0]) << 8) | int(plain[1])
	if suite_val != int(CipherSuite.tls_aes_128_gcm_sha256)
		&& suite_val != int(CipherSuite.tls_aes_256_gcm_sha384)
		&& suite_val != int(CipherSuite.tls_chacha20_poly1305_sha256) {
		return error('tls: unknown ticket suite')
	}
	mut created_u := u64(0)
	for i in 0 .. 8 {
		created_u = (created_u << 8) | u64(plain[2 + i])
	}
	created := i64(created_u)
	age_add := (u32(plain[10]) << 24) | (u32(plain[11]) << 16) | (u32(plain[12]) << 8) | u32(plain[13])
	nlen := int(plain[14])
	if 15 + nlen + 1 > plain.len {
		return error('tls: corrupt ticket nonce')
	}
	ticket_nonce := plain[15..15 + nlen].clone()
	mlen := int(plain[15 + nlen])
	if 15 + nlen + 1 + mlen != plain.len {
		return error('tls: corrupt ticket plaintext')
	}
	master := plain[15 + nlen + 1..].clone()
	return master, ticket_nonce, unsafe { CipherSuite(suite_val) }, created, age_add
}

// derive_resumption_master derives the resumption master secret (RFC 8446 §7.5).
pub fn derive_resumption_master(master_secret []u8, transcript_hash []u8, suite CipherSuite) ![]u8 {
	return derive_secret_for_suite(master_secret, 'res master', transcript_hash, suite)
}

// derive_psk derives the PSK bound to a ticket nonce (RFC 8446 §4.6.1).
pub fn derive_psk(resumption_master []u8, nonce []u8, suite CipherSuite) ![]u8 {
	return hkdf_expand_label_for_suite(resumption_master, 'resumption', nonce,
		cipher_suite_hash_len(suite), suite)
}

// derive_early_secret_with_psk derives the early secret from a PSK (RFC 8446 §7.1).
pub fn derive_early_secret_with_psk(psk []u8, suite CipherSuite) ![]u8 {
	if suite == .tls_aes_256_gcm_sha384 {
		return hkdf.extract(sha512.new384, psk, []u8{})
	}
	return hkdf.extract(sha256.new, psk, []u8{})
}

// binder_key_for derives the binder key for a PSK early secret ("res binder").
pub fn binder_key_for(early_secret []u8, suite CipherSuite) ![]u8 {
	empty := empty_transcript_hash_for_suite(suite)
	return derive_secret_for_suite(early_secret, 'res binder', empty, suite)
}

// compute_binder computes the PSK binder over the truncated ClientHello.
pub fn compute_binder(binder_key []u8, truncated_ch_hash []u8, suite CipherSuite) ![]u8 {
	return finished_verify_data_for_suite(binder_key, truncated_ch_hash, suite)
}

// verify_binder checks a client binder in constant time.
pub fn verify_binder(binder_key []u8, truncated_ch_hash []u8, offered []u8, suite CipherSuite) bool {
	expected := compute_binder(binder_key, truncated_ch_hash, suite) or { return false }
	if expected.len != offered.len {
		return false
	}
	return subtle.constant_time_compare(expected, offered) == 1
}

// ---------- pre_shared_key extension codecs (RFC 8446 §4.2.11) ----------

// encode_psk_modes returns the psk_key_exchange_modes extension data offering psk_dhe_ke.
pub fn encode_psk_modes() []u8 {
	return [u8(1), psk_mode_dhe_ke]
}

// parse_psk_modes reports whether the client offers psk_dhe_ke.
pub fn parse_psk_modes(data []u8) bool {
	if data.len < 2 {
		return false
	}
	mlen := int(data[0])
	if 1 + mlen != data.len {
		return false
	}
	for i in 0 .. mlen {
		if data[1 + i] == psk_mode_dhe_ke {
			return true
		}
	}
	return false
}

// PskIdentity is one offered ticket identity.
pub struct PskIdentity {
pub:
	ticket         []u8
	obfuscated_age u32
}

// encode_psk_offer builds the pre_shared_key extension data for one identity
// with a placeholder binder (zeros of hash length for suite).
pub fn encode_psk_offer(ticket []u8, obfuscated_age u32, suite CipherSuite) []u8 {
	hash_len := cipher_suite_hash_len(suite)
	mut identities := []u8{}
	id_len := ticket.len
	identities << u8((2 + id_len + 4) >> 8)
	identities << u8((2 + id_len + 4) & 0xff)
	identities << u8(id_len >> 8)
	identities << u8(id_len & 0xff)
	identities << ticket
	identities << u8(obfuscated_age >> 24)
	identities << u8(obfuscated_age >> 16)
	identities << u8(obfuscated_age >> 8)
	identities << u8(obfuscated_age)
	mut binders := []u8{}
	binders << u8((1 + hash_len) >> 8)
	binders << u8((1 + hash_len) & 0xff)
	binders << u8(hash_len)
	for _ in 0 .. hash_len {
		binders << u8(0)
	}
	mut out := []u8{}
	out << identities
	out << binders
	return out
}

// parse_psk_offer parses a client pre_shared_key extension, returning identities
// and binder values.
pub fn parse_psk_offer(data []u8) !([]PskIdentity, [][]u8) {
	if data.len < 4 {
		return error('tls: truncated psk offer')
	}
	ids_len := (int(data[0]) << 8) | int(data[1])
	if 2 + ids_len + 2 > data.len {
		return error('tls: truncated psk identities')
	}
	mut ids := []PskIdentity{}
	mut off := 2
	for off < 2 + ids_len {
		if off + 2 > data.len {
			return error('tls: truncated psk identity')
		}
		ilen := (int(data[off]) << 8) | int(data[off + 1])
		off += 2
		if off + ilen + 4 > data.len {
			return error('tls: truncated psk ticket')
		}
		ticket := data[off..off + ilen].clone()
		off += ilen
		age := (u32(data[off]) << 24) | (u32(data[off + 1]) << 16) | (u32(data[off + 2]) << 8) | u32(data[off + 3])
		off += 4
		ids << PskIdentity{
			ticket:         ticket
			obfuscated_age: age
		}
	}
	off = 2 + ids_len
	blen := (int(data[off]) << 8) | int(data[off + 1])
	off += 2
	if off + blen != data.len {
		return error('tls: trailing data in psk binders')
	}
	mut binders := [][]u8{}
	for off < data.len {
		llen := int(data[off])
		off += 1
		if off + llen > data.len {
			return error('tls: truncated psk binder')
		}
		binders << data[off..off + llen].clone()
		off += llen
	}
	return ids, binders
}

// patch_psk_binder replaces the (single) placeholder binder in encoded extension
// data with the real binder value.
pub fn patch_psk_binder(ext_data []u8, binder []u8) ![]u8 {
	ids, _ := parse_psk_offer(ext_data)!
	if ids.len != 1 {
		return error('tls: only single-psk offers are supported')
	}
	hash_len := binder.len
	mut out := ext_data.clone()
	// binder entry sits at the very end: [len(1)=hash_len][binder].
	if out.len < 1 + hash_len {
		return error('tls: corrupt psk offer')
	}
	start := out.len - hash_len
	for i in 0 .. hash_len {
		out[start + i] = binder[i]
	}
	return out
}

// binder_prefix returns the exact wire bytes a PSK binder covers: the full
// ClientHello handshake message with the binders list chopped off the end,
// length fields untouched (RFC 8446 §4.2.11.2 as implemented by OpenSSL and
// peers). The cut length comes from the identities vector in the offered
// pre_shared_key extension. The input must be the raw wire bytes: unknown
// cipher suites and GREASE values elsewhere in the message must be preserved
// bit-for-bit, which re-encoding a parsed message would not do.
pub fn binder_prefix(full_msg []u8, psk_data []u8) ![]u8 {
	if psk_data.len < 2 {
		return error('tls: no psk offer to truncate')
	}
	ids_len := (int(psk_data[0]) << 8) | int(psk_data[1])
	if 2 + ids_len > psk_data.len {
		return error('tls: truncated psk identities')
	}
	cut := psk_data.len - (2 + ids_len)
	if cut <= 0 || cut >= full_msg.len {
		return error('tls: bad psk binder truncation')
	}
	return full_msg[..full_msg.len - cut].clone()
}

// build_new_session_ticket derives the resumption master secret and seals one
// NewSessionTicket handshake message. The nonce and age_add are random per
// ticket; lifetime is ticket_lifetime.
pub fn build_new_session_ticket(master_secret []u8, transcript_hash []u8, suite CipherSuite, ticket_key []u8) ![]u8 {
	res_master := derive_resumption_master(master_secret, transcript_hash, suite)!
	nonce := rand.bytes(8)!
	age_bytes := rand.bytes(4)!
	age_add := (u32(age_bytes[0]) << 24) | (u32(age_bytes[1]) << 16) | (u32(age_bytes[2]) << 8) | u32(age_bytes[3])
	blob := seal_ticket(res_master, suite, age_add, nonce, ticket_key)!
	body := encode_new_session_ticket(ticket_lifetime, age_add, nonce, blob)!
	return encode_handshake_message(.new_session_ticket, body)
}

// client_psk_from_ticket processes a received NewSessionTicket, deriving the
// PSK the client stores for resumption.
pub fn client_psk_from_ticket(master_secret []u8, transcript_hash []u8, nst NewSessionTicket, suite CipherSuite, now_unix i64) !SessionState {
	res_master := derive_resumption_master(master_secret, transcript_hash, suite)!
	psk := derive_psk(res_master, nst.nonce, suite)!
	return SessionState{
		ticket:       nst.ticket.clone()
		psk:          psk
		suite:        suite
		created_unix: now_unix
		lifetime:     nst.lifetime
		age_add:      nst.age_add
	}
}

// parse_psk_selected parses the server selected identity.
pub fn parse_psk_selected(data []u8) !int {
	if data.len != 2 {
		return error('tls: bad psk selected identity')
	}
	return (int(data[0]) << 8) | int(data[1])
}

// encode_psk_selected builds the server pre_shared_key extension (selected identity 0).
pub fn encode_psk_selected() []u8 {
	return [u8(0), u8(0)]
}
