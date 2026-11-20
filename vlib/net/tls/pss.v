module tls

import crypto.rand
import crypto.sha256
import crypto.sha512
import crypto.subtle
import math.big

// EMSA-PSS sign/verify (RFC 8017 §9.1) for TLS 1.3 rsa_pss_rsae_* schemes.
// TLS 1.3 (RFC 8446 §4.2.3) fixes the salt length to the hash length, and
// the hash to SHA-256/384/512 matching the scheme. Only the rsae variants
// (salted with the message hash, key-nailed parameters) are implemented;
// rsa_pss_pss_* (restricted keys) are rejected by the caller.

// pss_mgf1 implements MGF1 (RFC 8017 App. B.2.1) with the given hash.
fn pss_mgf1(hash_fn fn ([]u8) []u8, hlen int, seed []u8, out_len int) ![]u8 {
	if out_len > (hlen << 32) {
		return error('tls: PSS mask too long')
	}
	mut out := []u8{cap: out_len}
	mut counter := u32(0)
	for out.len < out_len {
		mut block := []u8{cap: seed.len + 4}
		block << seed
		block << u8(counter >> 24)
		block << u8(counter >> 16)
		block << u8(counter >> 8)
		block << u8(counter)
		out << hash_fn(block)
		counter++
	}
	return out[..out_len]
}

// pss_emsa_encode builds the EMSA-PSS encoded message EM for msg.
fn pss_emsa_encode(hash_fn fn ([]u8) []u8, hlen int, msg []u8, em_bits int, salt []u8) ![]u8 {
	em_len := (em_bits + 7) / 8
	if em_len < hlen + salt.len + 2 {
		return error('tls: RSA key too short for PSS')
	}
	m_hash := hash_fn(msg)
	mut m := []u8{cap: 8 + hlen + salt.len}
	for _ in 0 .. 8 {
		m << u8(0)
	}
	m << m_hash
	m << salt
	h := hash_fn(m)
	ps_len := em_len - hlen - salt.len - 2
	mut db := []u8{cap: em_len - hlen - 1}
	for _ in 0 .. ps_len {
		db << u8(0)
	}
	db << u8(0x01)
	db << salt
	mask := pss_mgf1(hash_fn, hlen, h, em_len - hlen - 1)!
	for i in 0 .. db.len {
		db[i] ^= mask[i]
	}
	// Clear the leftmost 8*emLen - emBits bits of the first octet.
	top_bits := u8(0xff >> (8 * em_len - em_bits))
	db[0] &= top_bits
	mut em := []u8{cap: em_len}
	em << db
	em << h
	em << u8(0xbc)
	return em
}

// pss_hash_fn maps a rsa_pss_rsae_* scheme to its hash function.
fn pss_hash_fn(scheme u16) !fn ([]u8) []u8 {
	return match scheme {
		u16(0x0804) { sha256.sum256 }
		u16(0x0805) { sha512.sum384 }
		u16(0x0806) { sha512.sum512 }
		else { return error('tls: unsupported PSS scheme ${scheme}') }
	}
}

// pss_hash_for_scheme maps a rsa_pss_rsae_* scheme to its RSA hash id.
fn pss_hash_for_scheme(scheme u16) !RsaHash {
	return match scheme {
		u16(0x0804) { RsaHash.sha256 }
		u16(0x0805) { RsaHash.sha384 }
		u16(0x0806) { RsaHash.sha512 }
		else { return error('tls: unsupported PSS scheme ${scheme}') }
	}
}

// rsa_pss_sign signs msg with EMSA-PSS (salt length = hash length).
pub fn rsa_pss_sign(scheme u16, msg []u8, d big.Integer, n big.Integer) ![]u8 {
	hash_fn := pss_hash_fn(scheme) or { return err }
	h := pss_hash_for_scheme(scheme) or { return err }
	hlen := rsa_hash_len(h)
	em_bits := n.bit_len() - 1
	salt := rand.bytes(hlen) or { return err }
	em := pss_emsa_encode(hash_fn, hlen, msg, em_bits, salt)!
	em_len := (em_bits + 7) / 8
	m_int := big.integer_from_bytes(em, signum: 1)
	s_int := m_int.big_mod_pow(d, n)!
	return rsa_int_to_bytes(s_int, em_len)
}

// rsa_pss_verify verifies an EMSA-PSS signature over msg.
pub fn rsa_pss_verify(scheme u16, msg []u8, sig []u8, e u64, n big.Integer) ! {
	hash_fn := pss_hash_fn(scheme) or { return err }
	h := pss_hash_for_scheme(scheme) or { return err }
	hlen := rsa_hash_len(h)
	em_bits := n.bit_len() - 1
	em_len := (em_bits + 7) / 8
	if sig.len != em_len {
		return error(error_bad_signature)
	}
	if em_len < hlen + hlen + 2 {
		return error(error_bad_signature)
	}
	s_int := big.integer_from_bytes(sig, signum: 1)
	m_int := s_int.mod_pow(e, n)
	em := rsa_int_to_bytes(m_int, em_len)!
	if em[em.len - 1] != 0xbc {
		return error(error_bad_signature)
	}
	masked_db := em[..em.len - hlen - 1]
	h_received := em[em.len - hlen - 1..em.len - 1].clone()
	top_bits := u8(0xff >> (8 * em_len - em_bits))
	if masked_db[0] & ~top_bits != 0 {
		return error(error_bad_signature)
	}
	mask := pss_mgf1(hash_fn, hlen, h_received, em_len - hlen - 1)!
	mut db := []u8{cap: masked_db.len}
	for i in 0 .. masked_db.len {
		db << (masked_db[i] ^ mask[i])
	}
	db[0] &= top_bits
	// DB = PS || 0x01 || salt, with PS all zero and salt length = hlen.
	ps_len := em_len - hlen - hlen - 2
	for i in 0 .. ps_len {
		if db[i] != 0 {
			return error(error_bad_signature)
		}
	}
	if db[ps_len] != 0x01 {
		return error(error_bad_signature)
	}
	salt := db[ps_len + 1..].clone()
	expected := pss_emsa_encode(hash_fn, hlen, msg, em_bits, salt)!
	if subtle.constant_time_compare(em, expected) != 1 {
		return error(error_bad_signature)
	}
}
