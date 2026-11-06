// Copyright (c) 2026 blackshirt.
//
// rsa.v decodes PKCS #1 RSAPublicKey DER and verifies RSASSA-PKCS1-v1_5 and
// RSASSA-PSS chain signatures. ECDSA and Ed25519 dispatch to crypto.* packages.
module pki

import math.big
import encoding.hex
import crypto.sha256
import crypto.sha512
import crypto.subtle
import tls.wire

// RsaPublicKey represents a decoded RSA public key; populated only by the parser.
// @[noinit] blocks a fabricated zero-value key.
@[noinit]
pub struct RsaPublicKey {
mut:
	n          big.Integer // Modulus
	e          u64         // Public exponent (usually 65537)
	size_bytes int         // key size in bytes (modulus length), used to bound signatures
}

// max_rsa_modulus_bytes bounds the accepted RSA modulus (8192 bits) to avoid CPU DoS.
const max_rsa_modulus_bytes = 1024

// parse_rsa_public_key parses an RSA public key from ASN.1 DER (PKCS #1 RSAPublicKey).
// RSAPublicKey ::= SEQUENCE { modulus INTEGER, publicExponent INTEGER }
@[direct_array_access]
fn parse_rsa_public_key(der []u8) !RsaPublicKey {
	// der is the full DER-encoded SEQUENCE (tag 0x30 + length + contents).
	// Unwrap the outer SEQUENCE to get its contents, then parse the two INTEGERs.
	mut r := wire.new_reader(der)
	seq_elem := read_asn1_element(mut r)!
	if seq_elem.tag != 0x30 {
		return error('rsa: expected SEQUENCE tag 0x30, got 0x${seq_elem.tag:02x}')
	}
	elements := parse_sequence(seq_elem.data)!
	if elements.len < 2 {
		return error('rsa: invalid RSAPublicKey sequence length (${elements.len})')
	}
	n_elem := elements[0]
	e_elem := elements[1]

	mut n_bytes := n_elem.data.clone()
	// Strip leading zero byte if present (ASN.1 positive integer sign indicator).
	if n_bytes.len > 0 && n_bytes[0] == 0 {
		n_bytes = n_bytes[1..].clone()
	}
	if n_bytes.len == 0 {
		return error('rsa: empty modulus')
	}
	// Reject an oversized modulus before hex/big-integer import (CPU DoS guard).
	if n_bytes.len > max_rsa_modulus_bytes {
		return error('rsa: modulus ${n_bytes.len * 8} bits exceeds maximum supported ${max_rsa_modulus_bytes * 8} bits')
	}

	n_hex := hex.encode(n_bytes)
	// hex string as intermediate: V's big.Integer consumes radix strings, not raw byte slices.
	n := big.integer_from_radix(n_hex, 16) or {
		return error('rsa: failed to parse modulus: ${err.msg()}')
	}

	if e_elem.data.len > 8 {
		return error('rsa: public exponent exceeds 64 bits')
	}
	mut e := u64(0)
	for b in e_elem.data {
		e = (e << 8) | u64(b)
	}
	if e == 0 {
		return error('rsa: public exponent must be non-zero')
	}
	// RFC 8017 §5.1.3 / FIPS 186-5: a valid public exponent is odd and >= 3;
	// e == 1 makes RSAVP1 the identity (universal forgery).
	if e == 1 {
		return error('rsa: public exponent 1 is degenerate')
	}
	// Any even exponent is rejected (not invertible mod n).
	if e % 2 == 0 {
		return error('rsa: public exponent ${e} must be odd')
	}

	return RsaPublicKey{
		n:          n
		e:          e
		size_bytes: n_bytes.len
	}
}

// rsa_verify_raw computes m = s^e mod n (RFC 8017 §5.2.1); result is left-zero-padded to key size.
fn (k &RsaPublicKey) rsa_verify_raw(sig []u8) ![]u8 {
	if sig.len != k.size_bytes {
		return error('rsa: signature length (${sig.len}) does not match key size (${k.size_bytes})')
	}
	s_hex := hex.encode(sig)
	s := big.integer_from_radix(s_hex, 16) or {
		return error('rsa: failed to parse signature: ${err.msg()}')
	}
	if s >= k.n {
		return error('rsa: signature representative out of range')
	}

	// RFC 8017 §5.2.1 (RSAVP1): m = s^e mod n; all inputs public so not constant-time.
	m := s.mod_pow(k.e, k.n)
	mut m_hex := m.radix_str(16)
	if m_hex.len % 2 != 0 {
		m_hex = '0' + m_hex
	}
	mut m_bytes := hex.decode(m_hex) or {
		return error('rsa: failed to decode decrypted message: ${err.msg()}')
	}

	// Left-pad with zeros to match key size
	if m_bytes.len < k.size_bytes {
		pad_len := k.size_bytes - m_bytes.len
		mut padded := []u8{len: pad_len, init: 0x00}
		padded << m_bytes
		m_bytes = padded.clone()
	}
	return m_bytes
}

// mgf1 implements Mask Generation Function 1 (RFC 8017 Appendix B.2.1).
fn mgf1(seed []u8, mask_len int, hash_fn fn ([]u8) []u8) []u8 {
	mut out := []u8{cap: mask_len}
	mut counter := u32(0)
	for out.len < mask_len {
		mut data := []u8{cap: seed.len + 4}
		data << seed
		data << u8((counter >> 24) & 0xff)
		data << u8((counter >> 16) & 0xff)
		data << u8((counter >> 8) & 0xff)
		data << u8(counter & 0xff)

		digest := hash_fn(data)
		// RFC 8017 §B.2.1 step 2c: concatenate full hash blocks, then truncate to exactly mask_len.
		needed := mask_len - out.len
		to_take := if needed < digest.len { needed } else { digest.len }
		out << digest[..to_take]
		counter++
	}
	return out
}

// digest_bytes hashes data with the named digest ("sha256"/"sha384"/"sha512").
fn digest_bytes(digest_type string, data []u8) ![]u8 {
	return match digest_type {
		'sha256' { sha256.sum256(data) }
		'sha384' { sha512.sum384(data) }
		'sha512' { sha512.sum512(data) }
		else { error('pki: unsupported digest ${digest_type}') }
	}
}

// mgf1_for runs MGF1 under the hash named by digest_type (RFC 8017 §9.1.1).
fn mgf1_for(digest_type string, seed []u8, mask_len int) ![]u8 {
	mask := match digest_type {
		'sha256' { mgf1(seed, mask_len, sha256.sum256) }
		'sha384' { mgf1(seed, mask_len, sha512.sum384) }
		'sha512' { mgf1(seed, mask_len, sha512.sum512) }
		else {
			return error('pki: unsupported digest ${digest_type}')
		}
	}
	return mask
}

// verify_pss verifies an RSASSA-PSS signature (RFC 8017 §8.1.2 & §9.1.2),
// TLS 1.3 first (RFC 8446 §4.2.3).
pub fn (k &RsaPublicKey) verify_pss(digest_type string, m_hash []u8, sig []u8) !bool {
	h_len := match digest_type {
		'sha256' { 32 }
		'sha384' { 48 }
		'sha512' { 64 }
		else {
			return error('rsa: unsupported digest ${digest_type}')
		}
	}
	s_len := h_len // In TLS 1.3, salt length matches hash output length (RFC 8446 §4.2.3)
	// Hard-codes the saltLen == hLen convention (RFC 4055 §5.1 permits any length);
	// essentially every TLS-era TLS signer uses salt == hash length.
	// rsa_verify_raw gives EM = maskedDB || H || 0xbc, maskedDB being emLen - hLen
	// - 1 bytes and H the hLen-byte hash.
	em := k.rsa_verify_raw(sig)!
	em_len := em.len

	if em_len < h_len + s_len + 2 {
		return error('rsa-pss: encoding error: em_len too small')
	}

	// 1. Check rightmost octet is 0xbc
	if em[em_len - 1] != 0xbc {
		return error('rsa-pss: invalid trailer field (expected 0xbc, got 0x${em[em_len - 1]:02x})')
	}

	// 2. Extract maskedDB and H
	masked_db := em[..em_len - h_len - 1].clone()
	h := em[em_len - h_len - 1..em_len - 1].clone()

	// 3. Top bits check: top bit of EM must be 0.
	// RFC 8017 §9.2 uses emBits = modBits − 1 (m < n forces the top bit clear), so
	// the count of forced zero bits in maskedDB[0] is 8*emLen − (modBits − 1).
	mod_bits := k.size_bytes * 8
	top_zero_bits := 8 * em_len - (mod_bits - 1)
	if top_zero_bits > 0 && top_zero_bits <= 8 {
		mask := u8(0xff) << (8 - top_zero_bits)
		if (masked_db[0] & mask) != 0 {
			return error('rsa-pss: inconsistent top bits')
		}
	}

	// 4. Compute dbMask = MGF1(H, emLen - hLen - 1) under the suite's hash
	db_mask := mgf1_for(digest_type, h, masked_db.len)!

	// 5. Recover DB = maskedDB ^ dbMask
	mut db := []u8{len: masked_db.len}
	for i in 0 .. masked_db.len {
		db[i] = masked_db[i] ^ db_mask[i]
	}

	// Clear top zero bits in DB
	if top_zero_bits > 0 && top_zero_bits <= 8 {
		mask := ~u8(0xff << (8 - top_zero_bits))
		db[0] &= mask
	}

	// 6. Check padding in DB: leftmost emLen - hLen - sLen - 2 must be 0x00, followed by 0x01
	pad_zeros_len := em_len - h_len - s_len - 2
	for i in 0 .. pad_zeros_len {
		if db[i] != 0x00 {
			return error('rsa-pss: inconsistent DB zero padding at byte ${i}')
		}
	}
	if db[pad_zeros_len] != 0x01 {
		return error('rsa-pss: inconsistent DB separator byte (expected 0x01, got 0x${db[pad_zeros_len]:02x})')
	}

	// 7. Extract salt
	salt := db[db.len - s_len..].clone()

	// 8. Compute H_prime = Hash(0x00 * 8 || mHash || salt).
	// The 8 zero octets are EMSA-PSS's fixed "length mask" prefix (RFC 8017 §9.2),
	// domain-separating this hash from a plain message hash.
	mut m_prime := []u8{cap: 8 + m_hash.len + salt.len}
	for _ in 0 .. 8 {
		m_prime << 0x00
	}
	m_prime << m_hash
	m_prime << salt

	h_prime := digest_bytes(digest_type, m_prime)!

	// 9. Compare H and H_prime in constant time
	// (defensive hygiene: the comparison is against an attacker-influenced value).
	if subtle.constant_time_compare(h, h_prime) != 1 {
		return error('rsa-pss: signature verification failed (digest mismatch)')
	}

	return true
}

// verify_pkcs1_v1_5 verifies an RSASSA-PKCS1-v1_5 signature (RFC 8017 §8.2.2).
// DigestInfo is matched as one whole byte string (DER prefix + hash) rather than
// parsed: permissive prefix handling enabled Bleichenbacher-style forgeries, so
// the exact canonical encoding must appear byte-for-byte or be rejected.
fn (k &RsaPublicKey) verify_pkcs1_v1_5(digest_type string, m_hash []u8, sig []u8) !bool {
	em := k.rsa_verify_raw(sig)!
	if em.len < 11 {
		return error('rsa-pkcs1: message too short')
	}
	if em[0] != 0x00 || em[1] != 0x01 {
		return error('rsa-pkcs1: invalid prefix (expected 0x00 0x01)')
	}

	// Canonical DER DigestInfo prefix: SEQUENCE{ SEQUENCE{ OID(digest), NULL },
	// OCTET STRING(hLen) } per RFC 8017 §2.3.1 — the last byte (0x20/0x30/0x40) is
	// the hash length. Appending m_hash yields the expected EM tail verbatim.
	prefix := match digest_type {
		'sha256' {
			[u8(0x30), 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04,
				0x02, 0x01, 0x05, 0x00, 0x04, 0x20]
		}
		'sha384' {
			[u8(0x30), 0x41, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04,
				0x02, 0x02, 0x05, 0x00, 0x04, 0x30]
		}
		'sha512' {
			[u8(0x30), 0x51, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04,
				0x02, 0x03, 0x05, 0x00, 0x04, 0x40]
		}
		else {
			return error('rsa-pkcs1: unsupported digest')
		}
	}

	mut di := []u8{cap: prefix.len + m_hash.len}
	di << prefix
	di << m_hash

	if em.len < 3 + di.len {
		return error('rsa-pkcs1: em too short for DigestInfo')
	}

	mut sep_idx := -1
	// Scan from index 2 (0x00 0x01 already validated above): PS must be all 0xFF
	// up to the first 0x00 separator; any other byte is malformed padding.
	for i in 2 .. em.len {
		if em[i] == 0x00 {
			sep_idx = i
			break
		}
		if em[i] != 0xff {
			return error('rsa-pkcs1: invalid PS padding byte (expected 0xff, got 0x${em[i]:02x})')
		}
	}

	// Also catches sep_idx == -1 (no separator). Separator at index ≥ 10 ⇒ PS
	// length = sep_idx - 2 ≥ 8 (RFC 8017 §8.2.2 step 3: EM length ≥ 11).
	if sep_idx < 10 {
		return error('rsa-pkcs1: PS padding too short (must be at least 8 bytes)')
	}

	got_digest_info := em[sep_idx + 1..]
	if got_digest_info.len != di.len || subtle.constant_time_compare(got_digest_info, di) != 1 {
		return error('rsa-pkcs1: DigestInfo mismatch')
	}
	return true
}
