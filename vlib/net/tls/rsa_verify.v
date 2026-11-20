module tls

import crypto.sha256
import crypto.sha512
import crypto.subtle
import math.big

// RSA signature hash identifiers. Each maps to one TLS 1.3 signature scheme
// (for CertificateVerify) and one X.509 signature algorithm OID (for chain
// verification).
pub enum RsaHash {
	sha256
	sha384
	sha512
}

// rsa_hash_len returns the digest length for an RSA hash.
pub fn rsa_hash_len(h RsaHash) int {
	return match h {
		.sha256 { 32 }
		.sha384 { 48 }
		.sha512 { 64 }
	}
}

// rsa_hash_bytes hashes msg with the given RSA hash.
pub fn rsa_hash_bytes(h RsaHash, msg []u8) []u8 {
	return match h {
		.sha256 { sha256.sum256(msg) }
		.sha384 { sha512.sum384(msg) }
		.sha512 { sha512.sum512(msg) }
	}
}

// rsa_pkcs1_digestinfo_prefix returns the DigestInfo prefix for a hash,
// i.e. everything before the digest in a PKCS#1 v1.5 encoded message.
fn rsa_pkcs1_digestinfo_prefix(h RsaHash) []u8 {
	return match h {
		.sha256 {
			[u8(0x30), 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04,
				0x02, 0x01, 0x05, 0x00, 0x04, 0x20]
		}
		.sha384 {
			[u8(0x30), 0x41, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04,
				0x02, 0x02, 0x05, 0x00, 0x04, 0x30]
		}
		.sha512 {
			[u8(0x30), 0x51, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04,
				0x02, 0x03, 0x05, 0x00, 0x04, 0x40]
		}
	}
}

// rsa_hash_from_oid maps a signature-algorithm SEQUENCE value to its RSA hash
// by the primary OID. Accepted: sha256/384/512WithRSAEncryption. RSA-PSS and
// MD5/SHA-1 OIDs are rejected here (PSS has its own path; weak hashes are
// never accepted).
pub fn rsa_hash_from_oid(sigalg_val []u8) !RsaHash {
	if der_first_oid_is(sigalg_val, [u8(0x2a), 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b]) {
		return .sha256
	}
	if der_first_oid_is(sigalg_val, [u8(0x2a), 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0c]) {
		return .sha384
	}
	if der_first_oid_is(sigalg_val, [u8(0x2a), 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0d]) {
		return .sha512
	}
	return error('tls: unsupported RSA signature algorithm')
}

// rsa_pkcs1_encode builds the PKCS#1 v1.5 encoded message EM for a digest.
fn rsa_pkcs1_encode(digest []u8, h RsaHash, k int) ![]u8 {
	prefix := rsa_pkcs1_digestinfo_prefix(h)
	t_len := prefix.len + digest.len
	if k < t_len + 11 {
		return error('tls: RSA key too short for PKCS#1 v1.5')
	}
	mut em := []u8{}
	em << u8(0x00)
	em << u8(0x01)
	for _ in 0 .. k - t_len - 3 {
		em << u8(0xff)
	}
	em << u8(0x00)
	for b in prefix {
		em << b
	}
	for b in digest {
		em << b
	}
	return em
}

// rsa_int_to_bytes converts an Integer to exactly k big-endian bytes.
fn rsa_int_to_bytes(m big.Integer, k int) ![]u8 {
	m_bytes, _ := m.bytes()
	if m_bytes.len > k {
		return error('tls: RSA value out of range')
	}
	mut em := []u8{len: k}
	mut start := k - m_bytes.len
	for i in 0 .. m_bytes.len {
		em[start + i] = m_bytes[i]
	}
	return em
}

// rsa_pkcs1_verify_digest verifies a PKCS#1 v1.5 signature over a digest.
pub fn rsa_pkcs1_verify_digest(digest []u8, sig []u8, n big.Integer, e u64, h RsaHash) ! {
	k := (n.bit_len() + 7) / 8
	if sig.len != k {
		return error(error_bad_signature)
	}
	s_int := big.integer_from_bytes(sig, signum: 1)
	m_int := s_int.mod_pow(e, n)
	em := rsa_int_to_bytes(m_int, k)!
	expected := rsa_pkcs1_encode(digest, h, k)!
	if subtle.constant_time_compare(em, expected) != 1 {
		return error(error_bad_signature)
	}
}

// rsa_pkcs1_sign_digest signs a digest with a private exponent.
pub fn rsa_pkcs1_sign_digest(digest []u8, d big.Integer, n big.Integer, h RsaHash) ![]u8 {
	k := (n.bit_len() + 7) / 8
	em := rsa_pkcs1_encode(digest, h, k)!
	m_int := big.integer_from_bytes(em, signum: 1)
	s_int := m_int.big_mod_pow(d, n)!
	return rsa_int_to_bytes(s_int, k)
}

// verify_cert_signature_rsa verifies an RSA PKCS#1 v1.5 (SHA-256/384/512)
// certificate signature.
// verify_cert_signature_rsa dispatches on the certificate sigAlg OID and
// rejects anything else, including RSA-PSS and SHA-1/MD5.
pub fn verify_cert_signature_rsa(child_der []u8, parent_rsa_der []u8) ! {
	pmod, pexp := parse_rsa_pubkey(parent_rsa_der) or { return err }
	// parse child cert
	tag, cert_val, _ := der_tlv(child_der) or { return err }
	if tag != 0x30 {
		return error('tls: expected cert SEQUENCE')
	}
	// tbs raw bytes
	mut off := 0
	ttag, _, tconsumed := der_tlv(cert_val) or { return err }
	if ttag != 0x30 {
		return error('tls: expected tbs')
	}
	tbs_raw := cert_val[..tconsumed].clone()
	off = tconsumed
	_, sigalg_val, sa_consumed := der_tlv(cert_val[off..]) or { return err }
	h := rsa_hash_from_oid(sigalg_val) or { return err }
	off += sa_consumed
	// signature BIT STRING
	stag, sig_val, _ := der_tlv(cert_val[off..]) or { return err }
	if stag != 0x03 {
		return error('tls: expected BIT STRING')
	}
	if sig_val.len < 1 || sig_val[0] != 0 {
		return error('tls: bad sig padding')
	}
	sig := sig_val[1..].clone()

	digest := rsa_hash_bytes(h, tbs_raw)
	rsa_pkcs1_verify_digest(digest, sig, pmod, pexp, h) or { return err }
}

// RsaPrivateKey holds RSA private numbers for server signing.
pub struct RsaPrivateKey {
pub:
	n big.Integer
	e u64
	d big.Integer
}

// rsa_integer parses a DER INTEGER into a big Integer, stripping-sign safe.
fn rsa_integer(content []u8, off int) !(big.Integer, int) {
	tag, val, consumed := der_tlv(content[off..]) or { return err }
	if tag != 0x02 {
		return error('tls: expected INTEGER in RSA key')
	}
	mut slice := val.clone()
	if slice.len > 1 && slice[0] == 0 {
		slice = slice[1..].clone()
	}
	if slice.len == 0 {
		return error('tls: empty INTEGER in RSA key')
	}
	return big.integer_from_bytes(slice, signum: 1), off + consumed
}

// parse_rsa_private_key parses a PKCS#1 RSAPrivateKey or PKCS#8 PrivateKeyInfo
// DER blob into RSA private numbers (n, e, d).
pub fn parse_rsa_private_key(der []u8) !RsaPrivateKey {
	// Try PKCS#8 first: SEQUENCE { version, algId, OCTET STRING }.
	tag, content, _ := der_tlv(der) or { return err }
	if tag != 0x30 {
		return error('tls: expected SEQUENCE for RSA private key')
	}
	_, first, first_consumed := der_tlv(content) or { return err }
	_ = first
	// PKCS#8 has an AlgorithmIdentifier SEQUENCE as its second element;
	// PKCS#1 has the modulus INTEGER there instead.
	_, second, _ := der_tlv(content[first_consumed..]) or { return err }
	_ = second
	// Peek at the second element tag to distinguish the two formats.
	stag, _, _ := der_tlv(content[first_consumed..]) or { return err }
	if stag == 0x30 {
		// PKCS#8: skip version + algId, unwrap the OCTET STRING.
		mut off := 0
		_, _, vn := der_tlv(content[off..]) or { return err }
		off += vn
		_, alg_val, an := der_tlv(content[off..]) or { return err }
		_ = alg_val
		off += an
		ktag, key_der, _ := der_tlv(content[off..]) or { return err }
		if ktag != 0x04 {
			return error('tls: invalid PKCS#8 private key')
		}
		return parse_rsa_pkcs1(key_der)
	}
	return parse_rsa_pkcs1(der)
}

// parse_rsa_pkcs1 parses a PKCS#1 RSAPrivateKey DER blob.
fn parse_rsa_pkcs1(der []u8) !RsaPrivateKey {
	tag, content, _ := der_tlv(der) or { return err }
	if tag != 0x30 {
		return error('tls: expected SEQUENCE for RSA private key')
	}
	mut off := 0
	// version
	_, _, vn := der_tlv(content[off..]) or { return err }
	off += vn
	n, o1 := rsa_integer(content, off) or { return err }
	off = o1
	e_int, o2 := rsa_integer(content, off) or { return err }
	off = o2
	d, _ := rsa_integer(content, off) or { return err }
	e_bytes, _ := e_int.bytes()
	if e_bytes.len > 8 {
		return error('tls: RSA exponent too large')
	}
	mut e_val := u64(0)
	for b in e_bytes {
		e_val = (e_val << 8) | u64(b)
	}
	return RsaPrivateKey{
		n: n
		e: e_val
		d: d
	}
}

fn parse_rsa_pubkey(der []u8) !(big.Integer, u64) {
	tag, content, _ := der_tlv(der) or { return err }
	if tag != 0x30 {
		return error('tls: expected SEQUENCE')
	}
	// modulus
	mtag, mod_bytes, mod_consumed := der_tlv(content) or { return err }
	if mtag != 0x02 {
		return error('tls: expected INTEGER modulus')
	}
	// strip leading zero
	mut mod_slice := mod_bytes.clone()
	if mod_slice.len > 1 && mod_slice[0] == 0 {
		mod_slice = mod_slice[1..].clone()
	}
	n := big.integer_from_bytes(mod_slice, signum: 1)
	// exponent
	etag, exp_bytes, _ := der_tlv(content[mod_consumed..]) or { return err }
	if etag != 0x02 {
		return error('tls: expected INTEGER exponent')
	}
	mut exp_val := u64(0)
	for b in exp_bytes {
		exp_val = (exp_val << 8) | u64(b)
	}
	return n, exp_val
}
