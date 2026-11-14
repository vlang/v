module tls

import math.big
import crypto.sha256

// verify_cert_signature_rsa verifies an RSA PKCS#1 v1.5 (SHA-256) signature.
pub fn verify_cert_signature_rsa(child_der []u8, parent_rsa_der []u8) ! {
	// parse parent RSA public key: SEQUENCE { modulus INTEGER, exponent INTEGER }
	pmod, pexp := parse_rsa_pubkey(parent_rsa_der) or { return err }
	// parse child cert
	tag, cert_val, _ := der_tlv(child_der) or { return err }
	if tag != 0x30 { return error('tls: expected cert SEQUENCE') }
	// tbs raw bytes
	mut off := 0
	ttag, _, tconsumed := der_tlv(cert_val) or { return err }
	if ttag != 0x30 { return error('tls: expected tbs') }
	tbs_raw := cert_val[..tconsumed].clone()
	off = tconsumed
	// sigAlg skip
	_, _, sa_consumed := der_tlv(cert_val[off..]) or { return err }
	off += sa_consumed
	// signature BIT STRING
	stag, sig_val, _ := der_tlv(cert_val[off..]) or { return err }
	if stag != 0x03 { return error('tls: expected BIT STRING') }
	if sig_val.len < 1 || sig_val[0] != 0 { return error('tls: bad sig padding') }
	sig := sig_val[1..].clone()

	// RSA verify: s^e mod n
	s_int := big.integer_from_bytes(sig, signum: 1)
	m_int := s_int.mod_pow(pexp, pmod)
	// convert m to bytes, zero-padded to k
	k := (pmod.bit_len() + 7) / 8
	m_bytes, _ := m_int.bytes()
	// pad
	mut em := []u8{len: k}
	mut start := k - m_bytes.len
	for i in 0 .. m_bytes.len {
		em[start + i] = m_bytes[i]
	}
	// build expected PKCS#1 v1.5 EM for SHA-256
	hash := sha256.sum256(tbs_raw)
	// DigestInfo for SHA-256
	digest_info_prefix := [u8(0x30), 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65,
		0x03, 0x04, 0x02, 0x01, 0x05, 0x00, 0x04, 0x20]
	mut em_expected := []u8{}
	em_expected << u8(0x00)
	em_expected << u8(0x01)
	ps_len := k - 3 - digest_info_prefix.len - hash.len
	for _ in 0 .. ps_len { em_expected << u8(0xff) }
	em_expected << u8(0x00)
	for b in digest_info_prefix { em_expected << b }
	for b in hash { em_expected << b }

	if em != em_expected {
		return error(error_bad_signature)
	}
}

fn parse_rsa_pubkey(der []u8) !(big.Integer, u64) {
	tag, content, _ := der_tlv(der) or { return err }
	if tag != 0x30 { return error('tls: expected SEQUENCE') }
	// modulus
	mtag, mod_bytes, mod_consumed := der_tlv(content) or { return err }
	if mtag != 0x02 { return error('tls: expected INTEGER modulus') }
	// strip leading zero
	mut mod_slice := mod_bytes
	if mod_slice.len > 1 && mod_slice[0] == 0 { mod_slice = mod_slice[1..] }
	n := big.integer_from_bytes(mod_slice, signum: 1)
	// exponent
	etag, exp_bytes, _ := der_tlv(content[mod_consumed..]) or { return err }
	if etag != 0x02 { return error('tls: expected INTEGER exponent') }
	mut exp_val := u64(0)
	for b in exp_bytes { exp_val = (exp_val << 8) | u64(b) }
	return n, exp_val
}
