// Self-signed X.509 certificate generation for RSA keys.
module rsa

import crypto.rand
import crypto.sha256
import crypto.x509
import time

// DER content bytes of 1.2.840.113549.1.1.11 (sha256WithRSAEncryption).
const oid_sha256_with_rsa = [u8(0x2a), 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b]

// DER content bytes of 2.5.4.3 (commonName).
const oid_common_name = [u8(0x55), 0x04, 0x03]

// DER content bytes of 2.5.29.17 (subjectAltName).
const oid_subject_alt_name = [u8(0x55), 0x1d, 0x11]

// DER content bytes of 2.5.29.19 (basicConstraints).
const oid_basic_constraints = [u8(0x55), 0x1d, 0x13]

// cert_two formats a two-digit decimal.
fn cert_two(v int) string {
	if v < 10 {
		return '0${v}'
	}
	return v.str()
}

// der_utctime encodes t as a DER UTCTime (YYMMDDHHMMSSZ). X.509 times
// are UTC (Zulu): normalize local wall fields first, exactly like
// x509's der_utc, so the DER bytes match what the UTC-assuming parser
// reconstructs. UTCTime only covers 1950-2049; out-of-range years fail.
fn der_utctime(t time.Time) ![]u8 {
	u := t.local_to_utc()
	if u.year < 1950 || u.year > 2049 {
		return error('rsa: UTCTime year out of range')
	}
	mut s := ''
	s += cert_two(u.year % 100)
	s += cert_two(u.month)
	s += cert_two(u.day)
	s += cert_two(u.hour)
	s += cert_two(u.minute)
	s += cert_two(u.second)
	s += 'Z'
	return der_tlv(0x17, s.bytes())
}

// cert_name_der encodes a common-name-only X.509 Name.
fn cert_name_der(common_name string) []u8 {
	mut name_content := []u8{}
	if common_name.len > 0 {
		mut attr_content := []u8{}
		attr_content << der_oid(oid_common_name.clone())
		attr_content << der_tlv(0x0c, common_name.bytes())
		attr := der_seq(attr_content)
		set_content := der_tlv(0x31, attr)
		name_content << set_content
	}
	return der_seq(name_content)
}

// generate_self_signed creates a self-signed X.509 certificate for key
// (SHA-256 with RSA encryption) and returns the parsed certificate plus
// its DER encoding. The output parses with `x509.parse_der` and its
// signature verifies with `net.tls.verify_cert_signature_rsa` against
// the matching public key.
pub fn generate_self_signed(opts x509.CertGenOptions, key PrivateKey) !(x509.Certificate, []u8) {
	pk := key.public_key()
	serial := x509.normalize_serial_bytes(rand.bytes(8)!)

	name_der := cert_name_der(opts.common_name)

	mut validity_content := []u8{}
	validity_content << der_utctime(opts.not_before)!
	validity_content << der_utctime(opts.not_after)!
	validity := der_seq(validity_content)

	mut sig_alg_content := []u8{}
	sig_alg_content << der_oid(oid_sha256_with_rsa.clone())
	sig_alg_content << der_null()
	sig_alg_id := der_seq(sig_alg_content)

	spki := pk.to_spki_der()
	version := der_tlv(0xa0, der_tlv(0x02, [u8(0x02)]))

	mut tbs := []u8{}
	tbs << version
	tbs << der_tlv(0x02, serial)
	tbs << sig_alg_id
	tbs << name_der
	tbs << validity
	tbs << name_der
	tbs << spki

	mut exts_inner := []u8{}
	if opts.is_ca {
		mut bc_content := []u8{}
		bc_content << der_oid(oid_basic_constraints.clone())
		bc_content << der_tlv(0x01, [u8(0xff)])
		bc_content << der_octet_string(der_seq(der_tlv(0x01, [u8(0xff)])))
		exts_inner << der_seq(bc_content)
	}
	if opts.dns_names.len > 0 {
		mut san_content := []u8{}
		for dn in opts.dns_names {
			san_content << der_tlv(0x82, dn.bytes())
		}
		san_ext_value := der_seq(san_content)
		mut ext_content := []u8{}
		ext_content << der_oid(oid_subject_alt_name.clone())
		ext_content << der_octet_string(san_ext_value)
		exts_inner << der_seq(ext_content)
	}
	if exts_inner.len > 0 {
		tbs << der_tlv(0xa3, der_seq(exts_inner))
	}

	tbs_der := der_seq(tbs)

	digest := sha256.sum256(tbs_der)
	sig := key.sign_pkcs1_v15(digest, .sha256)!
	bsig := der_bit_string(sig)

	mut cert_content := []u8{}
	cert_content << tbs_der
	cert_content << sig_alg_id
	cert_content << bsig
	cert_der := der_seq(cert_content)

	cert := x509.parse_der(cert_der)!
	return cert, cert_der
}
