// Copyright (c) 2026 V project. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

// Minimal DER/X.509 parsing plus RSA PKCS#1 v1.5 sign/verify for TLS 1.2.
// Only what the handshake needs: leaf public-key extraction, signature
// verification (self-signed or pinned CA), and PKCS#8 server-key loading.
// Standalone; shares no code with net.tls.
module tls12

import crypto.sha256
import crypto.sha512
import crypto.subtle
import math.big
import time

// Well-known algorithm OIDs used here.
const oid_rsa_encryption = '1.2.840.113549.1.1.1'
const oid_ec_public_key = '1.2.840.10045.2.1'
const oid_secp256r1 = '1.3.132.0.10'
const oid_prime256v1 = '1.2.840.10045.3.1.7'
const oid_sha256_rsa = '1.2.840.113549.1.1.11'
const oid_sha384_rsa = '1.2.840.113549.1.1.12'
const oid_sha512_rsa = '1.2.840.113549.1.1.13'
const oid_ecdsa_sha256 = '1.2.840.10045.4.3.2'
const oid_common_name = '2.5.4.3'

// DigestInfo prefixes for EMSA-PKCS1-v1_5 (RFC 3447).
const digest_info_sha256 = [u8(0x30), 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65,
	0x03, 0x04, 0x02, 0x01, 0x05, 0x00, 0x04, 0x20]
const digest_info_sha384 = [u8(0x30), 0x41, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65,
	0x03, 0x04, 0x02, 0x02, 0x05, 0x00, 0x04, 0x30]
const digest_info_sha512 = [u8(0x30), 0x51, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65,
	0x03, 0x04, 0x02, 0x03, 0x05, 0x00, 0x04, 0x40]

// der_tlv encodes a tag-length-value triple (short/long form lengths).
fn der_tlv(tag u8, body []u8) []u8 {
	mut out := [tag]
	if body.len < 128 {
		out << u8(body.len)
	} else {
		mut lb := []u8{}
		mut n := body.len
		mut tmp := []u8{}
		for n > 0 {
			tmp << u8(n & 0xff)
			n >>= 8
		}
		for i := tmp.len - 1; i >= 0; i-- {
			lb << tmp[i]
		}
		out << u8(0x80 | lb.len)
		out << lb
	}
	out << body
	return out
}

// DerReader is a cursor over DER bytes.
struct DerReader {
	data []u8
mut:
	pos int
}

// at_end reports whether the cursor consumed all input.
fn (r &DerReader) at_end() bool {
	return r.pos >= r.data.len
}

// read_tlv reads one TLV; returns the tag number and the body bytes.
// Only tags < 31 are supported (all X.509 needs).
fn (mut r DerReader) read_tlv() !(int, []u8) {
	if r.pos + 2 > r.data.len {
		return error('tls12: truncated DER TLV')
	}
	tag := int(r.data[r.pos])
	r.pos++
	mut ln := int(r.data[r.pos])
	r.pos++
	if ln & 0x80 != 0 {
		nbytes := ln & 0x7f
		if nbytes == 0 || nbytes > 4 || r.pos + nbytes > r.data.len {
			return error('tls12: bad DER length')
		}
		ln = 0
		for _ in 0 .. nbytes {
			ln = int((u32(ln) << 8) | u32(r.data[r.pos]))
			r.pos++
		}
	}
	if ln < 0 || r.pos + ln > r.data.len {
		return error('tls12: DER length overruns input')
	}
	body := r.data[r.pos..r.pos + ln].clone()
	r.pos += ln
	return tag, body
}

// expect reads one TLV and checks its tag.
fn (mut r DerReader) expect(tag int) ![]u8 {
	got, body := r.read_tlv()!
	if got != tag {
		return error('tls12: unexpected DER tag ${got}, want ${tag}')
	}
	return body
}

// der_read_oid decodes an OID body to dotted form.
fn der_read_oid(body []u8) !string {
	if body.len == 0 {
		return error('tls12: empty OID')
	}
	mut nums := []u64{}
	mut v := u64(0)
	for b in body {
		v = (v << 7) | u64(b & 0x7f)
		if b & 0x80 == 0 {
			nums << v
			v = 0
		}
	}
	if nums.len == 0 {
		return error('tls12: bad OID')
	}
	first := nums[0]
	mut parts := [u64(first / 40), first % 40]
	parts << nums[1..]
	mut s := ''
	for i, p in parts {
		if i > 0 {
			s += '.'
		}
		s += p.str()
	}
	return s
}

// der_parse_time parses a UTCTime or GeneralizedTime body (must end in Z).
fn der_parse_time(tag int, body []u8) !time.Time {
	s := body.bytestr()
	if s.len < 11 || s[s.len - 1] != `Z` {
		return error('tls12: only UTC (Z) certificate times supported')
	}
	digits := s[..s.len - 1]
	mut year := 0
	mut rest := digits
	if tag == 0x17 {
		if digits.len != 12 {
			return error('tls12: bad UTCTime')
		}
		yy := digits[0..2].int()
		year = if yy >= 50 { 1900 + yy } else { 2000 + yy }
		rest = digits[2..]
	} else if tag == 0x18 {
		if digits.len != 14 {
			return error('tls12: bad GeneralizedTime')
		}
		year = digits[0..4].int()
		rest = digits[4..]
	} else {
		return error('tls12: not a time tag')
	}
	return time.Time{
		year:   year
		month:  rest[0..2].int()
		day:    rest[2..4].int()
		hour:   rest[4..6].int()
		minute: rest[6..8].int()
		second: rest[8..10].int()
	}
}

// time_not_after reports a <= b for UTC times.
fn time_not_after(a time.Time, b time.Time) bool {
	if a.year != b.year {
		return a.year < b.year
	}
	if a.month != b.month {
		return a.month < b.month
	}
	if a.day != b.day {
		return a.day < b.day
	}
	if a.hour != b.hour {
		return a.hour < b.hour
	}
	if a.minute != b.minute {
		return a.minute < b.minute
	}
	return a.second <= b.second
}

// RsaPublicKey is an RSA verification key.
struct RsaPublicKey {
	n big.Integer
	e big.Integer
}

// EcPublicKey is a P-256 verification key (uncompressed coordinates).
struct EcPublicKey {
	x []u8
	y []u8
}

// CertPubKey is either an RSA or an ECDSA public key.
type CertPubKey = EcPublicKey | RsaPublicKey

// Certificate is the parsed subset of an X.509 certificate the handshake uses.
struct Certificate {
	der         []u8
	tbs         []u8
	sig_alg_oid string
	sig         []u8
	issuer      string
	common_name string
	not_before  time.Time
	not_after   time.Time
	pubkey      CertPubKey
}

// der_string_body decodes a DirectoryString body to a V string.
fn der_string_body(tag int, body []u8) string {
	if tag == 0x1e {
		// BMPString: ASCII subset has zero high bytes.
		mut s := ''
		mut i := 0
		for i + 1 < body.len {
			if body[i] == 0 {
				s += body[i + 1].ascii_str()
			}
			i += 2
		}
		return s
	}
	return body.bytestr()
}

// parse_name extracts the commonName from a DER Name.
fn parse_name(der []u8) string {
	mut r := DerReader{ data: der }
	rdns := r.expect(0x30) or { return '' }
	mut rr := DerReader{ data: rdns }
	for !rr.at_end() {
		set_body := rr.expect(0x31) or { return '' }
		mut sr := DerReader{ data: set_body }
		for !sr.at_end() {
			atv := sr.expect(0x30) or { return '' }
			mut ar := DerReader{ data: atv }
			oid_body := ar.expect(0x06) or { return '' }
			oid := der_read_oid(oid_body) or { return '' }
			vt, vb := ar.read_tlv() or { return '' }
			if oid == oid_common_name {
				return der_string_body(vt, vb)
			}
		}
	}
	return ''
}

// parse_rsa_pubkey parses a PKCS#1 RSAPublicKey body.
fn parse_rsa_pubkey(der []u8) !RsaPublicKey {
	mut r := DerReader{ data: der }
	seq := r.expect(0x30)!
	mut ir := DerReader{ data: seq }
	nb := ir.expect(0x02)!
	eb := ir.expect(0x02)!
	return RsaPublicKey{
		n: os2ip(nb)
		e: os2ip(eb)
	}
}

// parse_certificate parses a DER X.509 certificate (leaf fields only).
fn parse_certificate(der []u8) !Certificate {
	mut outer := DerReader{ data: der }
	cert_body := outer.expect(0x30)!
	if !outer.at_end() {
		return error('tls12: trailing data after certificate')
	}
	// Keep the exact TBS bytes for signature verification.
	mut cr := DerReader{ data: cert_body }
	tbs_tag, tbs := cr.read_tlv()!
	if tbs_tag != 0x30 {
		return error('tls12: bad certificate TBS')
	}
	tbs_end := cr.pos
	tbs_raw := cert_body[..tbs_end].clone()
	sigalg_body := cr.expect(0x30)!
	sig_bits := cr.expect(0x03)!
	mut sr := DerReader{ data: sigalg_body }
	oid_body := sr.expect(0x06)!
	sig_oid := der_read_oid(oid_body)!
	mut tr := DerReader{ data: tbs }
	if tr.data.len >= 2 && tr.data[0] == 0xa0 {
		ver_tag, _ := tr.read_tlv()! // [0] version
		if ver_tag != 0xa0 {
			return error('tls12: bad certificate version')
		}
	}
	_ = tr.expect(0x02)! // serial
	_ = tr.expect(0x30)! // signature alg
	issuer_der := tr.expect(0x30)! // issuer Name (content bytes)
	validity := tr.expect(0x30)!
	subject_der := tr.expect(0x30)! // subject Name
	spki := tr.expect(0x30)!
	mut vr := DerReader{ data: validity }
	nb_tag, nb_body := vr.read_tlv()!
	na_tag, na_body := vr.read_tlv()!
	// Re-wrap Name contents as TLVs for parse_name.
	issuer := parse_name(der_tlv(0x30, issuer_der))
	subject := parse_name(der_tlv(0x30, subject_der))
	mut kr := DerReader{ data: spki }
	alg_body := kr.expect(0x30)!
	key_bits := kr.expect(0x03)!
	if key_bits.len < 1 || key_bits[0] != 0 {
		return error('tls12: only zero-unused-bits keys supported')
	}
	mut ar := DerReader{ data: alg_body }
	key_oid := der_read_oid(ar.expect(0x06)!)!
	mut pubkey := CertPubKey(RsaPublicKey{
		n: big.integer_from_int(0)
		e: big.integer_from_int(0)
	})
	if key_oid == oid_rsa_encryption {
		pubkey = parse_rsa_pubkey(key_bits[1..])!
	} else if key_oid == oid_ec_public_key {
		params_body := ar.expect(0x06)!
		params_oid := der_read_oid(params_body)!
		if params_oid != oid_secp256r1 && params_oid != oid_prime256v1 {
			return error('tls12: only secp256r1 EC keys supported')
		}
		raw_pt := key_bits[1..]
		if raw_pt.len != 65 || raw_pt[0] != 0x04 {
			return error('tls12: only uncompressed EC points supported')
		}
		pubkey = EcPublicKey{
			x: raw_pt[1..33].clone()
			y: raw_pt[33..65].clone()
		}
	} else {
		return error('tls12: unsupported public key algorithm ${key_oid}')
	}
	return Certificate{
		der:         der.clone()
		tbs:         tbs_raw
		sig_alg_oid: sig_oid
		sig:         sig_bits[1..].clone()
		issuer:      issuer
		common_name: subject
		not_before:  der_parse_time(nb_tag, nb_body)!
		not_after:   der_parse_time(na_tag, na_body)!
		pubkey:      pubkey
	}
}

// hash_for_oid hashes msg with the digest named by a signature OID.
fn hash_for_oid(sig_oid string, msg []u8) !([]u8, []u8) {
	if sig_oid == oid_sha256_rsa || sig_oid == oid_ecdsa_sha256 {
		h := sha256.sum256(msg)
		return h, digest_info_sha256
	}
	if sig_oid == oid_sha384_rsa {
		h := sha512.sum384(msg)
		return h, digest_info_sha384
	}
	if sig_oid == oid_sha512_rsa {
		h := sha512.sum512(msg)
		return h, digest_info_sha512
	}
	return error('tls12: unsupported signature algorithm ${sig_oid}')
}

// rsa_verify_pkcs1_v15 verifies an RSA PKCS#1 v1.5 signature over msg.
// The decrypted block is compared against a reconstructed expected block
// with a constant-time compare (no padding-scan branches).
fn rsa_verify_pkcs1_v15(key RsaPublicKey, sig []u8, msg []u8, sig_oid string) ! {
	hash, prefix := hash_for_oid(sig_oid, msg)!
	nb, _ := key.n.bytes()
	k := nb.len
	if sig.len != k {
		return error('tls12: bad RSA signature length')
	}
	if k < prefix.len + hash.len + 11 {
		return error('tls12: RSA modulus too small')
	}
	m := os2ip(sig).big_mod_pow(key.e, key.n)!
	em := i2osp_exact(m, k)!
	mut expected := []u8{len: k}
	expected[1] = 0x01
	for i in 2 .. k - prefix.len - hash.len - 1 {
		expected[i] = 0xff
	}
	for i in 0 .. prefix.len {
		expected[k - prefix.len - hash.len + i] = prefix[i]
	}
	for i in 0 .. hash.len {
		expected[k - hash.len + i] = hash[i]
	}
	if subtle.constant_time_compare(em, expected) != 1 {
		return error('tls12: RSA signature mismatch')
	}
}

// i2osp_exact serializes x as exactly ln big-endian bytes.
fn i2osp_exact(x big.Integer, ln int) ![]u8 {
	b, _ := x.bytes()
	if b.len > ln {
		return error('tls12: integer too large')
	}
	mut out := []u8{len: ln}
	for i in 0 .. b.len {
		out[ln - b.len + i] = b[i]
	}
	return out
}

// rsa_sign_pkcs1_v15 signs msg with a private exponent.
fn rsa_sign_pkcs1_v15(n big.Integer, d big.Integer, msg []u8, sig_oid string) ![]u8 {
	hash, prefix := hash_for_oid(sig_oid, msg)!
	nb, _ := n.bytes()
	k := nb.len
	if k < prefix.len + hash.len + 11 {
		return error('tls12: RSA modulus too small')
	}
	mut em := []u8{len: k}
	em[0] = 0x00
	em[1] = 0x01
	for i in 2 .. k - prefix.len - hash.len - 1 {
		em[i] = 0xff
	}
	em[k - prefix.len - hash.len - 1] = 0x00
	for i in 0 .. prefix.len {
		em[k - prefix.len - hash.len + i] = prefix[i]
	}
	for i in 0 .. hash.len {
		em[k - hash.len + i] = hash[i]
	}
	m := os2ip(em).big_mod_pow(d, n)!
	return i2osp_exact(m, k)
}

// verify_cert_signature verifies cert.tbs against pubkey (CA or self).
fn verify_cert_signature(cert Certificate, key CertPubKey) ! {
	match key {
		RsaPublicKey {
			if cert.sig_alg_oid != oid_sha256_rsa && cert.sig_alg_oid != oid_sha384_rsa
				&& cert.sig_alg_oid != oid_sha512_rsa {
				return error('tls12: cert/key algorithm mismatch')
			}
			rsa_verify_pkcs1_v15(key, cert.sig, cert.tbs, cert.sig_alg_oid)!
		}
		EcPublicKey {
			if cert.sig_alg_oid != oid_ecdsa_sha256 {
				return error('tls12: cert/key algorithm mismatch')
			}
			hash := sha256.sum256(cert.tbs)
			r, s := ecdsa_sig_from_der(cert.sig)!
			if !p256_ecdsa_verify(key.x, key.y, hash, r, s) {
				return error('tls12: ECDSA certificate signature mismatch')
			}
		}
	}
}

// check_validity fails if now is outside [not_before, not_after].
fn check_validity(cert Certificate, now time.Time) ! {
	if !time_not_after(cert.not_before, now) || !time_not_after(now, cert.not_after) {
		return error('tls12: certificate not valid at this time')
	}
}

// ServerKey is a loaded server private key.
type ServerKey = EcServerKey | RsaServerKey

// RsaServerKey holds RSA private parameters.
struct RsaServerKey {
	n big.Integer
	e big.Integer
	d big.Integer
}

// EcServerKey holds a P-256 private key.
struct EcServerKey {
	priv []u8
}

// parse_rsa_private_key parses a PKCS#1 RSAPrivateKey body.
fn parse_rsa_private_key(der []u8) !ServerKey {
	mut r := DerReader{ data: der }
	seq := r.expect(0x30)!
	mut ir := DerReader{ data: seq }
	_ = ir.expect(0x02)! // version
	nb := ir.expect(0x02)!
	eb := ir.expect(0x02)!
	db := ir.expect(0x02)!
	return RsaServerKey{
		n: os2ip(nb)
		e: os2ip(eb)
		d: os2ip(db)
	}
}

// parse_server_key_pkcs8 parses a PKCS#8 PrivateKeyInfo (RSA or P-256 EC).
fn parse_server_key_pkcs8(der []u8) !ServerKey {
	mut r := DerReader{ data: der }
	outer := r.expect(0x30)!
	mut ir := DerReader{ data: outer }
	_ = ir.expect(0x02)! // version
	alg := ir.expect(0x30)!
	key_oct := ir.expect(0x04)!
	mut ar := DerReader{ data: alg }
	alg_oid := der_read_oid(ar.expect(0x06)!)!
	if alg_oid == oid_rsa_encryption {
		return parse_rsa_private_key(key_oct)
	}
	if alg_oid == oid_ec_public_key {
		mut kr := DerReader{ data: key_oct }
		sec1 := kr.expect(0x30)!
		mut sr := DerReader{ data: sec1 }
		_ = sr.expect(0x02)! // version
		priv := sr.expect(0x04)!
		if priv.len != 32 {
			return error('tls12: bad EC private key length')
		}
		return EcServerKey{ priv: priv.clone() }
	}
	return error('tls12: unsupported private key algorithm ${alg_oid}')
}
