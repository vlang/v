// Copyright (c) 2026 blackshirt.
//
// X.509 decoding tests built from hand-assembled DER: SAN, basicConstraints,
// keyUsage, EKU, nameConstraints, commonName, and validity-time parsing, plus
// parse_certificate's structural rejection of malformed input.
module pki

import crypto.ecdsa

fn test_parse_san_extension() ! {
	// GeneralNames = SEQUENCE of dNSName [2] "example.com" and iPAddress [7] 1.1.1.1.
	// Context tags are primitive: [2] => 0x82, [7] => 0x87.
	dns_bytes := 'example.com'.bytes() // 11 ASCII bytes
	mut body := []u8{}
	body << u8(0x82)
	body << u8(dns_bytes.len)
	body << dns_bytes
	body << u8(0x87)
	body << u8(4)
	body << [u8(1), 1, 1, 1]
	mut der := []u8{}
	der << u8(0x30)
	der << u8(body.len)
	der << body

	dns, ip := parse_san_extension(der)
	assert dns == ['example.com']
	assert ip.len == 1
	assert ip[0] == [u8(1), 1, 1, 1]
}

fn test_parse_san_extension_rejects_null_and_bad_ip() ! {
	// dNSName with an embedded NUL (0x00) is dropped (CVE-2009-2408); an
	// iPAddress of 5 bytes (neither 4 nor 16) is ignored.
	mut body := []u8{}
	body << u8(0x82)
	body << u8(3)
	body << [u8(0x61), 0x00, 0x62]
	body << u8(0x87)
	body << u8(5)
	body << [u8(1), 2, 3, 4, 5]
	mut der := []u8{}
	der << u8(0x30)
	der << u8(body.len)
	der << body

	dns, ip := parse_san_extension(der)
	assert dns.len == 0
	assert ip.len == 0
}

fn test_parse_basic_constraints() {
	// SEQUENCE { BOOLEAN cA = TRUE } — no pathLenConstraint, so the limit is -1.
	ca := [u8(0x30), 0x03, 0x01, 0x01, 0xff]
	is_ca, path_len := parse_basic_constraints(ca)
	assert is_ca
	assert path_len == -1
	// cA DEFAULT FALSE (and unlimited path) when the SEQUENCE is empty.
	empty_seq := [u8(0x30), 0x00]
	is_ca2, path_len2 := parse_basic_constraints(empty_seq)
	assert !is_ca2
	assert path_len2 == -1
	// Explicit cA = FALSE.
	not_ca := [u8(0x30), 0x03, 0x01, 0x01, 0x00]
	is_ca3, _ := parse_basic_constraints(not_ca)
	assert !is_ca3
	// SEQUENCE { BOOLEAN cA = TRUE, INTEGER pathLen = 1 } caps the chain depth.
	with_len := [u8(0x30), 0x06, 0x01, 0x01, 0xff, 0x02, 0x01, 0x01]
	is_ca4, path_len4 := parse_basic_constraints(with_len)
	assert is_ca4
	assert path_len4 == 1
}

fn test_parse_path_len_constraint_strictness() {
	// Legit non-negative, minimal values decode verbatim.
	assert parse_path_len_constraint([u8(0x00)]) == 0
	assert parse_path_len_constraint([u8(0x01)]) == 1
	assert parse_path_len_constraint([u8(0x01), 0x00, 0x00]) == 65536
	// Every malformed encoding collapses to 0 (most restrictive), never the -1
	// unlimited sentinel that would widen the CA-depth limit validator.v enforces.
	assert parse_path_len_constraint([]u8{}) == 0 // empty INTEGER
	assert parse_path_len_constraint([u8(0x81)]) == 0 // negative (sign bit set)
	assert parse_path_len_constraint([u8(0x00), 0x01]) == 0 // non-minimal leading zero
	assert parse_path_len_constraint([u8(0x01), 0x00, 0x00, 0x00, 0x00]) == 0 // >4 octets
}

fn test_parse_basic_constraints_malformed_pathlen() {
	// SEQUENCE { cA TRUE, pathLen INTEGER = 0x81 (negative) } -> limit 0, not -1.
	neg := [u8(0x30), 0x06, 0x01, 0x01, 0xff, 0x02, 0x01, 0x81]
	is_ca, path_len := parse_basic_constraints(neg)
	assert is_ca
	assert path_len == 0
	// pathLen INTEGER = 0x02 0x02 00 01 (non-minimal "1") -> limit 0, not 1.
	nonmin := [u8(0x30), 0x07, 0x01, 0x01, 0xff, 0x02, 0x02, 0x00, 0x01]
	_, path_len2 := parse_basic_constraints(nonmin)
	assert path_len2 == 0
}

fn test_parse_san_extension_rejects_non_context_tag() ! {
	// A universal INTEGER (0x02) whose low 5 bits collide with dNSName [2], and a
	// constructed [2] (0xa2), must both be ignored: GeneralName choices are
	// primitive context-specific tags only, so bytestr()-ing their payload as a
	// hostname would inject a bogus name into the identity set.
	mut body := []u8{}
	body << u8(0x02)
	body << u8(2)
	body << [u8(0x61), 0x62] // INTEGER "ab"
	body << u8(0xa2)
	body << u8(2)
	body << [u8(0x61), 0x62] // constructed [2]
	mut der := []u8{}
	der << u8(0x30)
	der << u8(body.len)
	der << body

	dns, ip := parse_san_extension(der)
	assert dns.len == 0
	assert ip.len == 0
}

fn test_parse_key_usage_and_bits() {
	// BIT STRING, 0 unused bits, value 0x84 = digitalSignature (bit 0) |
	// keyCertSign (bit 5), the two assertions the validator checks.
	ku := [u8(0x03), 0x02, 0x00, 0x84]
	bits := parse_key_usage(ku)
	assert bits == [u8(0x84)]
	assert key_usage_bit(bits, 0)
	assert key_usage_bit(bits, 5)
	assert !key_usage_bit(bits, 1) // contentCommitment not asserted
	// Named bits past the encoded length are never asserted.
	assert !key_usage_bit(bits, 9)
	// An illegal unused-bits count (> 7) makes the whole bitmap untrustworthy,
	// so parse yields an empty (asserts-nothing) result.
	bad := [u8(0x03), 0x02, 0x08, 0x84]
	assert parse_key_usage(bad).len == 0
}

fn test_parse_eku_server_auth() {
	// SEQUENCE { OID id-kp-serverAuth (1.3.6.1.5.5.7.3.1) }
	server_auth := [u8(0x30), 0x0a, 0x06, 0x08, 0x2b, 0x06, 0x01, 0x05, 0x05, 0x07, 0x03, 0x01]
	assert parse_eku_server_auth(server_auth)
	// SEQUENCE { OID anyExtendedKeyUsage (2.5.29.37.0) } also satisfies it.
	any_eku := [u8(0x30), 0x06, 0x06, 0x04, 0x55, 0x1d, 0x25, 0x00]
	assert parse_eku_server_auth(any_eku)
	// id-kp-clientAuth (...3.2) is NOT server auth.
	client_auth := [u8(0x30), 0x0a, 0x06, 0x08, 0x2b, 0x06, 0x01, 0x05, 0x05, 0x07, 0x03, 0x02]
	assert !parse_eku_server_auth(client_auth)
}

fn test_extract_cn_from_name() {
	// A single RDN SET holding one Attribute SEQUENCE: OID commonName (2.5.4.3)
	// plus a PrintableString value. extract_cn receives the Name's child bytes.
	mut oid_el := []u8{}
	oid_el << [u8(0x06), 0x03, 0x55, 0x04, 0x03]
	val_bytes := 'example'.bytes()
	mut val_el := []u8{}
	val_el << u8(0x13)
	val_el << u8(val_bytes.len)
	val_el << val_bytes
	mut attr_body := []u8{}
	attr_body << oid_el
	attr_body << val_el
	mut attr := []u8{}
	attr << u8(0x30)
	attr << u8(attr_body.len)
	attr << attr_body
	mut name := []u8{}
	name << u8(0x31)
	name << u8(attr.len)
	name << attr
	assert extract_cn_from_name(name) == 'example'

	// A NUL byte in the value must yield "" (CVE-2009-2408).
	mut bad_val := []u8{}
	bad_val << [u8(0x13), 0x03, 0x61, 0x00, 0x62]
	mut bad_attr_body := []u8{}
	bad_attr_body << oid_el
	bad_attr_body << bad_val
	mut bad_attr := []u8{}
	bad_attr << u8(0x30)
	bad_attr << u8(bad_attr_body.len)
	bad_attr << bad_attr_body
	mut bad_name := []u8{}
	bad_name << u8(0x31)
	bad_name << u8(bad_attr.len)
	bad_name << bad_attr
	assert extract_cn_from_name(bad_name) == ''
}

fn test_parse_asn1_time() ! {
	// Pin the exact epoch second for a known date: 2021-01-01T00:00:00Z =
	// 1609459200. Equality (not just != 0) catches time.parse treating the UTC
	// fields as local wall-clock time, which would shift every expiry check in
	// validator.v by the host timezone offset.
	utc := Asn1Element{ tag: u8(0x17), is_construct: false, data: '210101000000Z'.bytes() }
	assert parse_asn1_time(utc)! == 1609459200
	gen := Asn1Element{ tag: u8(0x18), is_construct: false, data: '20210101000000Z'.bytes() }
	assert parse_asn1_time(gen)! == 1609459200

	// RFC 5280 year pivot: UTCTime YY >= 50 decodes as 19xx, so '750101000000Z'
	// is 1975-01-01T00:00:00Z = 157766400, not 2075.
	pivot := Asn1Element{ tag: u8(0x17), is_construct: false, data: '750101000000Z'.bytes() }
	assert parse_asn1_time(pivot)! == 157766400

	// Strict DER (H1): a century-prefixed over-long UTCTime, a non-digit field,
	// and a missing trailing 'Z' must all be rejected rather than silently
	// misparsed into a wrong validity window the validator would then trust.
	mut rejected := 0
	overlong := Asn1Element{ tag: u8(0x17), is_construct: false, data: '20301231235959Z'.bytes() }
	parse_asn1_time(overlong) or {
		assert err.msg().contains('unsupported time encoding')
		rejected++
	}
	nondigit := Asn1Element{ tag: u8(0x17), is_construct: false, data: '2A0101000000Z'.bytes() }
	parse_asn1_time(nondigit) or {
		assert err.msg().contains('unsupported time encoding')
		rejected++
	}
	no_zulu := Asn1Element{ tag: u8(0x17), is_construct: false, data: '210101000000+'.bytes() }
	parse_asn1_time(no_zulu) or {
		assert err.msg().contains('unsupported time encoding')
		rejected++
	}
	assert rejected == 3

	// Any other tag is an unsupported encoding.
	bad := Asn1Element{ tag: u8(0x0c), is_construct: false, data: 'whatever'.bytes() }
	parse_asn1_time(bad) or {
		assert err.msg().contains('unsupported time encoding')
		return
	}
	assert false
}

fn test_unwrap_octet_string() {
	// A DER OCTET STRING (0x04) wrapper is unwrapped to its contents...
	wrapped := [u8(0x04), 0x03, 0xaa, 0xbb, 0xcc]
	assert unwrap_octet_string(wrapped) == [u8(0xaa), 0xbb, 0xcc]
	// ...while non-OCTET-STRING input is returned unchanged.
	raw := [u8(0x30), 0x00]
	assert unwrap_octet_string(raw) == raw
}

fn test_parse_certificate_rejects_non_sequence() ! {
	// A certificate must begin with an ASN.1 SEQUENCE (0x30); an INTEGER (0x02)
	// header is rejected rather than walked further.
	if _ := parse_certificate([u8(0x02), 0x01, 0x05]) {
		assert false
	}
}

fn test_parse_certificate_rejects_truncated() ! {
	// A single byte cannot even hold a TLV header.
	if _ := parse_certificate([u8(0x30)]) {
		assert false
	}
}

// der_tlv frames the concatenation of `parts` under `tag`, choosing the minimal
// definite length form DER requires: short below 0x80, 0x81 + one octet up to
// 0xff, and 0x82 + two octets beyond (the full-certificate fixture's TBS passes
// 255). Children are appended via `<<` because V rejects `[]u8 + []u8` on these
// literals (same rationale as rsa_seq in rsa_test.v).
fn der_tlv(tag u8, parts [][]u8) []u8 {
	mut body := []u8{}
	for p in parts {
		body << p
	}
	mut out := []u8{}
	out << tag
	if body.len < 0x80 {
		out << u8(body.len)
	} else if body.len <= 0xff {
		out << u8(0x81)
		out << u8(body.len)
	} else {
		out << u8(0x82)
		out << u8((body.len >> 8) & 0xff)
		out << u8(body.len & 0xff)
	}
	out << body
	return out
}

// cert_skeleton_parts builds the reusable children shared by the truncated
// certificate fixtures below: a sha256WithRSAEncryption AlgorithmIdentifier,
// INTEGER 1 serial, empty Name SEQUENCEs, a UTCTime validity window, and a
// one-byte-signature BIT STRING (RFC 5280 §4.1 DER shapes). Index 0 is an
// unused placeholder so the meaningful children keep stable indices.
fn cert_skeleton_parts() [][]u8 {
	placeholder := []u8{}
	oid_sha256_rsa := [u8(0x06), 0x09, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b]
	sig_alg := der_tlv(0x30, [oid_sha256_rsa])
	mut serial_body := []u8{}
	serial_body << u8(0x01)
	serial := der_tlv(0x02, [serial_body])
	empty_body := []u8{}
	empty_name := der_tlv(0x30, [empty_body])
	validity := der_tlv(0x30, [der_tlv(0x17, ['200101000000Z'.bytes()]),
		der_tlv(0x17, ['301231235959Z'.bytes()])])
	mut sig_bits := []u8{}
	sig_bits << u8(0x00)
	sig_bits << u8(0x01)
	sig_val := der_tlv(0x03, [sig_bits])
	return [placeholder, sig_alg, serial, empty_name, validity, sig_val]
}

fn test_parse_certificate_rejects_missing_fields() ! {
	// TBSCertificate holds six mandatory fields when the optional version is
	// absent: a five-element TBS must trip the len<6 structural guard, not be
	// indexed further.
	parts := cert_skeleton_parts()
	sig_alg := parts[1]
	serial := parts[2]
	empty_name := parts[3]
	validity := parts[4]
	sig_val := parts[5]

	tbs := der_tlv(0x30, [serial, sig_alg, empty_name, validity, empty_name])
	cert := der_tlv(0x30, [tbs, sig_alg, sig_val])
	parse_certificate(cert) or {
		assert err.msg().contains('malformed TBSCertificate (5 elements)')
		return
	}
	assert false
}

fn test_parse_certificate_v3_missing_spki_fails_clean() ! {
	// Regression for the remote-DoS fix: the optional version [0] element shifts
	// every later field index up by one, so a six-element TBS that starts with it
	// is missing SPKI. Before the post-version length guard, parse_certificate
	// indexed tbs_elements[6] on a len==6 array and PANICKED on this
	// attacker-controlled handshake input; it must now return a decode error.
	parts := cert_skeleton_parts()
	sig_alg := parts[1]
	serial := parts[2]
	empty_name := parts[3]
	validity := parts[4]
	sig_val := parts[5]

	mut version_body := []u8{}
	version_body << u8(0x02) // ENUMERATED-style INTEGER value 2 => version v3
	version_v3 := der_tlv(0xa0, [der_tlv(0x02, [version_body])])

	tbs := der_tlv(0x30, [version_v3, serial, sig_alg, empty_name, validity, empty_name])
	cert := der_tlv(0x30, [tbs, sig_alg, sig_val])
	parse_certificate(cert) or {
		assert err.msg().contains('need 6 fields after version')
		return
	}
	assert false
}

fn test_parse_certificate_v3_full_decode() ! {
	// Positive control for the element-count guards: a complete v3 certificate —
	// version [0], all seven mandatory elements, and an extensions [3] carrying
	// SAN (no critical flag → extnValue at parts[1]) plus BasicConstraints /
	// KeyUsage / EKU (critical BOOLEAN → parts[2]) — must parse end to end. This
	// is the suite's only exercise of parse_certificate's success path, pinning
	// that the length guards reject truncation only, never a well-formed
	// certificate, and that every extension dispatch arm extracts real values.
	oid_sha256_rsa := [u8(0x06), 0x09, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b]
	sig_alg := der_tlv(0x30, [oid_sha256_rsa])
	ser_body := [u8(0x2a)]
	serial := der_tlv(0x02, [ser_body])
	empty_body := []u8{}
	issuer := der_tlv(0x30, [empty_body])
	validity := der_tlv(0x30, [der_tlv(0x17, ['200101000000Z'.bytes()]),
		der_tlv(0x17, ['301231235959Z'.bytes()])])
	subject := der_tlv(0x30, [empty_body])
	ver_body := [u8(0x02)]
	version_v3 := der_tlv(0xa0, [der_tlv(0x02, [ver_body])])

	// SPKI: id-ecPublicKey with the prime256v1 named-curve parameter and a
	// 65-byte uncompressed point. parse_certificate only stores the raw bytes;
	// on-curve validation happens later in ecdsa_public_key(), off this path.
	pub_alg_oid := [u8(0x06), 0x07, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x02, 0x01]
	curve_oid := [u8(0x06), 0x08, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x03, 0x01, 0x07]
	alg := der_tlv(0x30, [pub_alg_oid, curve_oid])
	mut point := []u8{len: 65, init: 0x01}
	point[0] = 0x04
	mut bits_body := []u8{}
	bits_body << u8(0x00) // BIT STRING unused-bits count
	bits_body << point
	bitstr := der_tlv(0x03, [bits_body])
	spki := der_tlv(0x30, [alg, bitstr])

	// Extensions: SAN (dNSName + iPAddress), then critical BasicConstraints
	// (cA TRUE, pathLen 0), KeyUsage (digitalSignature|keyCertSign = 0x84), and
	// EKU (id-kp-serverAuth).
	dns_name := der_tlv(0x82, ['example.com'.bytes()])
	ip_body := [u8(1), 2, 3, 4]
	ip_name := der_tlv(0x87, [ip_body])
	gnames := der_tlv(0x30, [dns_name, ip_name])
	san_oid := [u8(0x06), 0x03, 0x55, 0x1d, 0x11]
	san_ext := der_tlv(0x30, [san_oid, der_tlv(0x04, [gnames])])

	crit_bool := [u8(0x01), 0x01, 0xff]
	bc_content := [u8(0x01), 0x01, 0xff, 0x02, 0x01, 0x00]
	bc_oid := [u8(0x06), 0x03, 0x55, 0x1d, 0x13]
	bc_ext := der_tlv(0x30, [bc_oid, crit_bool, der_tlv(0x04, [der_tlv(0x30, [bc_content])])])

	ku_content := [u8(0x00), 0x84]
	ku_oid := [u8(0x06), 0x03, 0x55, 0x1d, 0x0f]
	ku_ext := der_tlv(0x30, [ku_oid, crit_bool, der_tlv(0x04, [der_tlv(0x03, [ku_content])])])

	eku_content := [u8(0x06), 0x08, 0x2b, 0x06, 0x01, 0x05, 0x05, 0x07, 0x03, 0x01]
	eku_oid := [u8(0x06), 0x03, 0x55, 0x1d, 0x25]
	eku_ext := der_tlv(0x30, [eku_oid, crit_bool, der_tlv(0x04, [der_tlv(0x30, [eku_content])])])

	ext_list := der_tlv(0x30, [san_ext, bc_ext, ku_ext, eku_ext])
	ext_wrap := der_tlv(0xa3, [ext_list])

	tbs := der_tlv(0x30, [version_v3, serial, sig_alg, issuer, validity, subject, spki, ext_wrap])
	sig_bits := [u8(0x00), 0x01, 0x02]
	sig_val := der_tlv(0x03, [sig_bits])
	cert := der_tlv(0x30, [tbs, sig_alg, sig_val])

	c := parse_certificate(cert)!
	assert c.version == 3
	assert c.serial_number == [u8(0x2a)]
	assert c.public_key_algo == .ecdsa
	assert c.ecdsa_curve == ecdsa.Nid.prime256v1
	assert c.public_key_raw.len == 65
	assert c.public_key_raw[0] == 0x04
	assert c.has_san
	assert c.sans == ['example.com']
	assert c.ip_sans[0] == [u8(1), 2, 3, 4]
	assert c.is_ca
	assert c.max_path_len == 0
	assert c.has_key_usage
	assert key_usage_bit(c.key_usage, 0) // digitalSignature
	assert key_usage_bit(c.key_usage, 5) // keyCertSign
	assert !key_usage_bit(c.key_usage, 1) // contentCommitment not asserted
	assert c.has_eku
	assert c.eku_server_auth
	assert c.issuer_cn == '' && c.subject_cn == ''
	assert c.signature == [u8(0x01), 0x02]
	assert c.tbs_raw[0] == 0x30
	assert c.not_before_unix > 0 && c.not_before_unix < c.not_after_unix
}

// minimal_extension_cert builds a well-formed v3 certificate whose only
// extension is the given OID (DER-encoded) with an optional critical flag.
fn minimal_extension_cert(ext_oid_der []u8, critical bool, ext_value_der []u8) []u8 {
	oid_sha256_rsa := [u8(0x06), 0x09, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b]
	sig_alg := der_tlv(0x30, [oid_sha256_rsa])
	ser_body := [u8(0x2a)]
	serial := der_tlv(0x02, [ser_body])
	empty_body := []u8{}
	issuer := der_tlv(0x30, [empty_body])
	validity := der_tlv(0x30, [der_tlv(0x17, ['200101000000Z'.bytes()]),
		der_tlv(0x17, ['301231235959Z'.bytes()])])
	subject := der_tlv(0x30, [empty_body])
	pub_alg_oid := [u8(0x06), 0x07, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x02, 0x01]
	curve_oid := [u8(0x06), 0x08, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x03, 0x01, 0x07]
	alg := der_tlv(0x30, [pub_alg_oid, curve_oid])
	mut point := []u8{len: 65, init: 0x01}
	point[0] = 0x04
	mut bits_body := [u8(0x00)]
	bits_body << point
	bitstr := der_tlv(0x03, [bits_body])
	spki := der_tlv(0x30, [alg, bitstr])

	mut ext_parts := [][]u8{}
	ext_parts << ext_oid_der
	if critical {
		ext_parts << [u8(0x01), 0x01, 0xff]
	}
	ext_parts << der_tlv(0x04, [ext_value_der])
	ext := der_tlv(0x30, ext_parts)
	ext_list := der_tlv(0x30, [ext])
	ext_wrap := der_tlv(0xa3, [ext_list])

	tbs := der_tlv(0x30, [serial, sig_alg, issuer, validity, subject, spki, ext_wrap])
	sig_bits := [u8(0x00), 0x01, 0x02]
	sig_val := der_tlv(0x03, [sig_bits])
	return der_tlv(0x30, [tbs, sig_alg, sig_val])
}

// test_parse_certificate_rejects_unknown_critical_extension: RFC 5280 §4.2
// requires rejecting a certificate carrying a critical extension the
// implementation does not understand — most importantly to stop a name-
// constrained intermediate (critical nameConstraints) from being treated as
// unconstrained. A non-critical unknown extension must still be ignored.
fn test_parse_certificate_rejects_unknown_critical_extension() ! {
	unknown_oid := [u8(0x06), 0x03, 0x2a, 0x03, 0x04] // OID 1.2.3.4
	value := der_tlv(0x04, ['x'.bytes()])

	critical_cert := minimal_extension_cert(unknown_oid, true, value)
	parse_certificate(critical_cert) or {
		assert err.msg().contains('unsupported critical extension')
		return
	}
	assert false
}

fn test_parse_certificate_ignores_unknown_noncritical_extension() ! {
	unknown_oid := [u8(0x06), 0x03, 0x2a, 0x03, 0x04]
	value := der_tlv(0x04, ['x'.bytes()])

	cert := minimal_extension_cert(unknown_oid, false, value)
	c := parse_certificate(cert)!
	assert c.version == 1
}

// test_parse_certificate_rejects_duplicate_extension: RFC 5280 §4.2 forbids two
// instances of the same extension OID. Two identical BasicConstraints extensions
// must fail the decode rather than let the second silently overwrite the first.
fn test_parse_certificate_rejects_duplicate_extension() ! {
	oid_sha256_rsa := [u8(0x06), 0x09, 0x2a, 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x0b]
	sig_alg := der_tlv(0x30, [oid_sha256_rsa])
	ser_body := [u8(0x2a)]
	serial := der_tlv(0x02, [ser_body])
	empty_body := []u8{}
	issuer := der_tlv(0x30, [empty_body])
	validity := der_tlv(0x30, [der_tlv(0x17, ['200101000000Z'.bytes()]),
		der_tlv(0x17, ['301231235959Z'.bytes()])])
	subject := der_tlv(0x30, [empty_body])
	pub_alg_oid := [u8(0x06), 0x07, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x02, 0x01]
	curve_oid := [u8(0x06), 0x08, 0x2a, 0x86, 0x48, 0xce, 0x3d, 0x03, 0x01, 0x07]
	alg := der_tlv(0x30, [pub_alg_oid, curve_oid])
	mut point := []u8{len: 65, init: 0x01}
	point[0] = 0x04
	mut bits_body := []u8{}
	bits_body << u8(0x00) // BIT STRING unused-bits count
	bits_body << point
	bitstr := der_tlv(0x03, [bits_body])
	spki := der_tlv(0x30, [alg, bitstr])

	// One non-critical BasicConstraints extension, then an identical twin.
	bc_oid := [u8(0x06), 0x03, 0x55, 0x1d, 0x13]
	bc_content := [u8(0x01), 0x01, 0xff] // BOOLEAN cA = TRUE
	bc_value := der_tlv(0x30, [bc_content])
	bc_ext := der_tlv(0x30, [bc_oid, der_tlv(0x04, [bc_value])])
	ext_list := der_tlv(0x30, [bc_ext, bc_ext])
	ext_wrap := der_tlv(0xa3, [ext_list])

	tbs := der_tlv(0x30, [serial, sig_alg, issuer, validity, subject, spki, ext_wrap])
	sig_bits := [u8(0x00), 0x01, 0x02]
	sig_val := der_tlv(0x03, [sig_bits])
	cert := der_tlv(0x30, [tbs, sig_alg, sig_val])

	parse_certificate(cert) or {
		assert err.msg().contains('duplicate extension')
		return
	}
	assert false
}

// test_parse_name_constraints: the NameConstraints extension (critical, RFC 5280
// §4.2.1.10) must be decoded into the permitted/excluded dNSName and iPAddress
// subtrees rather than rejected as an unknown critical extension.
fn test_parse_name_constraints() ! {
	// permittedSubtrees [0] = SEQUENCE OF GeneralSubtree{ dNSName "example.com" }
	dns_base := der_tlv(0x82, ['example.com'.bytes()])
	dns_subtree := der_tlv(0x30, [dns_base])
	permitted := der_tlv(0xa0, [der_tlv(0x30, [dns_subtree])])

	// excludedSubtrees [1] = SEQUENCE OF GeneralSubtree{ iPAddress 10.0.0.0/8 }
	ip_base := der_tlv(0x87, [[u8(10), 0, 0, 0], [u8(255), 0, 0, 0]])
	ip_subtree := der_tlv(0x30, [ip_base])
	excluded := der_tlv(0xa1, [der_tlv(0x30, [ip_subtree])])

	nc_value := der_tlv(0x30, [permitted, excluded])
	nc_oid := [u8(0x06), 0x03, 0x55, 0x1d, 0x1e] // 2.5.29.30

	cert := minimal_extension_cert(nc_oid, true, nc_value)
	c := parse_certificate(cert)!

	assert c.name_constraints_present
	assert c.permitted_dns == ['example.com']
	assert c.excluded_ip.len == 1
	assert c.excluded_ip[0] == [u8(10), 0, 0, 0, 255, 0, 0, 0]
}

// ---- Name-constraint subtree matching ----

fn test_dns_within_subtree_matching() {
	// Exact name and subdomains match; a different registrable name does not.
	assert dns_within_subtree('example.com', 'example.com')
	assert dns_within_subtree('a.example.com', 'example.com')
	assert dns_within_subtree('deep.a.example.com', 'example.com')
	// Case-insensitive per RFC 5280 §7.2.
	assert dns_within_subtree('EXAMPLE.com', 'example.com')
	// A boundary lookalike must not match.
	assert !dns_within_subtree('notexample.com', 'example.com')
	// A leading-dot subtree matches only subdomains, not the bare name.
	assert dns_within_subtree('a.example.com', '.example.com')
	assert !dns_within_subtree('example.com', '.example.com')
}

fn test_ip_within_subtree_matching() {
	// 10.0.0.0/8 covers 10.1.2.3 but not 11.0.0.1.
	cidr := [u8(10), 0, 0, 0, 255, 0, 0, 0]
	assert ip_within_subtree([u8(10), 1, 2, 3], cidr)
	assert !ip_within_subtree([u8(11), 0, 0, 1], cidr)
	// A length mismatch (IPv6 candidate vs IPv4 subtree) must not match.
	assert !ip_within_subtree([u8(0x20)], cidr)
}

fn test_enforce_name_constraints_rejects_out_of_subtree() ! {
	ca := Certificate{
		name_constraints_present: true
		permitted_dns:            ['example.com']
	}
	inside := Certificate{
		sans: ['api.example.com']
	}
	enforce_name_constraints(ca, inside)!

	outside := Certificate{
		sans: ['evil.test']
	}
	enforce_name_constraints(ca, outside) or {
		assert err.msg().contains('outside the permitted subtrees')
		return
	}
	assert false
}

fn test_enforce_name_constraints_honors_exclusions() ! {
	ca := Certificate{
		name_constraints_present: true
		excluded_dns:             ['bad.example.com']
	}
	banned := Certificate{
		sans: ['x.bad.example.com']
	}
	enforce_name_constraints(ca, banned) or {
		assert err.msg().contains('excluded subtree')
		return
	}
	assert false
}

// test_parse_certificate_rejects_unsupported_name_constraint_form: a
// nameConstraints subtree over a GeneralName form this validator cannot enforce
// (rfc822Name) must reject the certificate (RFC 5280 §6.1.4), not be dropped into
// an empty constraint set that would silently exempt every subordinate name.
fn test_parse_certificate_rejects_unsupported_name_constraint_form() ! {
	email_base := der_tlv(0x81, ['user@example.com'.bytes()]) // [1] rfc822Name
	subtree := der_tlv(0x30, [email_base])
	permitted := der_tlv(0xa0, [der_tlv(0x30, [subtree])])
	nc_value := der_tlv(0x30, [permitted])
	nc_oid := [u8(0x06), 0x03, 0x55, 0x1d, 0x1e] // 2.5.29.30
	// Non-critical: the rejection must come from the constraint parse, not the
	// unknown-critical-extension path.
	cert := minimal_extension_cert(nc_oid, false, nc_value)
	parse_certificate(cert) or {
		assert err.msg().contains('unsupported nameConstraints general name form')
		return
	}
	assert false
}

// test_parse_certificate_rejects_malformed_ip_length pins that an iPAddress
// subtree that is not address+mask (8 or 32 bytes) is a fatal decode error.
fn test_parse_certificate_rejects_malformed_ip_length() ! {
	bad_ip_base := der_tlv(0x87, [[u8(1), 2, 3, 4, 5]]) // 5 bytes: neither 8 nor 32
	subtree := der_tlv(0x30, [bad_ip_base])
	excluded := der_tlv(0xa1, [der_tlv(0x30, [subtree])])
	nc_value := der_tlv(0x30, [excluded])
	nc_oid := [u8(0x06), 0x03, 0x55, 0x1d, 0x1e] // 2.5.29.30
	cert := minimal_extension_cert(nc_oid, false, nc_value)
	parse_certificate(cert) or {
		assert err.msg().contains('iPAddress subtree has length')
		return
	}
	assert false
}

// test_enforce_name_constraints_ip_only_permitted_rejects_dns locks the
// cross-form fail-open: RFC 5280 §4.2.1.10 treats permittedSubtrees as a single
// namespace, so a permitted set that lists only an iPAddress subtree must still
// reject a subordinate dNS name rather than exempt it, while a subordinate whose
// names fall inside the permitted subtree still passes.
fn test_enforce_name_constraints_ip_only_permitted_rejects_dns() ! {
	ca := Certificate{
		name_constraints_present: true
		permitted_ip:             [[u8(10), 0, 0, 0, 255, 0, 0, 0]]
	}
	// A dNS-only subordinate is outside the (IP-only) permitted namespace.
	dns_child := Certificate{
		sans: ['api.example.com']
	}
	enforce_name_constraints(ca, dns_child) or {
		assert err.msg().contains('outside the permitted subtrees')
		return
	}
	assert false
}

fn test_enforce_name_constraints_permitted_ip_accepts_matching_ip() ! {
	ca := Certificate{
		name_constraints_present: true
		permitted_ip:             [[u8(10), 0, 0, 0, 255, 0, 0, 0]]
	}
	// Positive control: the union gate must not over-reject an in-subtree address.
	ok_child := Certificate{
		ip_sans: [[u8(10), 1, 2, 3]]
	}
	enforce_name_constraints(ca, ok_child)!
}
