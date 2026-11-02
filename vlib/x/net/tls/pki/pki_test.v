// Copyright (c) 2026 blackshirt.
//
// Validator- and helper-level PKI tests: hostname / IP-literal matching, OIDs,
// iPAddress SANs, chain trust anchoring, the new_validator /
// new_insecure_validator split, the RSA leaf-key floor and its escape hatch,
// the verify_cert_signature error contract, and trust-store PEM bundle
// resilience. Extension parsers live in x509_test.v.
module pki

import encoding.base64
import math.big

// test_hostname_matching pins the RFC 6125 §6.4.3 matcher rules: exact
// case-insensitive equality, a single-label leftmost wildcard that cannot cross
// a dot boundary or match an empty prefix, and the TLD / IP-literal guards that
// keep an over-broad '*' from authorizing unrelated namespaces. Only a full
// leftmost '*' is special — a bare '*', a partial-label '*x', or an inner '.*'
// must never wildcard (CA/Browser Forum Baseline).
fn test_hostname_matching() {
	// Exact matches
	assert match_hostname('example.com', 'example.com')
	assert match_hostname('EXAMPLE.COM', 'example.com')
	assert match_hostname('api.example.com', 'API.EXAMPLE.COM')

	// Wildcard matches
	assert match_hostname('*.example.com', 'foo.example.com')
	assert match_hostname('*.example.com', 'api.example.com')
	assert match_hostname('*.EXAMPLE.COM', 'api.example.com')

	// Negative wildcard matches
	assert !match_hostname('*.example.com', 'example.com') // Cannot match empty prefix

	assert !match_hostname('*.example.com', 'sub.api.example.com') // Cannot cross label boundaries

	assert !match_hostname('*.example.com', 'foo.badexample.com')

	// Only a leftmost full-label '*' is special (CA/Browser Baseline profile):
	// a bare '*', a partial-label '*x', or an inner '.*.' must never wildcard.
	assert !match_hostname('*', 'example.com')
	assert !match_hostname('*x.example.com', 'ax.example.com')
	assert !match_hostname('foo.*.com', 'foo.bar.com')

	// Wildcards must never match TLDs (RFC 6125 §6.4.3)
	assert !match_hostname('*.com', 'example.com')
	assert !match_hostname('*.org', 'site.org')

	// Wildcards must never match IP addresses (RFC 6125 §6.4.3)
	assert !match_hostname('*.1.1.1', '1.1.1.1')
	assert !match_hostname('*.168.1.1', '192.168.1.1')
}

// test_verify_hostnames pins the SAN-vs-CN precedence rule of RFC 6125 §6.4.4:
// the presence of ANY dNSName SAN disables CN checking entirely; the CN is only
// consulted for legacy certs with no SAN list at all.
fn test_verify_hostnames() {
	// SAN list present but non-matching while the CN WOULD match: must still
	// fail — this is the exact confused-deputy case that deprecated CN checks.
	assert !verify_hostnames(['a.example'], 'example.com', 'example.com')

	// Any matching SAN wins; non-matching siblings and the CN are irrelevant.
	assert verify_hostnames(['a.example', 'b.example'], 'wrong.cn', 'b.example')

	// Wildcards inside the SAN list follow the matcher's rules.
	assert verify_hostnames(['*.example.com'], '', 'api.example.com')
	assert !verify_hostnames(['*.example.com'], '', 'a.b.example.com')

	// No SANs: legacy CN fallback, wildcard included.
	assert verify_hostnames([], 'example.com', 'example.com')
	assert verify_hostnames([], '*.example.com', 'api.example.com')

	// Neither SANs nor CN: nothing can authorize the name.
	assert !verify_hostnames([], '', 'example.com')
}

// test_asn1_oid_parsing pins parse_oid's DER varint decoding: the first two
// arcs are packed into one subidentifier (first*40+second), multi-byte base-128
// subidentifiers unfold correctly, and an unterminated continuation byte yields
// '' rather than a silently truncated OID.
fn test_asn1_oid_parsing() {
	// OID 1.2.840.113549.1.1.1 (rsaEncryption) = 2a 86 48 86 f7 0d 01 01 01
	rsa_oid_bytes := [u8(0x2a), 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x01]
	assert parse_oid(rsa_oid_bytes) == '1.2.840.113549.1.1.1'

	// OID 2.5.4.3 (commonName) = 55 04 03
	cn_oid_bytes := [u8(0x55), 0x04, 0x03]
	assert parse_oid(cn_oid_bytes) == '2.5.4.3'

	// OID 2.100.3: first subidentifier = 2*40 + 100 = 180, a multi-byte varint
	// (81 34), followed by 03.
	multi_byte_first := [u8(0x81), 0x34, 0x03]
	assert parse_oid(multi_byte_first) == '2.100.3'

	// Unterminated varint in OID must return empty string
	truncated_oid := [u8(0x2a), 0x86, 0x48, 0x86]
	assert parse_oid(truncated_oid) == ''
}

// test_ip_san_matching pins the raw-iPAddress SAN comparison: IPv4 has a single
// canonical dotted-decimal spelling so a byte-for-byte rebuild matches, while
// IPv6 must be canonicalized on both sides so '::1' and its fully-expanded form
// agree on the same 16-byte SAN. Non-address names and empty SAN lists never
// match (fail closed).
fn test_ip_san_matching() {
	// IPv4 SAN 1.1.1.1
	ipv4 := [u8(1), 1, 1, 1]
	assert ip_san_matches([ipv4], '1.1.1.1')
	assert !ip_san_matches([ipv4], '1.1.1.2')
	assert !ip_san_matches([ipv4], 'example.com')

	// IPv6 SAN ::1 (15 zero bytes followed by 0x01)
	mut ipv6 := []u8{len: 16}
	ipv6[15] = 1
	assert ip_san_matches([ipv6], '::1')
	assert ip_san_matches([ipv6], '0:0:0:0:0:0:0:1')
	assert !ip_san_matches([ipv6], '::2')

	// Both families in one certificate
	assert ip_san_matches([ipv4, ipv6], '::1')
	assert !ip_san_matches([], '1.1.1.1')
}

// test_system_trust_store_loading smoke-checks the loader against the host's
// real CA store, so it presumes an installed bundle (Linux ca-certificates,
// macOS /etc/ssl/cert.pem, etc.). Path probing and PEM slicing live here rather
// than in the parser tests because they depend on os.exists and file layout,
// not on crafted certificate bytes.
fn test_system_trust_store_loading() {
	mut ts := new_trust_store()
	count := ts.load_system_ca_bundle() or { 0 }
	// On this Linux system, ca-certificates should contain > 50 root CAs
	assert count > 0
}

// test_is_ip_literal pins the address/name classifier: valid dotted-quad IPv4
// (with whitespace trimmed) and any colon-bearing IPv6 are literals, while a
// wrong label count, an out-of-range octet, a non-numeric label, or an empty
// string are ordinary DNS names. Wildcard-IP rejection in match_hostname
// depends on exactly these boundaries.
fn test_is_ip_literal() {
	assert is_ip_literal('192.168.1.1')
	assert is_ip_literal('0.0.0.0')
	assert is_ip_literal(' 10.0.0.1 ')
	assert is_ip_literal('::1')
	assert is_ip_literal('2001:db8::1')

	// Wrong number of dot-separated labels.
	assert !is_ip_literal('1.2.3')
	assert !is_ip_literal('1.2.3.4.5')
	// An octet outside 0..255 is not a valid IPv4 byte.
	assert !is_ip_literal('300.1.1.1')
	// A non-numeric label disqualifies the dotted quad.
	assert !is_ip_literal('1.2.3.x')
	// Empty host and ordinary names are never addresses.
	assert !is_ip_literal('')
	assert !is_ip_literal('example.com')
}

// test_verify_certificate_chain_rejects_empty_hostname: an empty server name
// cannot be matched against a certificate, so verification must fail closed.
// Previously an empty name skipped both SAN and CN checks, letting a dial
// address with no host part accept any certificate chaining to a trusted root.
fn test_verify_certificate_chain_rejects_empty_hostname() ! {
	validator := new_validator(new_trust_store())
	leaf := Certificate{}
	validator.verify_certificate_chain([leaf], '') or {
		assert err.msg().contains('empty server name')
		return
	}
	assert false
}

// test_chain_trust_anchor_can_precede_cross_signed_tail locks the anchor
// placement rule in chain_has_trusted_anchor: a chain that carries the trusted
// root anywhere (leaf, cross-signed intermediate, or a tail past the intended
// anchor) is accepted, while a chain that omits the trusted root entirely is
// refused. Servers commonly append a cross-signed copy of their issuing CA after
// the intermediate, so the trust path can terminate before the last element.
fn test_chain_trust_anchor_can_precede_cross_signed_tail() {
	root := Certificate{
		raw:             [u8(0x01)]
		is_ca:           true
		not_before_unix: 1
		not_after_unix:  200
	}
	chain := [
		Certificate{ raw: [u8(0x02)] },
		Certificate{ raw: [u8(0x01)] },
		Certificate{ raw: [u8(0x03)] },
	]
	assert chain_has_trusted_anchor(chain, [root], 100)
	assert !chain_has_trusted_anchor([chain[0], chain[2]], [root], 100)
}

// rsa_leaf_of_bits builds a leaf Certificate carrying an RSA public key whose
// modulus is `bits` long, enough to exercise check_leaf_rsa_floor without a real
// signed chain.
fn rsa_leaf_of_bits(bits int) Certificate {
	modulus := []u8{len: bits / 8, init: 0xab}
	key := RsaPublicKey{
		n:          big.Integer{}
		e:          u64(65537)
		size_bytes: modulus.len
	}
	return Certificate{
		public_key_algo: .rsa
		rsa_public_key:  key
	}
}

// test_leaf_rsa_floor_default_is_2048 exercises the default RSA leaf floor: a
// 1024-bit modulus is refused and the size is carried in the error message so
// operators can tell a policy reject from a decode failure.
fn test_leaf_rsa_floor_default_is_2048() ! {
	// A 1024-bit leaf key is refused under the default floor.
	check_leaf_rsa_floor(rsa_leaf_of_bits(1024), default_min_rsa_bits) or {
		assert err.msg().contains('leaf RSA key too short')
		assert err.msg().contains('1024 bits')
		return
	}
	assert false
}

// test_leaf_rsa_floor_accepts_2048_and_above is the positive control: keys
// exactly at (2048) and above (3072) the CA/Browser Forum baseline pass under
// the default floor.
fn test_leaf_rsa_floor_accepts_2048_and_above() ! {
	check_leaf_rsa_floor(rsa_leaf_of_bits(2048), default_min_rsa_bits)!
	check_leaf_rsa_floor(rsa_leaf_of_bits(3072), default_min_rsa_bits)!
}

// test_leaf_rsa_floor_ignores_non_rsa: ECDSA and Ed25519 leaves have no RSA
// modulus, so the floor is a no-op for them rather than a spurious error.
fn test_leaf_rsa_floor_ignores_non_rsa() ! {
	// An EC/Ed25519 leaf has no RSA modulus; the floor must not touch it.
	check_leaf_rsa_floor(Certificate{
		public_key_algo: .ed25519
	}, default_min_rsa_bits)!
}

// test_set_min_rsa_bits_escape_hatch pins the three-way contract of the setter:
// the default is restored by 0, a deliberate downgrade is honored down to
// min_rsa_bits_floor (1024), and any value below the floor is clamped up — so
// the escape hatch cannot silently accept breakable keys while still allowing
// documented legacy-interop relaxations.
fn test_set_min_rsa_bits_escape_hatch() {
	mut v := new_validator(new_trust_store())
	assert v.effective_min_rsa_bits() == default_min_rsa_bits
	// Deliberate downgrade to the floor is honored.
	v.set_min_rsa_bits(1024)
	assert v.effective_min_rsa_bits() == 1024
	check_leaf_rsa_floor(rsa_leaf_of_bits(1024), v.effective_min_rsa_bits())!
	// Anything below the floor is clamped up, not accepted.
	v.set_min_rsa_bits(512)
	assert v.effective_min_rsa_bits() == min_rsa_bits_floor
	// Zero restores the default.
	v.set_min_rsa_bits(0)
	assert v.effective_min_rsa_bits() == default_min_rsa_bits
}

// test_new_validator_always_verifies locks the misuse-resistance guarantee: the
// general-purpose constructor exposes no way to disable checks, so even against an
// empty store a chain is still verified (and here refused) rather than skipped.
fn test_new_validator_always_verifies() ! {
	mut v := new_validator(new_trust_store())
	v.verify_certificate_chain([]Certificate{}, 'example.com') or {
		assert err.msg().contains('empty certificate chain')
		return
	}
	assert false
}

// test_new_insecure_validator_skips_everything pins that the named escape hatch
// short-circuits the very first guard, so the dangerous path exists only under an
// explicit, greppable constructor name.
fn test_new_insecure_validator_skips_everything() ! {
	mut v := new_insecure_validator()
	v.verify_certificate_chain([]Certificate{}, 'example.com')!
}

// test_verify_cert_signature_errors_on_tampered_rsa_signature locks the
// misuse-resistant verify_cert_signature shape: a bad signature must surface
// as an error, never as a silently-ignored Ok(false) that the main chain loop's
// `or { ... }` guard would discard and treat as verified. Uses a length-
// mismatched signature against an RSA parent so rsa_verify_raw's own guard
// fires deterministically without a real signed fixture.
fn test_verify_cert_signature_errors_on_tampered_rsa_signature() ! {
	parent := rsa_leaf_of_bits(2048)
	child := Certificate{
		signature_algo_oid: '1.2.840.113549.1.1.11' // sha256WithRSAEncryption
		signature:          [u8(0xaa)]            // wrong length: not 256 bytes
		tbs_raw:            [u8(0xbb)]
	}
	verify_cert_signature(child, parent) or {
		assert err.msg().contains('signature length')
		return
	}
	assert false
}

// der_tlv wraps `tag` + concatenated `parts` in a definite-length DER header.
// Duplicated from x509_test.v because V compiles each _test.v as its own
// binary and cannot see siblings; kept minimal (three length forms) so the
// PEM-resync test can assemble a real, parseable certificate DER.
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

// minimal_valid_cert_der assembles a v1 X.509 DER blob that parse_certificate
// accepts end to end: six mandatory TBSCertificate elements (serial, sig_alg,
// issuer, validity, subject, SPKI), an ECDSA prime256v1 SPKI with a
// syntactically well-formed uncompressed point, and a 2-byte BIT STRING
// signature. parse_certificate only stores raw bytes; the on-curve check
// lives in ecdsa_public_key() and is off the add_pem path exercised here.
fn minimal_valid_cert_der() []u8 {
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
	tbs := der_tlv(0x30, [serial, sig_alg, issuer, validity, subject, spki])
	sig_bits := [u8(0x00), 0x01, 0x02]
	sig_val := der_tlv(0x03, [sig_bits])
	return der_tlv(0x30, [tbs, sig_alg, sig_val])
}

// test_add_pem_resyncs_past_malformed_block guards the fail-open regression
// a naive `for blk := pem.decode(rest)` loop would introduce: one corrupt
// envelope in a 150-root system bundle would halt iteration and silently
// drop every anchor behind it. add_pem splits on the BEGIN marker so each
// block is decoded independently — a bad block costs only itself.
fn test_add_pem_resyncs_past_malformed_block() ! {
	der := minimal_valid_cert_der()
	valid_pem := '-----BEGIN CERTIFICATE-----\n${base64.encode(der)}\n-----END CERTIFICATE-----\n'
	// Middle block has a BEGIN marker but no matching END — pem.decode_only
	// returns none, and add_pem must skip it and carry on to the third chunk.
	malformed_pem := '-----BEGIN CERTIFICATE-----\nthis-is-not-a-complete-pem-envelope\n'
	bundle := valid_pem + malformed_pem + valid_pem

	mut ts := new_trust_store()
	count := ts.add_pem(bundle)!
	assert count == 2
}

// test_add_pem_rejects_oversized_bundle: the input byte cap fires BEFORE
// split, so a hostile file never causes a chunk-vector allocation. Assert
// an error, not a silent partial load — silently returning a small count
// would leave the trust store looking healthy while most anchors are missing.
fn test_add_pem_rejects_oversized_bundle() ! {
	oversized := 'A'.repeat(max_pem_bundle_bytes + 1)
	mut ts := new_trust_store()
	ts.add_pem(oversized) or {
		assert err.msg().contains('exceeds')
		return
	}
	assert false
}

// test_add_pem_skips_oversized_der_block: a syntactically valid PEM envelope
// whose base64 body decodes past max_certificate_der_bytes must be skipped
// BEFORE parse_certificate runs. Without the cap, a 1 MB DER would push the
// ASN.1 parser through deep work for no security value; with it, the block
// is dropped and the loop continues normally.
fn test_add_pem_skips_oversized_der_block() ! {
	big_der := []u8{len: max_certificate_der_bytes + 1, init: u8(0x30)}
	huge_pem := '-----BEGIN CERTIFICATE-----\n${base64.encode(big_der)}\n-----END CERTIFICATE-----\n'
	mut ts := new_trust_store()
	count := ts.add_pem(huge_pem)!
	assert count == 0
}

// test_parse_certificate_rejects_oversized_der: the parser-level DER size
// cap fires before any ASN.1 work. Complements the add_pem block-level cap
// above so a caller bypassing add_pem (add_der or the handshake path) hits
// the same ceiling.
fn test_parse_certificate_rejects_oversized_der() ! {
	oversized := []u8{len: max_certificate_der_bytes + 1, init: u8(0x30)}
	parse_certificate(oversized) or {
		assert err.msg().contains('exceeds')
		return
	}
	assert false
}

// test_chain_has_trusted_anchor_skips_non_ca_root: RFC 5280 §6.1.1 anchors
// are CA certificates. Without the is_ca guard, a leaf misguidedly placed
// in the trust store via add_der would anchor any chain it byte-matched.
fn test_chain_has_trusted_anchor_skips_non_ca_root() ! {
	far_future := i64(4102444800) // year 2100
	non_ca := Certificate{
		is_ca:           false
		not_before_unix: 0
		not_after_unix:  far_future
		raw:             [u8(0xaa)]
	}
	// Raw bytes match on both sides, but is_ca=false must veto the anchor.
	assert !chain_has_trusted_anchor([non_ca], [non_ca], 100)

	ca := Certificate{
		is_ca:           true
		not_before_unix: 0
		not_after_unix:  far_future
		raw:             [u8(0xaa)]
	}
	// Same content, is_ca=true → anchors via the raw-equality fast path.
	assert chain_has_trusted_anchor([ca], [ca], 100)
}

// test_chain_has_trusted_anchor_skips_root_without_key_cert_sign: a CA that
// asserts keyUsage but omits keyCertSign (bit 5) cannot anchor, mirroring
// the same rule already enforced on intermediates at chain index i+1.
fn test_chain_has_trusted_anchor_skips_root_without_key_cert_sign() ! {
	far_future := i64(4102444800)
	root := Certificate{
		is_ca:           true
		has_key_usage:   true
		key_usage:       [u8(0x80)] // bit 0 = digitalSignature only; no keyCertSign
		not_before_unix: 0
		not_after_unix:  far_future
		raw:             [u8(0xaa)]
	}
	assert !chain_has_trusted_anchor([root], [root], 100)
	// Absent keyUsage leaves the anchor unconstrained (RFC 5280 §4.2.1.3).
	unconstrained := Certificate{
		is_ca:           true
		has_key_usage:   false
		not_before_unix: 0
		not_after_unix:  far_future
		raw:             [u8(0xaa)]
	}
	assert chain_has_trusted_anchor([unconstrained], [unconstrained], 100)
}

// test_match_hostname_rejects_empty_pattern_or_host: defense in depth
// against a cert carrying an empty dNSName SAN entry, which would
// otherwise succeed via the `p == h` shortcut when the hostname is also
// empty. Upstream verify_certificate_chain already rejects an empty
// hostname, but match_hostname must not silently re-open that path.
fn test_match_hostname_rejects_empty_pattern_or_host() ! {
	assert !match_hostname('', 'example.com')
	assert !match_hostname('example.com', '')
	assert !match_hostname('', '')
	assert !match_hostname('   ', 'example.com') // trim_space -> ''
	// Positive control: ordinary equality still works.
	assert match_hostname('example.com', 'example.com')
}
