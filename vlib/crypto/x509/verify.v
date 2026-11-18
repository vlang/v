module x509

import crypto.ecdsa
import crypto.ed25519
import crypto.sha256
import crypto.sha512
import crypto.slh_dsa
import crypto.subtle
import math.big
import time
import x.crypto.mldsa

// verify.v implements chain building and validation on top of the parser:
// build_chain assembles the longest valid path from a leaf through
// intermediates to a trust anchor, and verify_chain enforces usage,
// validity, revocation and hostname policy on an assembled chain.

// max_chain_len caps DFS depth (and every other chain walk).
const max_chain_len = 16

// VerifyUsage selects which EKU purpose a leaf must assert.
pub enum VerifyUsage {
	server_auth
	client_auth
}

// VerifyOptions tunes verify_chain. The zero value verifies for server
// auth at the current time with no hostname check and no revocation
// inputs; TLS passes explicit roots, hostname, CRLs and staples.
pub struct VerifyOptions {
pub mut:
	usage     VerifyUsage = .server_auth
	hostname  string
	at        time.Time
	crls      []Crl
	ocsp_resp []u8
}

// RsaCrlHash selects the digest for RSA PKCS#1 v1.5 sign/verify in this
// module (CRL signing, chain and CRL verification).
pub enum RsaCrlHash {
	sha256
	sha384
	sha512
}

// --- signature verification (per issuer key type) ---

// rsa_sigalg_hash maps an RSA signature-algorithm OID to its hash.
fn rsa_sigalg_hash(oid string) !RsaCrlHash {
	return match oid {
		'1.2.840.113549.1.1.11' { RsaCrlHash.sha256 }
		'1.2.840.113549.1.1.12' { RsaCrlHash.sha384 }
		'1.2.840.113549.1.1.13' { RsaCrlHash.sha512 }
		else { return error('x509: unsupported RSA signature algorithm') }
	}
}

// ecdsa_sigalg_hash maps an ECDSA signature-algorithm OID to its hash.
// Only the primary OID counts; trailing parameters never select the hash.
fn ecdsa_sigalg_hash(oid string) !RsaCrlHash {
	return match oid {
		'1.2.840.10045.4.3.2' { RsaCrlHash.sha256 }
		'1.2.840.10045.4.3.3' { RsaCrlHash.sha384 }
		'1.2.840.10045.4.3.4' { RsaCrlHash.sha512 }
		else { return error('x509: unsupported ECDSA signature algorithm') }
	}
}

// ecdsa_nid_for_point maps an uncompressed point length to its curve.
fn ecdsa_nid_for_point(point []u8) !ecdsa.Nid {
	return match point.len {
		65 { ecdsa.Nid.prime256v1 }
		97 { ecdsa.Nid.secp384r1 }
		133 { ecdsa.Nid.secp521r1 }
		else { return error('x509: bad EC point length') }
	}
}

// ecdsa_custom_opts hashes exactly once with h, matching OID semantics for
// every curve/hash combination.
fn ecdsa_custom_opts(h RsaCrlHash) ecdsa.SignerOpts {
	return match h {
		.sha256 {
			ecdsa.SignerOpts{
				hash_config:        .with_custom_hash
				allow_custom_hash:  true
				allow_smaller_size: true
				custom_hash:        sha256.new()
			}
		}
		.sha384 {
			ecdsa.SignerOpts{
				hash_config:        .with_custom_hash
				allow_custom_hash:  true
				allow_smaller_size: true
				custom_hash:        sha512.new384()
			}
		}
		.sha512 {
			ecdsa.SignerOpts{
				hash_config:        .with_custom_hash
				allow_custom_hash:  true
				allow_smaller_size: true
				custom_hash:        sha512.new()
			}
		}
	}
}

// sigalg_is_exact reports whether a signatureAlgorithm value is exactly
// one OID TLV (no parameters) naming want. PQ schemes require this shape
// so parameters cannot change the key's meaning.
fn sigalg_is_exact(val []u8, want_oid string) bool {
	if val.len == 0 || val[0] != tag_oid {
		return false
	}
	total := tlv_total_len(val, 0) or { return false }
	if total != val.len {
		return false
	}
	content := expect_tag(val, 0, tag_oid, 'algorithm OID') or { return false }
	return oid_string(content) or { '' } == want_oid
}

// mldsa_kind_from_sigalg maps a signatureAlgorithm value to its ML-DSA
// variant; the value must be exactly one OID (no parameters).
fn mldsa_kind_from_sigalg(val []u8) !mldsa.Kind {
	if sigalg_is_exact(val, oid_mldsa_44) {
		return .ml_dsa_44
	}
	if sigalg_is_exact(val, oid_mldsa_65) {
		return .ml_dsa_65
	}
	if sigalg_is_exact(val, oid_mldsa_87) {
		return .ml_dsa_87
	}
	return error('x509: unsupported ML-DSA signature algorithm')
}

// slh_dsa_params_from_sigalg maps a signatureAlgorithm value to its
// SLH-DSA parameter set; the value must be exactly one OID.
fn slh_dsa_params_from_sigalg(val []u8) !slh_dsa.ParameterSet {
	if sigalg_is_exact(val, oid_slh_dsa_sha2_128s) {
		return .sha2_128s
	}
	if sigalg_is_exact(val, oid_slh_dsa_sha2_128f) {
		return .sha2_128f
	}
	if sigalg_is_exact(val, oid_slh_dsa_sha2_192s) {
		return .sha2_192s
	}
	if sigalg_is_exact(val, oid_slh_dsa_sha2_192f) {
		return .sha2_192f
	}
	if sigalg_is_exact(val, oid_slh_dsa_sha2_256s) {
		return .sha2_256s
	}
	if sigalg_is_exact(val, oid_slh_dsa_sha2_256f) {
		return .sha2_256f
	}
	if sigalg_is_exact(val, oid_slh_dsa_shake_128s) {
		return .shake_128s
	}
	if sigalg_is_exact(val, oid_slh_dsa_shake_128f) {
		return .shake_128f
	}
	if sigalg_is_exact(val, oid_slh_dsa_shake_192s) {
		return .shake_192s
	}
	if sigalg_is_exact(val, oid_slh_dsa_shake_192f) {
		return .shake_192f
	}
	if sigalg_is_exact(val, oid_slh_dsa_shake_256s) {
		return .shake_256s
	}
	if sigalg_is_exact(val, oid_slh_dsa_shake_256f) {
		return .shake_256f
	}
	return error('x509: unsupported SLH-DSA signature algorithm')
}

// rsa_pkcs1_expected rebuilds the PKCS#1 v1.5 encoded message for a digest.
fn rsa_pkcs1_expected(digest []u8, h RsaCrlHash, k int) ![]u8 {
	prefix := match h {
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
	t_len := prefix.len + digest.len
	if k < t_len + 11 {
		return error('x509: RSA key too short for PKCS#1 v1.5')
	}
	mut em := []u8{}
	em << u8(0x00)
	em << u8(0x01)
	for _ in 0 .. k - t_len - 3 {
		em << u8(0xff)
	}
	em << u8(0x00)
	em << prefix
	em << digest
	return em
}

// rsa_int_to_bytes renders an Integer as exactly k big-endian bytes.
fn rsa_int_to_bytes(m big.Integer, k int) ![]u8 {
	m_bytes, _ := m.bytes()
	if m_bytes.len > k {
		return error('x509: RSA value out of range')
	}
	mut em := []u8{len: k}
	start := k - m_bytes.len
	for i in 0 .. m_bytes.len {
		em[start + i] = m_bytes[i]
	}
	return em
}

// rsa_verify_pkcs1 verifies a PKCS#1 v1.5 signature over a digest.
fn rsa_verify_pkcs1(digest []u8, sig []u8, n big.Integer, e u64, h RsaCrlHash) ! {
	k := (n.bit_len() + 7) / 8
	if sig.len != k {
		return error('x509: bad RSA signature length')
	}
	s_int := big.integer_from_bytes(sig, signum: 1)
	m_int := s_int.mod_pow(e, n)
	em := rsa_int_to_bytes(m_int, k)!
	expected := rsa_pkcs1_expected(digest, h, k)!
	if subtle.constant_time_compare(em, expected) != 1 {
		return error('x509: bad signature')
	}
}

// rsa_pub_numbers parses an RSAPublicKey DER into modulus and exponent.
fn rsa_pub_numbers(der []u8) !(big.Integer, u64) {
	content := expect_tag(der, 0, tag_seq, 'RSAPublicKey')!
	mod_raw := expect_tag(content, 0, tag_integer, 'RSA modulus')!
	off := skip_tlv(content, 0)!
	exp_raw := expect_tag(content, off, tag_integer, 'RSA exponent')!
	if skip_tlv(content, off)! != content.len {
		return error('x509: trailing RSAPublicKey data')
	}
	mut mod_slice := mod_raw.clone()
	if mod_slice.len > 1 && mod_slice[0] == 0 {
		mod_slice = mod_slice[1..].clone()
	}
	if mod_slice.len == 0 {
		return error('x509: empty RSA modulus')
	}
	n := big.integer_from_bytes(mod_slice, signum: 1)
	if exp_raw.len == 0 || exp_raw.len > 8 {
		return error('x509: bad RSA exponent')
	}
	mut e_val := u64(0)
	for b in exp_raw {
		e_val = (e_val << 8) | u64(b)
	}
	return n, e_val
}

// verify_child_signature verifies child.tbs_der against an issuer key:
// ECDSA (hash from the sigAlg OID), RSA PKCS#1 v1.5 (hash from the sigAlg
// OID), Ed25519/ML-DSA/SLH-DSA (pure signature, sigAlg OID must match
// the key variant with absent parameters). RSA-PSS and weak hashes fail.
fn verify_child_signature(child &Certificate, parent PublicKey) ! {
	match parent {
		EcdsaP256PublicKey {
			verify_child_ecdsa(child, parent.point)!
		}
		EcdsaP384PublicKey {
			verify_child_ecdsa(child, parent.point)!
		}
		EcdsaP521PublicKey {
			verify_child_ecdsa(child, parent.point)!
		}
		RsaPublicKey {
			h := rsa_sigalg_hash(child.sig_alg_oid)!
			n, e := rsa_pub_numbers(parent.der)!
			digest := match h {
				.sha256 { sha256.sum256(child.tbs_der) }
				.sha384 { sha512.sum384(child.tbs_der) }
				.sha512 { sha512.sum512(child.tbs_der) }
			}
			rsa_verify_pkcs1(digest, child.signature, n, e, h)!
		}
		Ed25519PublicKey {
			if !sigalg_is_exact(child.sig_alg_val, oid_ed25519) {
				return error('x509: Ed25519 sigAlg/key mismatch')
			}
			ok := ed25519.verify(parent.raw, child.tbs_der, child.signature) or { return err }
			if !ok {
				return error('x509: bad signature')
			}
		}
		MlDsa44PublicKey {
			verify_child_mldsa(child, parent.raw, .ml_dsa_44)!
		}
		MlDsa65PublicKey {
			verify_child_mldsa(child, parent.raw, .ml_dsa_65)!
		}
		MlDsa87PublicKey {
			verify_child_mldsa(child, parent.raw, .ml_dsa_87)!
		}
		SlhDsaShake128sPublicKey {
			verify_child_slh_dsa(child, parent.raw, .shake_128s)!
		}
		SlhDsaShake128fPublicKey {
			verify_child_slh_dsa(child, parent.raw, .shake_128f)!
		}
		SlhDsaShake192sPublicKey {
			verify_child_slh_dsa(child, parent.raw, .shake_192s)!
		}
		SlhDsaShake192fPublicKey {
			verify_child_slh_dsa(child, parent.raw, .shake_192f)!
		}
		SlhDsaShake256sPublicKey {
			verify_child_slh_dsa(child, parent.raw, .shake_256s)!
		}
		SlhDsaShake256fPublicKey {
			verify_child_slh_dsa(child, parent.raw, .shake_256f)!
		}
		SlhDsaSha2128sPublicKey {
			verify_child_slh_dsa(child, parent.raw, .sha2_128s)!
		}
		SlhDsaSha2128fPublicKey {
			verify_child_slh_dsa(child, parent.raw, .sha2_128f)!
		}
		SlhDsaSha2192sPublicKey {
			verify_child_slh_dsa(child, parent.raw, .sha2_192s)!
		}
		SlhDsaSha2192fPublicKey {
			verify_child_slh_dsa(child, parent.raw, .sha2_192f)!
		}
		SlhDsaSha2256sPublicKey {
			verify_child_slh_dsa(child, parent.raw, .sha2_256s)!
		}
		SlhDsaSha2256fPublicKey {
			verify_child_slh_dsa(child, parent.raw, .sha2_256f)!
		}
	}
}

// verify_child_ecdsa verifies a child against a P-256/384/521 parent key.
fn verify_child_ecdsa(child &Certificate, point []u8) ! {
	h := ecdsa_sigalg_hash(child.sig_alg_oid)!
	nid := ecdsa_nid_for_point(point)!
	parent_pk := ecdsa.PublicKey.from_uncompressed_bytes(point, ecdsa.CurveOptions{
		nid: nid
	})!
	ok := parent_pk.verify(child.tbs_der, child.signature, ecdsa_custom_opts(h))!
	if !ok {
		return error('x509: bad signature')
	}
}

// verify_child_mldsa verifies a child against a parent ML-DSA key.
fn verify_child_mldsa(child &Certificate, raw []u8, kind mldsa.Kind) ! {
	sig_kind := mldsa_kind_from_sigalg(child.sig_alg_val)!
	if sig_kind != kind {
		return error('x509: ML-DSA sigAlg/key mismatch')
	}
	pk := mldsa.PublicKey.from_bytes(raw, kind)!
	ok := pk.verify(child.tbs_der, child.signature, mldsa.SignerOpts{})!
	if !ok {
		return error('x509: bad signature')
	}
}

// verify_child_slh_dsa verifies a child against an SLH-DSA parent key
// (all 12 parameter sets are instantiated by the pure-V primitive).
fn verify_child_slh_dsa(child &Certificate, raw []u8, params slh_dsa.ParameterSet) ! {
	sig_params := slh_dsa_params_from_sigalg(child.sig_alg_val)!
	if sig_params != params {
		return error('x509: SLH-DSA sigAlg/key mismatch')
	}
	pk := slh_dsa.public_key_from_bytes(params, raw)!
	if !slh_dsa.verify(child.tbs_der, child.signature, [], pk) {
		return error('x509: bad signature')
	}
}

// --- chain building ---

// is_anchor reports trust-anchor membership by exact DER bytes.
fn is_anchor(cert &Certificate, roots []Certificate) bool {
	for r in roots {
		if r.raw == cert.raw {
			return true
		}
	}
	return false
}

// valid_at reports whether cert covers t (inclusive bounds).
fn valid_at(cert &Certificate, t time.Time) bool {
	return cert.not_before.unix() <= t.unix() && t.unix() <= cert.not_after.unix()
}

// check_name_constraints enforces a CA's permitted/excluded dNSName and
// iP subtrees against the child's SAN identities.
fn check_name_constraints(ca &Certificate, child &Certificate) ! {
	if !ca.has_name_constraints {
		return
	}
	for dns in child.dns_names {
		if ca.permitted_dns.len > 0 {
			mut ok := false
			for c in ca.permitted_dns {
				if dns_constraint_matches(c, dns) {
					ok = true
					break
				}
			}
			if !ok {
				return error('x509: name constraint violation')
			}
		}
		for c in ca.excluded_dns {
			if dns_constraint_matches(c, dns) {
				return error('x509: name constraint violation')
			}
		}
	}
	for ip in child.ip_addresses {
		if ca.permitted_ips.len > 0 {
			mut ok := false
			for c in ca.permitted_ips {
				if ip_in_range(ip, c) {
					ok = true
					break
				}
			}
			if !ok {
				return error('x509: name constraint violation')
			}
		}
		for c in ca.excluded_ips {
			if ip_in_range(ip, c) {
				return error('x509: name constraint violation')
			}
		}
	}
}

// valid_link checks one chain edge: issuer match is the caller's job; this
// enforces CA status, keyCertSign, validity, signature, name constraints
// and pathLenConstraint. Trust anchors are exempt from the CA flag only.
// The parent's name constraints apply to every certificate below it in the
// path (RFC 5280 4.2.1.10), not just the immediate child: below_all holds
// the complete below-parent path so a SAN-less subordinate CA cannot
// launder an out-of-scope leaf.
fn valid_link(child &Certificate, parent &Certificate, now time.Time, anchor bool, below_ca int, below_all []Certificate) ! {
	if !valid_at(child, now) || !valid_at(parent, now) {
		return error('x509: certificate not valid at this time')
	}
	if !anchor {
		if !parent.has_basic_constraints || !parent.is_ca {
			return error('x509: parent is not a CA')
		}
	}
	if parent.has_basic_constraints && parent.is_ca && parent.has_path_len {
		if below_ca > parent.path_len {
			return error('x509: path length exceeded')
		}
	}
	if parent.has_key_usage && parent.key_usage & ku_key_cert_sign == 0 {
		return error('x509: parent cannot sign certificates')
	}
	for i in 0 .. below_all.len {
		check_name_constraints(parent, &below_all[i])!
	}
	parent_pk := parent.public_key() or { return error('x509: bad parent public key') }
	verify_child_signature(child, parent_pk)!
}

// build_chain assembles the longest valid path from leaf through
// intermediates to a trust anchor in roots. The direct-root fast path
// (leaf issued by, or equal to, a root) is tried before the DFS. Every
// edge enforces issuer match, CA status, keyCertSign, validity windows,
// signature, name constraints and pathLenConstraint. Name constraints
// propagate down the complete path: each ancestor is checked against
// every certificate below it (RFC 5280 4.2.1.10).
pub fn build_chain(leaf Certificate, intermediates []Certificate, roots []Certificate) ![]Certificate {
	return build_chain_at(leaf, intermediates, roots, time.now())
}

// build_chain_at is build_chain at an explicit time (tests and callers
// with a pinned clock); build_chain uses the current time.
fn build_chain_at(leaf Certificate, intermediates []Certificate, roots []Certificate, now time.Time) ![]Certificate {
	if roots.len == 0 {
		return error('x509: no trust anchors')
	}
	// Cap the DFS pool: intermediates come from the peer flight and an
	// unbounded pool makes path search combinatorial.
	if intermediates.len > 64 {
		return error('x509: too many intermediates')
	}
	if !valid_at(leaf, now) {
		return error('x509: leaf not valid at this time')
	}
	// Direct-root fast path: leaf IS a trust anchor.
	if is_anchor(leaf, roots) {
		return [leaf]
	}
	// Direct-root fast path: leaf issued directly by a root.
	for r in roots {
		if r.subject_der != leaf.issuer_der {
			continue
		}
		valid_link(leaf, r, now, true, 0, [leaf]) or { continue }
		return [leaf, r]
	}
	// Full search over intermediates and cross-signing roots.
	mut pool := intermediates.clone()
	for r in roots {
		pool << r
	}
	mut best := []Certificate{}
	mut path := [leaf]
	dfs_chain(mut path, pool, roots, now, mut best)
	if best.len == 0 {
		return error('x509: no valid chain to a trust anchor')
	}
	return best
}

// dfs_chain explores every issuer-matching edge, keeping the longest path
// that terminates at a trust anchor.
fn dfs_chain(mut path []Certificate, pool []Certificate, roots []Certificate, now time.Time, mut best []Certificate) {
	if path.len > max_chain_len {
		return
	}
	child := path.last()
	for parent in pool {
		if parent.subject_der != child.issuer_der {
			continue
		}
		mut seen := false
		for c in path {
			if c.raw == parent.raw {
				seen = true
				break
			}
		}
		if seen {
			continue
		}
		anchor := is_anchor(parent, roots)
		mut below_ca := 0
		for i in 1 .. path.len {
			if path[i].is_ca {
				below_ca++
			}
		}
		valid_link(child, parent, now, anchor, below_ca, path) or { continue }
		path << parent
		if anchor && path.len > best.len {
			best = path.clone()
		}
		// A root can also act as a cross-signed intermediate on the way
		// to a further anchor, so keep exploring past anchors.
		dfs_chain(mut path, pool, roots, now, mut best)
		path.delete(path.len - 1)
	}
}

// --- chain validation ---

// verify_chain enforces leaf usage (EKU serverAuth/clientAuth),
// keyUsage consistency (leaf digitalSignature), validity at opts.at,
// hostname (server_auth only), CRL revocation and an optional OCSP
// staple on an assembled chain (leaf first, anchor last).
pub fn verify_chain(chain []Certificate, opts VerifyOptions) ! {
	if chain.len == 0 || chain.len > max_chain_len {
		return error('x509: invalid chain length')
	}
	// A zero Time means "now" (year 0 is never a real validation instant).
	at := if opts.at.year == 0 { time.now() } else { opts.at }
	for c in chain {
		if !valid_at(c, at) {
			return error('x509: certificate not valid at this time')
		}
	}
	leaf := chain[0]
	// Leaf keyUsage consistency: TLS leaves sign, so digitalSignature is
	// required whenever the extension is present.
	if leaf.has_key_usage && leaf.key_usage & ku_digital_signature == 0 {
		return error('x509: leaf cannot sign')
	}
	// EKU per usage: absent means any usage; anyExtendedKeyUsage also
	// satisfies both server and client auth.
	want := match opts.usage {
		.server_auth { oid_eku_server_auth }
		.client_auth { oid_eku_client_auth }
	}
	if leaf.ext_key_usage.len > 0 {
		mut ok := false
		for eku in leaf.ext_key_usage {
			if eku == want || eku == oid_eku_any {
				ok = true
				break
			}
		}
		if !ok {
			return error('x509: extended key usage violation')
		}
	}
	if opts.usage == .server_auth && opts.hostname.len > 0 {
		leaf.check_host(opts.hostname)!
	}
	// Revocation: every non-anchor cert against CRLs issued by its parent.
	for i in 0 .. chain.len - 1 {
		cert := chain[i]
		parent := chain[i + 1]
		for crl in opts.crls {
			if crl.issuer_der != parent.subject_der {
				continue
			}
			verify_crl(crl, parent)!
			if crl.is_revoked(cert.serial) {
				return error('x509: certificate revoked')
			}
		}
	}
	// OCSP staple (another transport supplies the bytes; see ocsp.v).
	if opts.ocsp_resp.len > 0 {
		issuer_der := if chain.len > 1 { chain[1].raw } else { chain[0].raw }
		verify_ocsp_staple(opts.ocsp_resp, leaf.raw, issuer_der)!
	}
}
