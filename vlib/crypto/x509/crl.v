module x509

import crypto.ecdsa
import crypto.ed25519
import crypto.pem
import crypto.rand
import crypto.sha1
import crypto.sha256
import crypto.sha512
import crypto.slh_dsa
import math.big
import os
import time
import x.crypto.mldsa

// crl.v implements X.509 certificate revocation lists: DER parsing,
// signature and validity verification, serial lookup, CA-signed
// generation for every supported key type, and env-based CRL inputs.

// Crl is a parsed X.509 CRL (RFC 5280 §5).
pub struct Crl {
pub:
	raw              []u8
	issuer           map[string]string
	issuer_der       []u8
	this_update      time.Time
	next_update      time.Time
	has_next_update  bool
	revoked          []RevokedEntry
	sig_alg_oid      string
	sig_alg_val      []u8
	signature        []u8
	tbs_der          []u8
	crl_number       []u8
	authority_key_id []u8
}

// RevokedEntry is one revokedCertificates entry.
pub struct RevokedEntry {
pub:
	serial     []u8
	revoked_at time.Time
}

// CrlEntry is a revocation input to make_crl.
pub struct CrlEntry {
pub:
	serial     []u8
	revoked_at time.Time
}

// CrlGenOptions tunes make_crl. An empty crl_number gets fresh random
// bytes (like certificate serials).
pub struct CrlGenOptions {
pub:
	this_update     time.Time
	next_update     time.Time
	has_next_update bool
	revoked         []CrlEntry
	crl_number      []u8
}

// CertSignerKind selects the CA key type for CRL/OCSP signing.
pub enum CertSignerKind {
	ecdsa
	rsa
	ed25519
	mldsa
	slh_dsa
}

// CertSigner is a CA signing key of any supported type. The kind field
// selects which key is used; RSA signs PKCS#1 v1.5 with rsa_hash
// (SHA-256 default), ECDSA hashes per curve, the rest sign raw bytes.
pub struct CertSigner {
pub mut:
	kind       CertSignerKind = .ecdsa
	ecdsa_priv ?ecdsa.PrivateKey
	rsa_n      big.Integer
	rsa_e      u64
	rsa_d      big.Integer
	rsa_hash   RsaCrlHash = .sha256
	ed_priv    ed25519.PrivateKey
	mldsa_priv mldsa.PrivateKey
	mldsa_kind mldsa.Kind = .ml_dsa_44
	slh_priv   slh_dsa.PrivateKey
	slh_params slh_dsa.ParameterSet = .shake_128s
}

// sign_tbs signs TBS bytes with signer, returning the signature and the
// signature-algorithm OID (which selects the encoding in make_crl).
pub fn sign_tbs(signer CertSigner, tbs []u8) !([]u8, string) {
	return match signer.kind {
		.ecdsa {
			priv := signer.ecdsa_priv or { return error('x509: missing ECDSA signer key') }
			pub_bytes := priv.public_key()!.bytes()!
			_, sigalg_oid := ecdsa_sig_alg_for_key(pub_bytes)!
			sig := priv.sign(tbs, ecdsa.SignerOpts{})!
			sig, sigalg_oid
		}
		.rsa {
			digest := match signer.rsa_hash {
				.sha256 { sha256.sum256(tbs) }
				.sha384 { sha512.sum384(tbs) }
				.sha512 { sha512.sum512(tbs) }
			}
			sig := rsa_sign_pkcs1(digest, signer.rsa_d, signer.rsa_n, signer.rsa_hash)!
			oid := match signer.rsa_hash {
				.sha256 { '1.2.840.113549.1.1.11' }
				.sha384 { '1.2.840.113549.1.1.12' }
				.sha512 { '1.2.840.113549.1.1.13' }
			}
			sig, oid
		}
		.ed25519 {
			if signer.ed_priv.len != ed25519.private_key_size {
				return error('x509: missing Ed25519 signer key')
			}
			sig := ed25519.sign(signer.ed_priv, tbs)!
			sig, oid_ed25519
		}
		.mldsa {
			sig := signer.mldsa_priv.sign(tbs, mldsa.SignerOpts{ deterministic: true })!
			oid := mldsa_sigalg_oid(signer.mldsa_kind)
			sig, oid
		}
		.slh_dsa {
			sig := slh_dsa.sign_deterministic(tbs, [], signer.slh_priv)!
			sig, slh_dsa_sigalg_oid(signer.slh_params)
		}
	}
}

// rsa_sign_pkcs1 signs a digest with a private exponent (PKCS#1 v1.5).
fn rsa_sign_pkcs1(digest []u8, d big.Integer, n big.Integer, h RsaCrlHash) ![]u8 {
	k := (n.bit_len() + 7) / 8
	em := rsa_pkcs1_expected(digest, h, k)!
	m_int := big.integer_from_bytes(em, signum: 1)
	s_int := m_int.big_mod_pow(d, n)!
	return rsa_int_to_bytes(s_int, k)
}

// parse_crl_pem parses the first X509 CRL block of PEM text.
pub fn parse_crl_pem(pem_text string) !Crl {
	mut rest := pem_text
	mut saw_block := false
	for {
		block, next := pem.decode(rest) or {
			return error(if saw_block {
				'x509: no X509 CRL PEM block found'
			} else {
				'x509: no PEM block found'
			})
		}
		saw_block = true
		if block.block_type == 'X509 CRL' {
			return parse_crl_der(block.data)
		}
		rest = next
	}
	return error('x509: no X509 CRL PEM block found')
}

// parse_crl_der parses one complete DER-encoded CRL without verifying it.
pub fn parse_crl_der(der []u8) !Crl {
	if der.len > 65536 {
		return error('x509: CRL too large')
	}
	body := expect_tag(der, 0, tag_seq, 'CertificateList')!
	if tlv_total_len(der, 0)! != der.len {
		return error('x509: trailing CRL data')
	}
	tbs := expect_tag(body, 0, tag_seq, 'tbsCertList')!
	outer_pos := skip_tlv(body, 0)!
	outer_algorithm := expect_tag(body, outer_pos, tag_seq, 'CRL signature algorithm')!
	parse_algorithm(outer_algorithm)!
	signature_pos := skip_tlv(body, outer_pos)!
	signature := expect_tag(body, signature_pos, tag_bitstring, 'CRL signature')!
	validate_bitstring(signature)!
	if signature.len < 2 || signature[0] != 0
		|| skip_tlv(body, signature_pos)! != body.len {
		return error('x509: invalid CRL signature encoding')
	}
	mut pos := 0
	// Optional version (v2 CRLs carry INTEGER 1).
	if pos < tbs.len && tbs[pos] == tag_integer {
		ver := expect_tag(tbs, pos, tag_integer, 'CRL version')!
		pos = skip_tlv(tbs, pos)!
		if ver.len != 1 || ver[0] > 1 {
			return error('x509: bad CRL version')
		}
	}
	algorithm := expect_tag(tbs, pos, tag_seq, 'CRL TBS signature algorithm')!
	parse_algorithm(algorithm)!
	if algorithm != outer_algorithm {
		return error('x509: CRL signature algorithms differ')
	}
	pos = skip_tlv(tbs, pos)!
	issuer := parse_name(expect_tag(tbs, pos, tag_seq, 'CRL issuer')!)!
	issuer_der, _ := split_tlv(tbs, pos)!
	pos = skip_tlv(tbs, pos)!
	this_update := parse_time(split_crl_time(tbs, pos)!)!
	pos = skip_crl_time(tbs, pos)!
	mut next_update := time.Time{}
	mut has_next := false
	if pos < tbs.len && (tbs[pos] == tag_utctime || tbs[pos] == tag_gentime) {
		next_update = parse_time(split_crl_time(tbs, pos)!)!
		pos = skip_crl_time(tbs, pos)!
		has_next = true
	}
	mut revoked := []RevokedEntry{}
	if pos < tbs.len && tbs[pos] == tag_seq {
		revoked = parse_revoked_certs(expect_tag(tbs, pos, tag_seq, 'revokedCertificates')!)!
		pos = skip_tlv(tbs, pos)!
	}
	mut crl_number := []u8{}
	mut authority_key_id := []u8{}
	if pos < tbs.len && tbs[pos] == 0xa0 {
		ext_outer := expect_tag(tbs, pos, 0xa0, 'CRL extensions')!
		pos = skip_tlv(tbs, pos)!
		exts := expect_tag(ext_outer, 0, tag_seq, 'CRL extensions list')!
		if skip_tlv(ext_outer, 0)! != ext_outer.len {
			return error('x509: trailing CRL extensions data')
		}
		crl_number, authority_key_id = parse_crl_extensions(exts)!
	}
	if pos != tbs.len {
		return error('x509: unexpected tbsCertList field')
	}
	tbs_full, _ := split_tlv(body, 0)!
	sig_oid := oid_string(expect_tag(outer_algorithm, 0, tag_oid, 'algorithm OID')!)!
	return Crl{
		raw:              der.clone()
		issuer:           issuer
		issuer_der:       issuer_der.clone()
		this_update:      this_update
		next_update:      next_update
		has_next_update:  has_next
		revoked:          revoked
		sig_alg_oid:      sig_oid
		sig_alg_val:      outer_algorithm.clone()
		signature:        signature[1..].clone()
		tbs_der:          tbs_full.clone()
		crl_number:       crl_number
		authority_key_id: authority_key_id
	}
}

// split_crl_time returns the full Time TLV at pos.
fn split_crl_time(tbs []u8, pos int) ![]u8 {
	tlv, _ := split_tlv(tbs, pos)!
	if tlv[0] != tag_utctime && tlv[0] != tag_gentime {
		return error('x509: expected CRL time')
	}
	return tlv
}

// skip_crl_time skips one Time TLV.
fn skip_crl_time(tbs []u8, pos int) !int {
	tlv, next := split_tlv(tbs, pos)!
	if tlv[0] != tag_utctime && tlv[0] != tag_gentime {
		return error('x509: expected CRL time')
	}
	return next
}

// parse_revoked_certs reads the revokedCertificates list.
fn parse_revoked_certs(body []u8) ![]RevokedEntry {
	mut out := []RevokedEntry{}
	mut pos := 0
	for pos < body.len {
		if out.len >= 4096 {
			return error('x509: too many revoked certificates')
		}
		entry := expect_tag(body, pos, tag_seq, 'revoked entry')!
		pos = skip_tlv(body, pos)!
		serial := expect_tag(entry, 0, tag_integer, 'revoked serial')!.clone()
		if serial.len == 0 || serial[0] & 0x80 != 0 {
			return error('x509: invalid revoked serial')
		}
		mut epos := skip_tlv(entry, 0)!
		rev_tlv, _ := split_tlv(entry, epos)!
		revoked_at := parse_time(rev_tlv)!
		epos = skip_tlv(entry, epos)!
		if epos < entry.len {
			// crlEntryExtensions: structural skip only.
			_ := expect_tag(entry, epos, tag_seq, 'entry extensions')!
			epos = skip_tlv(entry, epos)!
		}
		if epos != entry.len {
			return error('x509: trailing revoked entry data')
		}
		out << RevokedEntry{
			serial:     serial
			revoked_at: revoked_at
		}
	}
	return out
}

// parse_crl_extensions reads CRLNumber and authorityKeyId, rejecting
// unknown critical CRL extensions fail-closed (mirroring certificates).
fn parse_crl_extensions(body []u8) !([]u8, []u8) {
	mut crl_number := []u8{}
	mut aki := []u8{}
	mut seen := map[string]bool{}
	mut pos := 0
	mut count := 0
	for pos < body.len {
		if count >= max_extensions {
			return error('x509: too many CRL extensions')
		}
		count++
		inner := expect_tag(body, pos, tag_seq, 'CRL extension')!
		pos = skip_tlv(body, pos)!
		oid := oid_string(expect_tag(inner, 0, tag_oid, 'CRL extension OID')!)!
		if oid in seen {
			return error('x509: duplicate CRL extension `${oid}`')
		}
		seen[oid] = true
		mut ip := skip_tlv(inner, 0)!
		mut critical := false
		if ip < inner.len && inner[ip] == 0x01 {
			crit := expect_tag(inner, ip, 0x01, 'critical BOOLEAN')!
			if crit.len != 1 || crit[0] != 0xff {
				return error('x509: invalid critical BOOLEAN')
			}
			critical = true
			ip = skip_tlv(inner, ip)!
		}
		octets := expect_tag(inner, ip, tag_octet_string, 'CRL extension value')!
		if skip_tlv(inner, ip)! != inner.len {
			return error('x509: trailing CRL extension data')
		}
		match oid {
			'2.5.29.20' {
				num := expect_tag(octets, 0, tag_integer, 'CRLNumber')!
				if skip_tlv(octets, 0)! != octets.len || num.len == 0 || num.len > 20 {
					return error('x509: invalid CRLNumber')
				}
				crl_number = num.clone()
			}
			oid_authority_key_id {
				aki = parse_authority_key_id(octets)!
			}
			else {
				if critical {
					return error('x509: unsupported critical CRL extension `${oid}`')
				}
			}
		}
	}
	return crl_number, aki
}

// is_revoked reports whether serial appears in the CRL (sign-byte
// tolerant, like serial_equal).
pub fn (c &Crl) is_revoked(serial []u8) bool {
	for r in c.revoked {
		if serial_equal(r.serial, serial) {
			return true
		}
	}
	return false
}

// verify_crl checks issuer match, the validity window, cRLSign key
// usage on the issuer, and the CRL signature.
pub fn verify_crl(crl &Crl, issuer &Certificate) ! {
	if crl.issuer_der != issuer.subject_der {
		return error('x509: CRL issuer mismatch')
	}
	now := time.now()
	if now.unix() < crl.this_update.unix() {
		return error('x509: CRL not yet valid')
	}
	if crl.has_next_update && now.unix() > crl.next_update.unix() {
		return error('x509: CRL expired')
	}
	if issuer.has_key_usage && issuer.key_usage & ku_crl_sign == 0 {
		return error('x509: issuer cannot sign CRLs')
	}
	pk := issuer.public_key()!
	probe := Certificate{
		tbs_der:     crl.tbs_der
		sig_alg_oid: crl.sig_alg_oid
		sig_alg_val: crl.sig_alg_val
		signature:   crl.signature
	}
	verify_child_signature(&probe, pk)!
}

// make_crl builds and signs a CRL for issuer's subject, returning the
// parsed CRL and its DER. revoked serials must be the DER INTEGER
// contents of revoked certificates (high bit clear).
pub fn make_crl(issuer &Certificate, opts CrlGenOptions, signer CertSigner) !(Crl, []u8) {
	if opts.revoked.len > 4096 {
		return error('x509: too many revoked certificates')
	}
	issuer_der := extract_subject_der(issuer.raw)!
	mut number := opts.crl_number.clone()
	if number.len == 0 {
		number = rand.bytes(8)!
		for number.len > 1 && number[0] == 0 {
			number = number[1..].clone()
		}
		number[0] &= 0x7f
		if number[0] == 0 {
			number[0] = 1
		}
	}
	// The sigAlg OID comes from the signer, so resolve it before TBS
	// assembly; the final signature binds the assembled TBS below.
	sig_oid := signer_sigalg_oid(signer)!
	sig_alg_id := crl_sig_alg_id(sig_oid, signer.kind)!
	return make_crl_assembled(issuer, issuer_der, number, opts, signer, sig_alg_id)
}

// signer_sigalg_oid resolves the signature-algorithm OID without signing.
fn signer_sigalg_oid(signer CertSigner) !string {
	return match signer.kind {
		.ecdsa {
			priv := signer.ecdsa_priv or { return error('x509: missing ECDSA signer key') }
			pub_bytes := priv.public_key()!.bytes()!
			_, sigalg_oid := ecdsa_sig_alg_for_key(pub_bytes)!
			sigalg_oid
		}
		.rsa {
			match signer.rsa_hash {
				.sha256 { '1.2.840.113549.1.1.11' }
				.sha384 { '1.2.840.113549.1.1.12' }
				.sha512 { '1.2.840.113549.1.1.13' }
			}
		}
		.ed25519 {
			oid_ed25519
		}
		.mldsa {
			mldsa_sigalg_oid(signer.mldsa_kind)
		}
		.slh_dsa {
			slh_dsa_sigalg_oid(signer.slh_params)
		}
	}
}

// crl_sig_alg_id encodes the AlgorithmIdentifier: RSA carries a NULL
// parameter (openssl style); ECDSA and PQ schemes carry absent
// parameters (matching this module's certificate style).
fn crl_sig_alg_id(sig_oid string, kind CertSignerKind) ![]u8 {
	mut content := []u8{}
	for b in encode_oid(sig_oid) {
		content << b
	}
	if kind == .rsa {
		content << der_tlv(0x05, []u8{})
	}
	return der_seq(content)
}

// der_integer_bytes wraps bytes as a DER INTEGER, prepending a zero
// sign byte when the high bit is set (canonical positive encoding).
fn der_integer_bytes(b []u8) []u8 {
	if b.len > 0 && b[0] & 0x80 != 0 {
		mut padded := []u8{}
		padded << u8(0)
		for x in b {
			padded << x
		}
		return der_int(padded)
	}
	return der_int(b)
}

// make_crl_assembled builds the final TBS with the real sigAlg,
// signs it, and wraps the CertificateList.
fn make_crl_assembled(issuer &Certificate, issuer_der []u8, number []u8, opts CrlGenOptions, signer CertSigner, sig_alg_id []u8) !(Crl, []u8) {
	mut tbs := []u8{}
	// TBSCertList version is a bare INTEGER (v2 == 1), unlike the
	// [0]-tagged version in TBSCertificate.
	tbs << der_int([u8(1)])
	for b in sig_alg_id {
		tbs << b
	}
	for b in issuer_der {
		tbs << b
	}
	for b in der_utc(opts.this_update) {
		tbs << b
	}
	if opts.has_next_update {
		for b in der_utc(opts.next_update) {
			tbs << b
		}
	}
	if opts.revoked.len > 0 {
		mut list := []u8{}
		for r in opts.revoked {
			if r.serial.len == 0 || r.serial.len > 20 {
				return error('x509: invalid revoked serial')
			}
			mut entry := []u8{}
			entry << der_integer_bytes(r.serial)
			for b in der_utc(r.revoked_at) {
				entry << b
			}
			list << der_seq(entry)
		}
		tbs << der_seq(list)
	}
	// Extensions: CRLNumber + authorityKeyId (issuer SKI).
	aki := issuer_ski_for_crl(issuer, signer)!
	mut exts_inner := []u8{}
	mut num_content := []u8{}
	for b in encode_oid('2.5.29.20') {
		num_content << b
	}
	for b in der_octstr(der_integer_bytes(number)) {
		num_content << b
	}
	exts_inner << der_seq(num_content)
	if aki.len > 0 {
		mut aki_content := []u8{}
		for b in encode_oid(oid_authority_key_id) {
			aki_content << b
		}
		mut kid := []u8{}
		kid << der_tlv(0x80, aki)
		for b in der_octstr(der_seq(kid)) {
			aki_content << b
		}
		exts_inner << der_seq(aki_content)
	}
	tbs << der_ctx(0, der_seq(exts_inner))
	tbs_der := der_seq(tbs)
	sig, _ := sign_tbs(signer, tbs_der)!
	mut cert_list := []u8{}
	for b in tbs_der {
		cert_list << b
	}
	for b in sig_alg_id {
		cert_list << b
	}
	for b in der_bitstr(sig) {
		cert_list << b
	}
	crl_der := der_seq(cert_list)
	crl := parse_crl_der(crl_der)!
	return crl, crl_der
}

// issuer_ski_for_crl resolves the AKI value: the issuer certificate's
// subjectKeyId when present, else the signer's public-key SHA-1
// (RFC 5280 §4.2.1.2 method 1: hash of the BIT STRING value).
fn issuer_ski_for_crl(issuer &Certificate, signer CertSigner) ![]u8 {
	if issuer.subject_key_id.len > 0 {
		return issuer.subject_key_id.clone()
	}
	raw := match signer.kind {
		.ecdsa {
			priv := signer.ecdsa_priv or { return error('x509: missing ECDSA signer key') }
			priv.public_key()!.bytes()!
		}
		.rsa {
			if signer.rsa_e == 0 {
				return error('x509: missing RSA signer exponent')
			}
			rsa_pubkey_der(signer.rsa_n, signer.rsa_e)!
		}
		.ed25519 {
			if signer.ed_priv.len != ed25519.private_key_size {
				return error('x509: missing Ed25519 signer key')
			}
			signer.ed_priv.public_key()
		}
		.mldsa {
			signer.mldsa_priv.public_key().bytes()
		}
		.slh_dsa {
			signer.slh_priv.public_key().bytes()
		}
	}
	return sha1.sum(raw)
}

// rsa_pubkey_der encodes an RSAPublicKey DER (SEQUENCE of modulus and
// exponent) for SKI hashing and SPKI assembly.
fn rsa_pubkey_der(n big.Integer, e u64) ![]u8 {
	n_bytes, _ := n.bytes()
	if n_bytes.len == 0 {
		return error('x509: empty RSA modulus')
	}
	mut content := []u8{}
	content << der_integer_bytes(n_bytes)
	mut e_raw := []u8{}
	mut ev := e
	for ev > 0 {
		e_raw << u8(ev & 0xff)
		ev >>= 8
	}
	mut e_be := []u8{}
	for i := e_raw.len - 1; i >= 0; i-- {
		e_be << e_raw[i]
	}
	content << der_integer_bytes(e_be)
	return der_seq(content)
}

// load_system_crls reads revocation inputs without new Config fields,
// mirroring the SSL_CERT_FILE pattern: SSL_CRL_FILE (explicit bundle),
// SSL_CRL_DIR (directory scan), plus the SSL_CERT_FILE sibling
// directory (*.crl/*.pem). Unparsable entries are skipped.
pub fn load_system_crls() []Crl {
	mut out := []Crl{}
	path := os.getenv('SSL_CRL_FILE')
	if path.len > 0 {
		out << load_crl_bundle_file(path)
	}
	dir := os.getenv('SSL_CRL_DIR')
	if dir.len > 0 {
		out << load_crl_bundle_dir(dir)
	}
	cert_path := os.getenv('SSL_CERT_FILE')
	if cert_path.len > 0 {
		out << load_crl_bundle_dir(os.dir(cert_path))
	}
	return out
}

// load_crl_bundle_file reads one file as PEM (X509 CRL blocks) or raw DER.
fn load_crl_bundle_file(path string) []Crl {
	mut out := []Crl{}
	data := os.read_file(path) or { return out }
	if data.contains('-----BEGIN ') {
		mut rest := data
		for {
			block, next := pem.decode(rest) or { break }
			if block.block_type == 'X509 CRL' {
				crl := parse_crl_der(block.data) or {
					rest = next
					if next.len == 0 {
						break
					}
					continue
				}
				out << crl
			}
			rest = next
			if rest.len == 0 {
				break
			}
		}
		return out
	}
	crl := parse_crl_der(data.bytes()) or { return out }
	out << crl
	return out
}

// load_crl_bundle_dir scans every file in dir for CRLs.
fn load_crl_bundle_dir(dir string) []Crl {
	mut out := []Crl{}
	if dir.len == 0 {
		return out
	}
	entries := os.ls(dir) or { return out }
	for e in entries {
		if !(e.ends_with('.crl') || e.ends_with('.pem') || e.ends_with('.der')) {
			continue
		}
		out << load_crl_bundle_file(os.join_path(dir, e))
	}
	return out
}
