module x509

import crypto.rand
import x.crypto.mldsa

// mldsa_sigalg_oid maps an ML-DSA kind to its signature-algorithm OID
// (which doubles as the SPKI algorithm OID per
// draft-ietf-lamps-dilithium-certificates).
fn mldsa_sigalg_oid(kind mldsa.Kind) string {
	return match kind {
		.ml_dsa_44 { oid_mldsa_44 }
		.ml_dsa_65 { oid_mldsa_65 }
		.ml_dsa_87 { oid_mldsa_87 }
	}
}

// mldsa_spki builds a SubjectPublicKeyInfo for a raw ML-DSA public key:
// SEQUENCE { SEQUENCE { OID }, BIT STRING raw } with absent parameters.
fn mldsa_spki(raw []u8, kind mldsa.Kind) []u8 {
	mut alg_content := []u8{}
	for b in encode_oid(mldsa_sigalg_oid(kind)) { alg_content << b }
	alg := der_seq(alg_content)
	mut content := []u8{}
	for b in alg { content << b }
	bs := der_bitstr(raw)
	for b in bs { content << b }
	return der_seq(content)
}

// mldsa_tbs_common assembles the TBS body shared by self-signed and
// CA-signed ML-DSA certificates. A CA:TRUE basicConstraints extension is
// emitted when is_ca is set so openssl verify accepts V-generated CAs.
fn mldsa_tbs_common(serial []u8, sig_alg_id []u8, issuer []u8, validity []u8, subject []u8, spki []u8, dns_names []string, is_ca bool) []u8 {
	mut tbs := []u8{}
	version := der_ctx(0, der_int([u8(0x02)]))
	for b in version { tbs << b }
	tbs << der_int(serial)
	for b in sig_alg_id { tbs << b }
	for b in issuer { tbs << b }
	for b in validity { tbs << b }
	for b in subject { tbs << b }
	for b in spki { tbs << b }
	mut exts_inner := []u8{}
	if is_ca {
		mut bc_content := []u8{}
		for b in encode_oid('2.5.29.19') { bc_content << b }
		for b in der_tlv(0x01, [u8(0xff)]) { bc_content << b }
		for b in der_octstr(der_seq(der_tlv(0x01, [u8(0xff)]))) { bc_content << b }
		bc_ext := der_seq(bc_content)
		for b in bc_ext { exts_inner << b }
	}
	if dns_names.len > 0 {
		mut san_content := []u8{}
		for dn in dns_names {
			gn := der_tlv(0x82, dn.bytes())
			for b in gn { san_content << b }
		}
		san_ext_value := der_seq(san_content)
		mut ext_content := []u8{}
		for b in encode_oid('2.5.29.17') { ext_content << b }
		for b in der_octstr(san_ext_value) { ext_content << b }
		ext := der_seq(ext_content)
		for b in ext { exts_inner << b }
	}
	if exts_inner.len > 0 {
		exts := der_seq(exts_inner)
		ctx3 := der_ctx(3, exts)
		for b in ctx3 { tbs << b }
	}
	return tbs
}

// generate_self_signed_mldsa creates a self-signed ML-DSA certificate.
// The TBS bytes are signed directly (pure ML-DSA, deterministic for stable
// test fixtures); the outer sigAlg OID matches the key kind. The key must
// agree with kind (checked by public-key length).
pub fn generate_self_signed_mldsa(opts CertGenOptions, key mldsa.PrivateKey, kind mldsa.Kind) !(Certificate, []u8) {
	if key.public_key().bytes().len != kind.public_key_size() {
		return error('x509: ML-DSA key/kind mismatch')
	}
	raw := key.public_key().bytes()
	mut serial := rand.bytes(8) or { return err }
	for serial.len > 1 && serial[0] == 0 {
		serial = serial[1..].clone()
	}
	serial[0] = serial[0] & 0x7f

	mut name_content := []u8{}
	if opts.common_name.len > 0 {
		mut cn_attr_content := []u8{}
		for b in encode_oid('2.5.4.3') { cn_attr_content << b }
		for b in der_utf8(opts.common_name) { cn_attr_content << b }
		cn_attr := der_seq(cn_attr_content)
		set_content := der_set(cn_attr)
		for b in set_content { name_content << b }
	}
	name_der := der_seq(name_content)

	mut validity_content := []u8{}
	for b in der_utc(opts.not_before) { validity_content << b }
	for b in der_utc(opts.not_after) { validity_content << b }
	validity := der_seq(validity_content)

	mut sig_alg_content := []u8{}
	for b in encode_oid(mldsa_sigalg_oid(kind)) { sig_alg_content << b }
	sig_alg_id := der_seq(sig_alg_content)

	spki := mldsa_spki(raw, kind)
	tbs := mldsa_tbs_common(serial, sig_alg_id, name_der, validity, name_der, spki, opts.dns_names, opts.is_ca)
	tbs_der := der_seq(tbs)

	sig := key.sign(tbs_der, mldsa.SignerOpts{ deterministic: true }) or { return err }
	bsig := der_bitstr(sig)

	mut cert_content := []u8{}
	for b in tbs_der { cert_content << b }
	for b in sig_alg_id { cert_content << b }
	for b in bsig { cert_content << b }
	cert_der := der_seq(cert_content)

	cert := parse_der(cert_der) or { return err }
	return cert, cert_der
}

// generate_ca_signed_mldsa creates an ML-DSA leaf signed by an ML-DSA CA.
// The leaf and CA keys may be different variants (mirroring ECDSA
// cross-curve support); the sigAlg OID follows the CA key and the SPKI
// follows the leaf key. Each key must agree with its kind.
pub fn generate_ca_signed_mldsa(leaf_opts CertGenOptions, leaf_key mldsa.PrivateKey, leaf_kind mldsa.Kind, ca_cert &Certificate, ca_key mldsa.PrivateKey, ca_kind mldsa.Kind) !(Certificate, []u8) {
	if leaf_key.public_key().bytes().len != leaf_kind.public_key_size() {
		return error('x509: ML-DSA leaf key/kind mismatch')
	}
	if ca_key.public_key().bytes().len != ca_kind.public_key_size() {
		return error('x509: ML-DSA CA key/kind mismatch')
	}
	issuer_der := extract_subject_der(ca_cert.raw) or { return err }
	leaf_raw := leaf_key.public_key().bytes()
	mut serial := rand.bytes(8) or { return err }
	for serial.len > 1 && serial[0] == 0 {
		serial = serial[1..].clone()
	}
	serial[0] = serial[0] & 0x7f

	mut subj_content := []u8{}
	if leaf_opts.common_name.len > 0 {
		mut cn_attr_content := []u8{}
		for b in encode_oid('2.5.4.3') { cn_attr_content << b }
		for b in der_utf8(leaf_opts.common_name) { cn_attr_content << b }
		cn_attr := der_seq(cn_attr_content)
		set_content := der_set(cn_attr)
		for b in set_content { subj_content << b }
	}
	subject_der := der_seq(subj_content)

	mut validity_content := []u8{}
	for b in der_utc(leaf_opts.not_before) { validity_content << b }
	for b in der_utc(leaf_opts.not_after) { validity_content << b }
	validity := der_seq(validity_content)

	mut sig_alg_content := []u8{}
	for b in encode_oid(mldsa_sigalg_oid(ca_kind)) { sig_alg_content << b }
	sig_alg_id := der_seq(sig_alg_content)

	spki := mldsa_spki(leaf_raw, leaf_kind)

	tbs := mldsa_tbs_common(serial, sig_alg_id, issuer_der, validity, subject_der, spki, leaf_opts.dns_names, leaf_opts.is_ca)
	tbs_der := der_seq(tbs)

	sig := ca_key.sign(tbs_der, mldsa.SignerOpts{ deterministic: true }) or { return err }

	mut cert_content := []u8{}
	for b in tbs_der { cert_content << b }
	for b in sig_alg_id { cert_content << b }
	bsig := der_bitstr(sig)
	for b in bsig { cert_content << b }
	cert_der := der_seq(cert_content)

	cert := parse_der(cert_der) or { return err }
	return cert, cert_der
}
