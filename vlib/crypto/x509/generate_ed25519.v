module x509

import crypto.ed25519
import crypto.rand

// ed25519_spki builds a SubjectPublicKeyInfo for a raw Ed25519 public key
// (RFC 8410): SEQUENCE { SEQUENCE { OID id-Ed25519 }, BIT STRING raw }
// with absent parameters.
fn ed25519_spki(raw []u8) []u8 {
	mut alg_content := []u8{}
	for b in encode_oid(oid_ed25519) {
		alg_content << b
	}
	alg := der_seq(alg_content)
	mut content := []u8{}
	for b in alg {
		content << b
	}
	bs := der_bitstr(raw)
	for b in bs {
		content << b
	}
	return der_seq(content)
}

// generate_self_signed_ed25519 creates a self-signed Ed25519 certificate.
// The TBS bytes are signed directly (pure Ed25519, deterministic for
// stable test fixtures); the outer sigAlg OID is id-Ed25519. The key
// must be 64 bytes (seed ++ public key).
pub fn generate_self_signed_ed25519(opts CertGenOptions, key ed25519.PrivateKey) !(Certificate, []u8) {
	check_ip_names(opts.ip_names)!
	if key.len != ed25519.private_key_size {
		return error('x509: bad Ed25519 private key length')
	}
	raw := key.public_key()
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
	for b in encode_oid(oid_ed25519) { sig_alg_content << b }
	sig_alg_id := der_seq(sig_alg_content)

	spki := ed25519_spki(raw)
	ski := ski_for_raw(raw)
	tbs := mldsa_tbs_common(serial, sig_alg_id, name_der, validity, name_der, spki,
		opts, ski, ski, policy_exts(opts))
	tbs_der := der_seq(tbs)

	sig := ed25519.sign(key, tbs_der) or { return err }
	bsig := der_bitstr(sig)

	mut cert_content := []u8{}
	for b in tbs_der { cert_content << b }
	for b in sig_alg_id { cert_content << b }
	for b in bsig { cert_content << b }
	cert_der := der_seq(cert_content)

	cert := parse_der(cert_der) or { return err }
	return cert, cert_der
}

// generate_ca_signed_ed25519 creates an Ed25519 leaf signed by an
// Ed25519 CA. The sigAlg OID is id-Ed25519 and the SPKI carries the
// leaf key; both keys must be 64 bytes.
pub fn generate_ca_signed_ed25519(leaf_opts CertGenOptions, leaf_key ed25519.PrivateKey, ca_cert &Certificate, ca_key ed25519.PrivateKey) !(Certificate, []u8) {
	check_ip_names(leaf_opts.ip_names)!
	if leaf_key.len != ed25519.private_key_size {
		return error('x509: bad Ed25519 leaf private key length')
	}
	if ca_key.len != ed25519.private_key_size {
		return error('x509: bad Ed25519 CA private key length')
	}
	issuer_der := extract_subject_der(ca_cert.raw) or { return err }
	leaf_raw := leaf_key.public_key()
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
	for b in encode_oid(oid_ed25519) { sig_alg_content << b }
	sig_alg_id := der_seq(sig_alg_content)

	spki := ed25519_spki(leaf_raw)

	ski := ski_for_raw(leaf_raw)
	ca_ski := ca_ski_of(ca_cert)!
	tbs := mldsa_tbs_common(serial, sig_alg_id, issuer_der, validity, subject_der,
		spki, leaf_opts, ski, ca_ski, policy_exts(leaf_opts))
	tbs_der := der_seq(tbs)

	sig := ed25519.sign(ca_key, tbs_der) or { return err }

	mut cert_content := []u8{}
	for b in tbs_der { cert_content << b }
	for b in sig_alg_id { cert_content << b }
	bsig := der_bitstr(sig)
	for b in bsig { cert_content << b }
	cert_der := der_seq(cert_content)

	cert := parse_der(cert_der) or { return err }
	return cert, cert_der
}
