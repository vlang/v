module x509

import crypto.rand
import crypto.slh_dsa

// slh_dsa_sigalg_oid maps an SLH-DSA parameter set to its
// signature-algorithm OID (which doubles as the SPKI algorithm OID,
// like ML-DSA).
fn slh_dsa_sigalg_oid(params slh_dsa.ParameterSet) string {
	return match params {
		.shake_128s { oid_slh_dsa_shake_128s }
		.shake_128f { oid_slh_dsa_shake_128f }
		.shake_192s { oid_slh_dsa_shake_192s }
		.shake_192f { oid_slh_dsa_shake_192f }
		.shake_256s { oid_slh_dsa_shake_256s }
		.shake_256f { oid_slh_dsa_shake_256f }
		.sha2_128s { oid_slh_dsa_sha2_128s }
		.sha2_128f { oid_slh_dsa_sha2_128f }
		.sha2_192s { oid_slh_dsa_sha2_192s }
		.sha2_192f { oid_slh_dsa_sha2_192f }
		.sha2_256s { oid_slh_dsa_sha2_256s }
		.sha2_256f { oid_slh_dsa_sha2_256f }
	}
}

// slh_dsa_spki builds a SubjectPublicKeyInfo for a raw SLH-DSA public key:
// SEQUENCE { SEQUENCE { OID }, BIT STRING raw } with absent parameters.
fn slh_dsa_spki(raw []u8, params slh_dsa.ParameterSet) []u8 {
	mut alg_content := []u8{}
	for b in encode_oid(slh_dsa_sigalg_oid(params)) { alg_content << b }
	alg := der_seq(alg_content)
	mut content := []u8{}
	for b in alg { content << b }
	bs := der_bitstr(raw)
	for b in bs { content << b }
	return der_seq(content)
}

// slh_dsa_check_wired validates the parameter set against the pure-V
// primitive (all 12 SLH-DSA sets are instantiated; kept as a call-site
// gate for future instantiations).
fn slh_dsa_check_wired(params slh_dsa.ParameterSet) ! {
	_ = slh_dsa.get_params(params)
}

// generate_self_signed_slh_dsa creates a self-signed SLH-DSA certificate.
// The TBS bytes are signed directly (deterministic pure SLH-DSA signing for
// stable test fixtures); the outer sigAlg OID matches the parameter set.
// The key must agree with params (checked by parameter-set equality).
pub fn generate_self_signed_slh_dsa(opts CertGenOptions, key slh_dsa.PrivateKey, params slh_dsa.ParameterSet) !(Certificate, []u8) {
	check_ip_names(opts.ip_names)!
	if key.params != params {
		return error('x509: SLH-DSA key/params mismatch')
	}
	slh_dsa_check_wired(params)!
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
	for b in encode_oid(slh_dsa_sigalg_oid(params)) { sig_alg_content << b }
	sig_alg_id := der_seq(sig_alg_content)

	spki := slh_dsa_spki(raw, params)
	// Same TBS shape as ML-DSA (see mldsa_tbs_common).
	ski := ski_for_raw(raw)
	tbs := mldsa_tbs_common(serial, sig_alg_id, name_der, validity, name_der, spki,
		opts, ski, ski, policy_exts(opts))
	tbs_der := der_seq(tbs)

	sig := slh_dsa.sign_deterministic(tbs_der, [], key) or { return err }
	bsig := der_bitstr(sig)

	mut cert_content := []u8{}
	for b in tbs_der { cert_content << b }
	for b in sig_alg_id { cert_content << b }
	for b in bsig { cert_content << b }
	cert_der := der_seq(cert_content)

	cert := parse_der(cert_der) or { return err }
	return cert, cert_der
}

// generate_ca_signed_slh_dsa creates an SLH-DSA leaf signed by an SLH-DSA CA.
// The leaf and CA keys may be different parameter sets (mirroring the ML-DSA
// cross-variant support); the sigAlg OID follows the CA parameter set and
// the SPKI follows the leaf parameter set. Each key must agree with its
// parameter set.
pub fn generate_ca_signed_slh_dsa(leaf_opts CertGenOptions, leaf_key slh_dsa.PrivateKey, leaf_params slh_dsa.ParameterSet, ca_cert &Certificate, ca_key slh_dsa.PrivateKey, ca_params slh_dsa.ParameterSet) !(Certificate, []u8) {
	check_ip_names(leaf_opts.ip_names)!
	if leaf_key.params != leaf_params {
		return error('x509: SLH-DSA leaf key/params mismatch')
	}
	if ca_key.params != ca_params {
		return error('x509: SLH-DSA CA key/params mismatch')
	}
	slh_dsa_check_wired(leaf_params)!
	slh_dsa_check_wired(ca_params)!
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
	for b in encode_oid(slh_dsa_sigalg_oid(ca_params)) { sig_alg_content << b }
	sig_alg_id := der_seq(sig_alg_content)

	spki := slh_dsa_spki(leaf_raw, leaf_params)

	// Same TBS shape as ML-DSA (see mldsa_tbs_common).
	ski := ski_for_raw(leaf_raw)
	ca_ski := ca_ski_of(ca_cert)!
	tbs := mldsa_tbs_common(serial, sig_alg_id, issuer_der, validity, subject_der,
		spki, leaf_opts, ski, ca_ski, policy_exts(leaf_opts))
	tbs_der := der_seq(tbs)

	sig := slh_dsa.sign_deterministic(tbs_der, [], ca_key) or { return err }

	mut cert_content := []u8{}
	for b in tbs_der { cert_content << b }
	for b in sig_alg_id { cert_content << b }
	bsig := der_bitstr(sig)
	for b in bsig { cert_content << b }
	cert_der := der_seq(cert_content)

	cert := parse_der(cert_der) or { return err }
	return cert, cert_der
}
