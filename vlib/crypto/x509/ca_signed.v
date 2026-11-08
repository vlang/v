module x509

import crypto.ecdsa
import crypto.rand
import crypto.sha256
import time

fn extract_subject_der(cert_der []u8) ![]u8 {
	cert_tlv, _ := split_tlv(cert_der, 0) or { return err }
	hl, _ := der_length(cert_der, 1)!
	tbs_full := cert_tlv[1 + hl..]
	tbs_content := expect_tag(tbs_full, 0, 0x30, 'tbsCertificate')!
	mut pos := 0
	if pos < tbs_content.len && tbs_content[pos] == 0xa0 {
		pos = skip_tlv(tbs_content, pos)!
	}
	for _ in 0 .. 3 {
		pos = skip_tlv(tbs_content, pos)!
	}
	pos = skip_tlv(tbs_content, pos)!
	subject_tlv, _ := split_tlv(tbs_content, pos)!
	return subject_tlv.clone()
}

pub fn generate_ca_signed(leaf_opts CertGenOptions, leaf_key ecdsa.PrivateKey, ca_cert &Certificate, ca_key ecdsa.PrivateKey) !(Certificate, []u8) {
	issuer_der := extract_subject_der(ca_cert.raw) or { return err }
	pub_bytes := ecdsa_pub_uncompressed(leaf_key) or { return err }
	mut serial := rand.bytes(8) or { return err }
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
	for b in encode_oid('1.2.840.10045.4.3.2') { sig_alg_content << b }
	for b in der_null() { sig_alg_content << b }
	sig_alg_id := der_seq(sig_alg_content)

	mut spki_alg_c := []u8{}
	for b in encode_oid(oid_ec_public_key) { spki_alg_c << b }
	for b in encode_oid(oid_prime256v1) { spki_alg_c << b }
	spki_alg := der_seq(spki_alg_c)
	mut spki_content := []u8{}
	for b in spki_alg { spki_content << b }
	bs := der_bitstr(pub_bytes)
	for b in bs { spki_content << b }
	spki := der_seq(spki_content)

	version := der_ctx(0, der_int([u8(0x02)]))

	mut tbs := []u8{}
	for b in version { tbs << b }
	tbs << der_int(serial)
	for b in sig_alg_id { tbs << b }
	for b in issuer_der { tbs << b }
	for b in validity { tbs << b }
	for b in subject_der { tbs << b }
	for b in spki { tbs << b }

	if leaf_opts.dns_names.len > 0 {
		mut san_content := []u8{}
		for dn in leaf_opts.dns_names {
			gn := der_tlv(0x82, dn.bytes())
			for b in gn { san_content << b }
		}
		san_ext_value := der_seq(san_content)
		mut ext_content := []u8{}
		for b in encode_oid('2.5.29.17') { ext_content << b }
		for b in der_octstr(san_ext_value) { ext_content << b }
		ext := der_seq(ext_content)
		exts := der_seq(ext)
		ctx3 := der_ctx(3, exts)
		for b in ctx3 { tbs << b }
	}

	tbs_der := der_seq(tbs)

	hash := sha256.sum256(tbs_der)
	sig := ca_key.sign(hash, ecdsa.SignerOpts{}) or { return err }

	mut cert_content := []u8{}
	for b in tbs_der { cert_content << b }
	for b in sig_alg_id { cert_content << b }
	bsig := der_bitstr(sig)
	for b in bsig { cert_content << b }
	cert_der := der_seq(cert_content)

	cert := parse_der(cert_der) or { return err }
	return cert, cert_der
}
