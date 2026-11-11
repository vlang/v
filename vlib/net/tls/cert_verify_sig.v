module tls

import crypto.ecdsa
import crypto.sha256

pub fn verify_cert_signature_ecdsa(child_der []u8, parent_point []u8) ! {
	// Certificate ::= SEQUENCE { tbs, sigAlg, sig }
	tag, cert_val, _ := der_tlv(child_der) or { return err }
	if tag != 0x30 { return error('tls: expected cert SEQUENCE') }
	// tbs
	ttag, _, tbs_consumed := der_tlv(cert_val) or { return err }
	if ttag != 0x30 { return error('tls: expected tbs SEQUENCE') }
	tbs_bytes := child_der[4..4 + tbs_consumed - 4] // approximate; better slice from content
	_ = tbs_bytes
	// Re-slice properly: tbs occupies cert_val[0..tbs_consumed]
	tbs_raw := cert_val[..tbs_consumed].clone()
	// sigAlg (skip)
	_, _, sigalg_consumed := der_tlv(cert_val[tbs_consumed..]) or { return err }
	sig_pos := tbs_consumed + sigalg_consumed
	// signature BIT STRING
	stag, sig_val, _ := der_tlv(cert_val[sig_pos..]) or { return err }
	if stag != 0x03 { return error('tls: expected BIT STRING signature') }
	if sig_val.len < 1 || sig_val[0] != 0 { return error('tls: bad sig padding') }
	sig := sig_val[1..].clone()

	parent_pk := ecdsa.PublicKey.from_uncompressed_bytes(parent_point, ecdsa.CurveOptions{}) or { return err }
	hash := sha256.sum256(tbs_raw)
	ok := parent_pk.verify(hash, sig, ecdsa.SignerOpts{}) or { return err }
	if !ok { return error(error_bad_signature) }
}
