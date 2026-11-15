module tls

import crypto.ecdsa
import crypto.ed25519
import crypto.sha256
import crypto.sha512
import crypto.slh_dsa
import crypto.x509
import math.big
import time
import x.crypto.mldsa

// ecdsa_sigalg_hash maps a certificate sigAlg SEQUENCE value to the hash it
// mandates. Only the primary OID counts; a trailing OID smuggled into the
// parameters position can never select the hash.
// Accepted: ecdsa-with-SHA256/384/512. SHA-1/MD5 and RSA OIDs are rejected.
fn ecdsa_sigalg_hash(sigalg_val []u8) !RsaHash {
	if der_first_oid_is(sigalg_val, [u8(0x2a), 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x02]) {
		return .sha256
	}
	if der_first_oid_is(sigalg_val, [u8(0x2a), 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x03]) {
		return .sha384
	}
	if der_first_oid_is(sigalg_val, [u8(0x2a), 0x86, 0x48, 0xce, 0x3d, 0x04, 0x03, 0x04]) {
		return .sha512
	}
	return error('tls: unsupported certificate signature algorithm')
}

// ecdsa_nid_for_point maps an uncompressed point length to its curve.
fn ecdsa_nid_for_point(point []u8) !ecdsa.Nid {
	return match point.len {
		65 { ecdsa.Nid.prime256v1 }
		97 { ecdsa.Nid.secp384r1 }
		133 { ecdsa.Nid.secp521r1 }
		else { return error('tls: bad EC point length') }
	}
}

// ecdsa_custom_opts builds SignerOpts that hash exactly once with h,
// regardless of the key curve. This gives exact OID semantics for every
// curve/hash combination (cross-combos included).
fn ecdsa_custom_opts(h RsaHash) ecdsa.SignerOpts {
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

// verify_cert_signature_ecdsa verifies a certificate signature against a
// P-256/P-384/P-521 parent key.
// verify_cert_signature_ecdsa supports ecdsa-with-SHA256/384/512; every other
// signature algorithm is rejected. The TBS bytes are hashed exactly once with
// the OID-mandated hash (matching what real CAs sign), for any curve/hash
// combination.
pub fn verify_cert_signature_ecdsa(child_der []u8, parent_point []u8) ! {
	// Certificate ::= SEQUENCE { tbs, sigAlg, sig }
	tag, cert_val, _ := der_tlv(child_der) or { return err }
	if tag != 0x30 {
		return error('tls: expected cert SEQUENCE')
	}
	// tbs occupies cert_val[0..tbs_consumed] (tag+length+value).
	ttag, _, tbs_consumed := der_tlv(cert_val) or { return err }
	if ttag != 0x30 {
		return error('tls: expected tbs SEQUENCE')
	}
	tbs_raw := cert_val[..tbs_consumed].clone()
	_, sigalg_val, sigalg_consumed := der_tlv(cert_val[tbs_consumed..]) or { return err }
	h := ecdsa_sigalg_hash(sigalg_val) or { return err }
	sig_pos := tbs_consumed + sigalg_consumed
	// signature BIT STRING
	stag, sig_val, _ := der_tlv(cert_val[sig_pos..]) or { return err }
	if stag != 0x03 {
		return error('tls: expected BIT STRING signature')
	}
	if sig_val.len < 1 || sig_val[0] != 0 {
		return error('tls: bad sig padding')
	}
	sig := sig_val[1..].clone()

	nid := ecdsa_nid_for_point(parent_point) or { return err }
	parent_pk := ecdsa.PublicKey.from_uncompressed_bytes(parent_point, ecdsa.CurveOptions{
		nid: nid
	}) or { return err }
	ok := parent_pk.verify(tbs_raw, sig, ecdsa_custom_opts(h)) or { return err }
	if !ok {
		return error(error_bad_signature)
	}
}

// verify_cert_signature dispatches chain-signature verification by parent key
// type: ECDSA (hash from the sigAlg OID), RSA PKCS#1 v1.5 (hash from the
// sigAlg OID), ML-DSA or SLH-DSA (pure signature over the TBS bytes, sigAlg
// OID must match the key variant). RSA-PSS-signed CA certificates are
// rejected.
pub fn verify_cert_signature(child_der []u8, parent x509.PublicKey) ! {
	match parent {
		x509.EcdsaP256PublicKey {
			verify_cert_signature_ecdsa(child_der, parent.point) or { return err }
		}
		x509.EcdsaP384PublicKey {
			verify_cert_signature_ecdsa(child_der, parent.point) or { return err }
		}
		x509.EcdsaP521PublicKey {
			verify_cert_signature_ecdsa(child_der, parent.point) or { return err }
		}
		x509.RsaPublicKey {
			verify_cert_signature_rsa(child_der, parent.der) or { return err }
		}
		x509.MlDsa44PublicKey {
			verify_cert_signature_mldsa(child_der, parent.raw, .ml_dsa_44) or { return err }
		}
		x509.MlDsa65PublicKey {
			verify_cert_signature_mldsa(child_der, parent.raw, .ml_dsa_65) or { return err }
		}
		x509.MlDsa87PublicKey {
			verify_cert_signature_mldsa(child_der, parent.raw, .ml_dsa_87) or { return err }
		}
		x509.SlhDsaShake128sPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .shake_128s) or { return err }
		}
		x509.SlhDsaShake128fPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .shake_128f) or { return err }
		}
		x509.SlhDsaShake192sPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .shake_192s) or { return err }
		}
		x509.SlhDsaShake192fPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .shake_192f) or { return err }
		}
		x509.SlhDsaShake256sPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .shake_256s) or { return err }
		}
		x509.SlhDsaShake256fPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .shake_256f) or { return err }
		}
		x509.Ed25519PublicKey {
			verify_cert_signature_ed25519(child_der, parent.raw) or { return err }
		}
		x509.SlhDsaSha2128sPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .sha2_128s) or { return err }
		}
		x509.SlhDsaSha2128fPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .sha2_128f) or { return err }
		}
		x509.SlhDsaSha2192sPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .sha2_192s) or { return err }
		}
		x509.SlhDsaSha2192fPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .sha2_192f) or { return err }
		}
		x509.SlhDsaSha2256sPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .sha2_256s) or { return err }
		}
		x509.SlhDsaSha2256fPublicKey {
			verify_cert_signature_slh_dsa(child_der, parent.raw, .sha2_256f) or { return err }
		}
	}
}

// mldsa_kind_from_sigalg maps a certificate sigAlg SEQUENCE value to its
// ML-DSA variant. The value must be exactly one OID (no parameters):
// id-ml-dsa-44/65/87; everything else is rejected.
fn mldsa_kind_from_sigalg(sigalg_val []u8) !mldsa.Kind {
	if der_is_single_oid(sigalg_val, [u8(0x60), 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, 0x11]) {
		return .ml_dsa_44
	}
	if der_is_single_oid(sigalg_val, [u8(0x60), 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, 0x12]) {
		return .ml_dsa_65
	}
	if der_is_single_oid(sigalg_val, [u8(0x60), 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, 0x13]) {
		return .ml_dsa_87
	}
	return error('tls: unsupported ML-DSA signature algorithm')
}

// verify_cert_signature_mldsa verifies a certificate signature against a
// parent ML-DSA key. ML-DSA signs the raw TBS bytes (no pre-hash); the
// certificate sigAlg OID must match the parent key variant.
pub fn verify_cert_signature_mldsa(child_der []u8, parent_raw []u8, kind mldsa.Kind) ! {
	tag, cert_val, _ := der_tlv(child_der) or { return err }
	if tag != 0x30 {
		return error('tls: expected cert SEQUENCE')
	}
	ttag, _, tbs_consumed := der_tlv(cert_val) or { return err }
	if ttag != 0x30 {
		return error('tls: expected tbs SEQUENCE')
	}
	tbs_raw := cert_val[..tbs_consumed].clone()
	_, sigalg_val, sigalg_consumed := der_tlv(cert_val[tbs_consumed..]) or { return err }
	sig_kind := mldsa_kind_from_sigalg(sigalg_val) or { return err }
	if sig_kind != kind {
		return error('tls: ML-DSA sigAlg/key mismatch')
	}
	sig_pos := tbs_consumed + sigalg_consumed
	stag, sig_val, _ := der_tlv(cert_val[sig_pos..]) or { return err }
	if stag != 0x03 {
		return error('tls: expected BIT STRING signature')
	}
	if sig_val.len < 1 || sig_val[0] != 0 {
		return error('tls: bad sig padding')
	}
	sig := sig_val[1..].clone()

	pk := mldsa.PublicKey.from_bytes(parent_raw, kind) or { return err }
	ok := pk.verify(tbs_raw, sig, mldsa.SignerOpts{}) or { return err }
	if !ok {
		return error(error_bad_signature)
	}
}

// slh_dsa_sigalg_tail maps an SLH-DSA parameter set to the final arc of its
// signature-algorithm OID 2.16.840.1.101.3.4.3.X: arcs 20-25 (0x14-0x19) are
// the SHA2 sets, arcs 26-31 (0x1a-0x1f) the SHAKE sets. Verified against
// `openssl list -signature-algorithms` (OpenSSL 4.1.0).
fn slh_dsa_sigalg_tail(params slh_dsa.ParameterSet) u8 {
	return match params {
		.sha2_128s { u8(0x14) }
		.sha2_128f { u8(0x15) }
		.sha2_192s { u8(0x16) }
		.sha2_192f { u8(0x17) }
		.sha2_256s { u8(0x18) }
		.sha2_256f { u8(0x19) }
		.shake_128s { u8(0x1a) }
		.shake_128f { u8(0x1b) }
		.shake_192s { u8(0x1c) }
		.shake_192f { u8(0x1d) }
		.shake_256s { u8(0x1e) }
		.shake_256f { u8(0x1f) }
	}
}

// slh_dsa_sigalg_content returns the DER content bytes of the
// signature-algorithm OID for an SLH-DSA parameter set.
fn slh_dsa_sigalg_content(params slh_dsa.ParameterSet) []u8 {
	return [u8(0x60), 0x86, 0x48, 0x01, 0x65, 0x03, 0x04, 0x03, slh_dsa_sigalg_tail(params)]
}

// slh_dsa_all_params lists every SLH-DSA parameter set in OID order.
fn slh_dsa_all_params() []slh_dsa.ParameterSet {
	return [
		slh_dsa.ParameterSet.sha2_128s,
		.sha2_128f,
		.sha2_192s,
		.sha2_192f,
		.sha2_256s,
		.sha2_256f,
		.shake_128s,
		.shake_128f,
		.shake_192s,
		.shake_192f,
		.shake_256s,
		.shake_256f,
	]
}

// slh_dsa_params_from_sigalg maps a certificate sigAlg SEQUENCE value to its
// SLH-DSA parameter set. The value must be exactly one OID (no parameters);
// everything else is rejected.
fn slh_dsa_params_from_sigalg(sigalg_val []u8) !slh_dsa.ParameterSet {
	for params in slh_dsa_all_params() {
		if der_is_single_oid(sigalg_val, slh_dsa_sigalg_content(params)) {
			return params
		}
	}
	return error('tls: unsupported SLH-DSA signature algorithm')
}

// verify_cert_signature_slh_dsa verifies a certificate signature against a
// parent SLH-DSA key. SLH-DSA signs the raw TBS bytes (no pre-hash, pure
// variant); the certificate sigAlg OID must match the parent key variant.
pub fn verify_cert_signature_slh_dsa(child_der []u8, parent_raw []u8, params slh_dsa.ParameterSet) ! {
	tag, cert_val, _ := der_tlv(child_der) or { return err }
	if tag != 0x30 {
		return error('tls: expected cert SEQUENCE')
	}
	ttag, _, tbs_consumed := der_tlv(cert_val) or { return err }
	if ttag != 0x30 {
		return error('tls: expected tbs SEQUENCE')
	}
	tbs_raw := cert_val[..tbs_consumed].clone()
	_, sigalg_val, sigalg_consumed := der_tlv(cert_val[tbs_consumed..]) or { return err }
	sig_params := slh_dsa_params_from_sigalg(sigalg_val) or { return err }
	if sig_params != params {
		return error('tls: SLH-DSA sigAlg/key mismatch')
	}
	sig_pos := tbs_consumed + sigalg_consumed
	stag, sig_val, _ := der_tlv(cert_val[sig_pos..]) or { return err }
	if stag != 0x03 {
		return error('tls: expected BIT STRING signature')
	}
	if sig_val.len < 1 || sig_val[0] != 0 {
		return error('tls: bad sig padding')
	}
	sig := sig_val[1..].clone()

	pk := slh_dsa.public_key_from_bytes(params, parent_raw) or { return err }
	if !slh_dsa.verify(tbs_raw, sig, [], pk) {
		return error(error_bad_signature)
	}
}

// ed25519_sigalg_content is the DER content bytes of id-Ed25519
// (1.3.101.112, RFC 8410): first byte 1*40+3, then 101, 112.
fn ed25519_sigalg_content() []u8 {
	return [u8(0x2b), 101, 112]
}

// verify_cert_signature_ed25519 verifies a certificate signature against
// a parent Ed25519 key. Ed25519 signs the raw TBS bytes (no pre-hash);
// the certificate sigAlg OID must be exactly id-Ed25519 (no parameters).
pub fn verify_cert_signature_ed25519(child_der []u8, parent_raw []u8) ! {
	if parent_raw.len != 32 {
		return error('tls: bad Ed25519 public key length')
	}
	tag, cert_val, _ := der_tlv(child_der) or { return err }
	if tag != 0x30 {
		return error('tls: expected cert SEQUENCE')
	}
	ttag, _, tbs_consumed := der_tlv(cert_val) or { return err }
	if ttag != 0x30 {
		return error('tls: expected tbs SEQUENCE')
	}
	tbs_raw := cert_val[..tbs_consumed].clone()
	_, sigalg_val, sigalg_consumed := der_tlv(cert_val[tbs_consumed..]) or { return err }
	if !der_is_single_oid(sigalg_val, ed25519_sigalg_content()) {
		return error('tls: Ed25519 sigAlg/key mismatch')
	}
	sig_pos := tbs_consumed + sigalg_consumed
	stag, sig_val, _ := der_tlv(cert_val[sig_pos..]) or { return err }
	if stag != 0x03 {
		return error('tls: expected BIT STRING signature')
	}
	if sig_val.len < 1 || sig_val[0] != 0 {
		return error('tls: bad sig padding')
	}
	sig := sig_val[1..].clone()

	ok := ed25519.verify(parent_raw, tbs_raw, sig) or { return err }
	if !ok {
		return error(error_bad_signature)
	}
}

// cv_content builds the RFC 8446 §4.4.3 signing input: 64×0x20, the context
// string for the sender role, a 0x00 separator, and the transcript hash.
pub fn cv_content(is_server bool, transcript_hash []u8) []u8 {
	mut content := []u8{}
	for _ in 0 .. 64 {
		content << u8(0x20)
	}
	if is_server {
		content << 'TLS 1.3, server CertificateVerify'.bytes()
	} else {
		content << 'TLS 1.3, client CertificateVerify'.bytes()
	}
	content << u8(0)
	content << transcript_hash
	return content
}

// verify_certificate_verify verifies a CertificateVerify message body against
// the peer leaf public key, dispatching on the negotiated scheme.
// verify_certificate_verify enforces scheme/key-type agreement: ECDSA schemes
// need the matching curve, Ed25519 needs scheme 0x0807, RSA schemes need an
// RSA key, ML-DSA schemes need the matching ML-DSA variant, SLH-DSA schemes
// need the matching SLH-DSA SHAKE parameter set.
pub fn verify_certificate_verify(cv_body []u8, pk x509.PublicKey, content []u8) ! {
	scheme, sig := parse_cert_verify_sig(cv_body) or { return err }
	match scheme {
		u16(0x0403) {
			match pk {
				x509.EcdsaP256PublicKey {
					epk := ecdsa.PublicKey.from_uncompressed_bytes(pk.point, ecdsa.CurveOptions{}) or {
						return err
					}
					ok := epk.verify(content, sig, ecdsa.SignerOpts{}) or { return err }
					if !ok {
						return error('tls: certificate verify failed')
					}
				}
				else {
					return error('tls: scheme/key mismatch in CertificateVerify')
				}
			}
		}
		u16(0x0503) {
			match pk {
				x509.EcdsaP384PublicKey {
					epk := ecdsa.PublicKey.from_uncompressed_bytes(pk.point, ecdsa.CurveOptions{
						nid: .secp384r1
					}) or { return err }
					ok := epk.verify(content, sig, ecdsa.SignerOpts{}) or { return err }
					if !ok {
						return error('tls: certificate verify failed')
					}
				}
				else {
					return error('tls: scheme/key mismatch in CertificateVerify')
				}
			}
		}
		u16(0x0603) {
			match pk {
				x509.EcdsaP521PublicKey {
					epk := ecdsa.PublicKey.from_uncompressed_bytes(pk.point, ecdsa.CurveOptions{
						nid: .secp521r1
					}) or { return err }
					ok := epk.verify(content, sig, ecdsa.SignerOpts{}) or { return err }
					if !ok {
						return error('tls: certificate verify failed')
					}
				}
				else {
					return error('tls: scheme/key mismatch in CertificateVerify')
				}
			}
		}
		u16(0x0401), u16(0x0501), u16(0x0601) {
			h := match scheme {
				u16(0x0401) { RsaHash.sha256 }
				u16(0x0501) { RsaHash.sha384 }
				else { RsaHash.sha512 }
			}
			match pk {
				x509.RsaPublicKey {
					n, e := parse_rsa_pubkey(pk.der) or { return err }
					digest := rsa_hash_bytes(h, content)
					rsa_pkcs1_verify_digest(digest, sig, n, e, h) or { return err }
				}
				else {
					return error('tls: scheme/key mismatch in CertificateVerify')
				}
			}
		}
		u16(0x0804), u16(0x0805), u16(0x0806) {
			match pk {
				x509.RsaPublicKey {
					n, e := parse_rsa_pubkey(pk.der) or { return err }
					rsa_pss_verify(scheme, content, sig, e, n) or { return err }
				}
				else {
					return error('tls: scheme/key mismatch in CertificateVerify')
				}
			}
		}
		u16(0x0904), u16(0x0905), u16(0x0906) {
			kind := match scheme {
				u16(0x0904) { mldsa.Kind.ml_dsa_44 }
				u16(0x0905) { mldsa.Kind.ml_dsa_65 }
				else { mldsa.Kind.ml_dsa_87 }
			}
			raw := match pk {
				x509.MlDsa44PublicKey {
					if kind != .ml_dsa_44 {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.MlDsa65PublicKey {
					if kind != .ml_dsa_65 {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.MlDsa87PublicKey {
					if kind != .ml_dsa_87 {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				else {
					return error('tls: scheme/key mismatch in CertificateVerify')
				}
			}
			mpk := mldsa.PublicKey.from_bytes(raw, kind) or { return err }
			ok := mpk.verify(content, sig, mldsa.SignerOpts{}) or { return err }
			if !ok {
				return error('tls: certificate verify failed')
			}
		}
		// SLH-DSA SHA2 schemes 0x0911-0x0916, assigned in OID order
		// (SHA2-128s first). Measured from the ClientHello
		// signature_algorithms extension emitted by
		// `openssl s_client -sigalgs slhdsa_sha2_128s/...` (OpenSSL 4.1.0);
		// codepoint order matches the x509 OID order (.20-.25).
		u16(0x0911), u16(0x0912), u16(0x0913), u16(0x0914), u16(0x0915), u16(0x0916) {
			params := match scheme {
				u16(0x0911) { slh_dsa.ParameterSet.sha2_128s }
				u16(0x0912) { slh_dsa.ParameterSet.sha2_128f }
				u16(0x0913) { slh_dsa.ParameterSet.sha2_192s }
				u16(0x0914) { slh_dsa.ParameterSet.sha2_192f }
				u16(0x0915) { slh_dsa.ParameterSet.sha2_256s }
				else { slh_dsa.ParameterSet.sha2_256f }
			}
			raw := match pk {
				x509.SlhDsaSha2128sPublicKey {
					if params != .sha2_128s {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.SlhDsaSha2128fPublicKey {
					if params != .sha2_128f {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.SlhDsaSha2192sPublicKey {
					if params != .sha2_192s {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.SlhDsaSha2192fPublicKey {
					if params != .sha2_192f {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.SlhDsaSha2256sPublicKey {
					if params != .sha2_256s {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.SlhDsaSha2256fPublicKey {
					if params != .sha2_256f {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				else {
					return error('tls: scheme/key mismatch in CertificateVerify')
				}
			}
			spk := slh_dsa.public_key_from_bytes(params, raw) or { return err }
			if !slh_dsa.verify(content, sig, [], spk) {
				return error('tls: certificate verify failed')
			}
		}
		// SLH-DSA SHAKE schemes 0x0917-0x091c, assigned in OID order
		// (SHAKE-128s first). Measured from the ClientHello
		// signature_algorithms extension emitted by
		// `openssl s_client -sigalgs slhdsa_shake_128s/...` (OpenSSL 4.1.0);
		// the ml_dsa-style control (mldsa44 -> 0x0904) matches this repo.
		u16(0x0917), u16(0x0918), u16(0x0919), u16(0x091a), u16(0x091b), u16(0x091c) {
			params := match scheme {
				u16(0x0917) { slh_dsa.ParameterSet.shake_128s }
				u16(0x0918) { slh_dsa.ParameterSet.shake_128f }
				u16(0x0919) { slh_dsa.ParameterSet.shake_192s }
				u16(0x091a) { slh_dsa.ParameterSet.shake_192f }
				u16(0x091b) { slh_dsa.ParameterSet.shake_256s }
				else { slh_dsa.ParameterSet.shake_256f }
			}
			raw := match pk {
				x509.SlhDsaShake128sPublicKey {
					if params != .shake_128s {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.SlhDsaShake128fPublicKey {
					if params != .shake_128f {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.SlhDsaShake192sPublicKey {
					if params != .shake_192s {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.SlhDsaShake192fPublicKey {
					if params != .shake_192f {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.SlhDsaShake256sPublicKey {
					if params != .shake_256s {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				x509.SlhDsaShake256fPublicKey {
					if params != .shake_256f {
						return error('tls: scheme/key mismatch in CertificateVerify')
					}
					pk.raw
				}
				else {
					return error('tls: scheme/key mismatch in CertificateVerify')
				}
			}
			spk := slh_dsa.public_key_from_bytes(params, raw) or { return err }
			if !slh_dsa.verify(content, sig, [], spk) {
				return error('tls: certificate verify failed')
			}
		}
		// Ed25519 (RFC 8446 §4.2.3): scheme 0x0807 signs the content
		// directly with the raw 32-byte key.
		u16(0x0807) {
			match pk {
				x509.Ed25519PublicKey {
					ok := ed25519.verify(pk.raw, content, sig) or { return err }
					if !ok {
						return error('tls: certificate verify failed')
					}
				}
				else {
					return error('tls: scheme/key mismatch in CertificateVerify')
				}
			}
		}
		else {
			return error('tls: unsupported signature scheme')
		}
	}
}

// verify_peer_chain builds and validates a peer chain from a Certificate
// flight: leaf plus same-flight intermediates against store roots (or
// system roots when the store is empty), via x509.build_chain with the
// direct-root fast paths. EKU follows usage (serverAuth + hostname for
// servers, clientAuth without hostname for clients), system CRLs apply,
// and an optional OCSP staple is checked. It returns the leaf public key
// for CertificateVerify. Error mapping preserves the legacy error_chain_*
// contract (validity floor, hostname mismatch, untrusted chain).
pub fn verify_peer_chain(cert_ders [][]u8, roots []x509.Certificate, usage x509.VerifyUsage, hostname string, ocsp_resp []u8) !x509.PublicKey {
	if cert_ders.len == 0 {
		return error('tls: empty certificate chain')
	}
	leaf := x509.parse_der(cert_ders[0]) or { return error(error_bad_signature) }
	mut intermediates := []x509.Certificate{}
	for d in cert_ders[1..] {
		intermediates << x509.parse_der(d) or { return error(error_bad_signature) }
	}
	actual := if roots.len > 0 {
		roots
	} else {
		(load_system_roots() or { return error(error_chain_untrusted) }).roots
	}
	chain := x509.build_chain(leaf, intermediates, actual) or { return error(error_chain_untrusted) }
	x509.verify_chain(chain, x509.VerifyOptions{
		usage:     usage
		hostname:  hostname
		crls:      x509.load_system_crls()
		ocsp_resp: ocsp_resp
	}) or {
		msg := err.msg()
		if msg.contains('host') {
			return error(error_hostname_mismatch)
		}
		if msg.contains('not valid') {
			if time.now().unix() > leaf.not_after.unix() {
				return error(error_chain_expired)
			}
			return error(error_chain_untrusted)
		}
		return error(error_chain_untrusted)
	}
	return chain[0].public_key() or { return error(error_bad_signature) }
}

// rsa_pub_numbers parses an RSA public key into numbers for verification.
pub fn rsa_pub_numbers(pk x509.RsaPublicKey) !(big.Integer, u64) {
	return parse_rsa_pubkey(pk.der)
}
