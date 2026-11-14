module tls

import crypto.ecdsa
import crypto.sha256
import crypto.sha512
import crypto.x509
import math.big
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
// sigAlg OID), or ML-DSA (pure signature over the TBS bytes, sigAlg OID must
// match the key variant). RSA-PSS-signed CA certificates are rejected.
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
// need the matching curve, RSA schemes need an RSA key, ML-DSA schemes need
// the matching ML-DSA variant.
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
		else {
			return error('tls: unsupported signature scheme')
		}
	}
}

// rsa_pub_numbers parses an RSA public key into numbers for verification.
pub fn rsa_pub_numbers(pk x509.RsaPublicKey) !(big.Integer, u64) {
	return parse_rsa_pubkey(pk.der)
}
