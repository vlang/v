module tls

import crypto.ecdsa
import crypto.x509

// Minimal DER TLV reader. Returns (tag, value slice, bytes consumed).
fn der_tlv(data []u8) !(u8, []u8, int) {
	if data.len < 2 {
		return error('der: truncated')
	}
	tag := data[0]
	mut off := 1
	// length
	mut length := 0
	if data[off] & 0x80 == 0 {
		length = int(data[off])
		off++
	} else {
		nb := int(data[off] & 0x7f)
		off++
		if nb == 0 || nb > 4 || off + nb > data.len {
			return error('der: bad length')
		}
		for i in 0 .. nb {
			length = length * 256 + int(data[off + i])
		}
		off += nb
	}
	if off + length > data.len {
		return error('der: truncated value')
	}
	val := data[off..off + length].clone()
	return tag, val, off + length
}

// der_first_oid_is reports whether the first child of der (usually a
// signatureAlgorithm or AlgorithmIdentifier SEQUENCE value) is an OID whose
// content bytes equal want. Unlike der_contains_oid, a trailing OID smuggled
// into the parameters position cannot match: only the primary algorithm
// identifier counts.
fn der_first_oid_is(der []u8, want []u8) bool {
	if der.len < 2 || der[0] != 0x06 {
		return false
	}
	mut j := 1
	mut content_len := 0
	if der[j] & 0x80 == 0 {
		content_len = int(der[j])
		j++
	} else {
		nb := int(der[j] & 0x7f)
		j++
		if nb <= 0 || nb > 4 || j + nb > der.len {
			return false
		}
		for k in 0 .. nb {
			content_len = content_len * 256 + int(der[j + k])
		}
		j += nb
	}
	if j + content_len > der.len || content_len != want.len {
		return false
	}
	for k in 0 .. want.len {
		if der[j + k] != want[k] {
			return false
		}
	}
	return true
}

// der_is_single_oid reports whether der is exactly one OID TLV with the
// wanted content bytes and nothing else (no parameters allowed).
fn der_is_single_oid(der []u8, want []u8) bool {
	tag, val, consumed := der_tlv(der) or { return false }
	if tag != 0x06 || consumed != der.len || val.len != want.len {
		return false
	}
	for i in 0 .. want.len {
		if val[i] != want[i] {
			return false
		}
	}
	return true
}

// der_contains_oid reports whether der (usually a sigAlg SEQUENCE value) holds an
// OID whose content bytes equal want. It scans every TLV, so it must not be
// used where only the primary algorithm identifier counts; use
// der_first_oid_is there instead.
fn der_contains_oid(der []u8, want []u8) bool {
	mut i := 0
	for i < der.len {
		if der[i] == 0x06 && i + 1 < der.len {
			mut j := i + 1
			mut content_len := 0
			if der[j] & 0x80 == 0 {
				content_len = int(der[j])
				j++
			} else {
				nb := int(der[j] & 0x7f)
				j++
				if nb <= 0 || nb > 4 || j + nb > der.len {
					i++
					continue
				}
				for k in 0 .. nb {
					content_len = content_len * 256 + int(der[j + k])
				}
				j += nb
			}
			if j + content_len <= der.len && content_len == want.len {
				mut ok := true
				for k in 0 .. want.len {
					if der[j + k] != want[k] {
						ok = false
						break
					}
				}
				if ok {
					return true
				}
			}
		}
		i++
	}
	return false
}

// extract_cert_der extracts the first certificate DER from a Certificate message body.
// extract_cert_der only returns the leaf; use extract_chain_ders when the
// server sends intermediates (the x509 chain builder verifies them).
pub fn extract_cert_der(cert_msg_body []u8) ![]u8 {
	if cert_msg_body.len < 4 {
		return error('tls: truncated Certificate')
	}
	mut off := 0
	ctx_len := int(cert_msg_body[off])
	off += 1 + ctx_len
	if off + 3 > cert_msg_body.len {
		return error('tls: truncated cert list length')
	}
	list_len := (int(cert_msg_body[off]) << 16) | (int(cert_msg_body[off + 1]) << 8) | int(cert_msg_body[off + 2])
	_ = list_len
	off += 3
	if off + 3 > cert_msg_body.len {
		return error('tls: truncated cert entry')
	}
	cert_len := (int(cert_msg_body[off]) << 16) | (int(cert_msg_body[off + 1]) << 8) | int(cert_msg_body[off + 2])
	off += 3
	if off + cert_len > cert_msg_body.len {
		return error('tls: truncated cert')
	}
	return cert_msg_body[off..off + cert_len].clone()
}

// parse_cert_pubkey parses the leaf certificate public key from a Certificate body.
// parse_cert_pubkey supports RSA, P-256/P-384/P-521 ECDSA, Ed25519, ML-DSA-44/65/87
// and SLH-DSA (all 12 parameter sets) keys.
pub fn parse_cert_pubkey(cert_msg_body []u8) !x509.PublicKey {
	if cert_msg_body.len < 4 {
		return error('tls: truncated Certificate')
	}
	mut off := 0
	// certificate_request_context
	ctx_len := int(cert_msg_body[off])
	off += 1 + ctx_len
	if off + 3 > cert_msg_body.len {
		return error('tls: truncated cert list length')
	}
	list_len := (int(cert_msg_body[off]) << 16) | (int(cert_msg_body[off + 1]) << 8) | int(cert_msg_body[off + 2])
	off += 3
	if list_len == 0 {
		return error('tls: empty certificate list')
	}
	if off + 3 > cert_msg_body.len {
		return error('tls: truncated cert entry')
	}
	cert_len := (int(cert_msg_body[off]) << 16) | (int(cert_msg_body[off + 1]) << 8) | int(cert_msg_body[off + 2])
	off += 3
	if off + cert_len > cert_msg_body.len {
		return error('tls: truncated cert')
	}
	cert_der := cert_msg_body[off..off + cert_len]
	cert := x509.parse_der(cert_der) or { return err }
	return cert.public_key()
}

// parse_cert_verify_sig extracts the scheme and signature from a TLS 1.3
// CertificateVerify body.
// parse_cert_verify_sig accepts the schemes net.tls implements
// (ecdsa 0x0403/0x0503/0x0603, eddsa 0x0807, mldsa 0x0904/0x0905/0x0906,
// slh-dsa-sha2 0x0911/0x0912/0x0913/0x0914/0x0915/0x0916,
// slh-dsa-shake 0x0917/0x0918/0x0919/0x091a/0x091b/0x091c, rsa_pss_rsae
// 0x0804/0x0805/0x0806, rsa_pkcs1 0x0401/0x0501/0x0601) and rejects everything
// else, including rsa_pss_pss variants.
pub fn parse_cert_verify_sig(cv_body []u8) !(u16, []u8) {
	if cv_body.len < 4 {
		return error('tls: truncated CertificateVerify')
	}
	// algorithm (2) + sig length (2) + sig
	scheme := (u16(cv_body[0]) << 8) | u16(cv_body[1])
	match scheme {
		u16(0x0403), u16(0x0503), u16(0x0603), u16(0x0807), u16(0x0904), u16(0x0905), u16(0x0906),
		u16(0x0911), u16(0x0912), u16(0x0913), u16(0x0914), u16(0x0915), u16(0x0916),
		u16(0x0917), u16(0x0918), u16(0x0919), u16(0x091a), u16(0x091b), u16(0x091c),
		u16(0x0804), u16(0x0805), u16(0x0806), u16(0x0401), u16(0x0501), u16(0x0601) {
		}
		else {
			return error('tls: unsupported signature scheme')
		}
	}
	sig_len := (int(cv_body[2]) << 8) | int(cv_body[3])
	if 4 + sig_len != cv_body.len {
		return error('tls: bad CertificateVerify signature length')
	}
	return scheme, cv_body[4..4 + sig_len].clone()
}

// extract_chain_ders extracts every certificate DER (leaf first) from a
// Certificate message body, so servers sending leaf+intermediates verify
// through x509.build_chain. The client and server Certificate flights
// share entry framing, so this reuses the client-chain parser.
pub fn extract_chain_ders(cert_msg_body []u8) ![][]u8 {
	chain, _ := parse_client_cert_chain(cert_msg_body)!
	if chain.len == 0 {
		return error('tls: empty certificate list')
	}
	return chain
}

// verify_server_chain verifies a presented Certificate body against roots:
// the direct-root fast path applies when the leaf is (or is issued by) a
// root; otherwise intermediates from the same flight build the longest
// valid path via x509.build_chain. Hostname, EKU (serverAuth), CRLs from
// SSL_CRL_FILE/SSL_CRL_DIR and OCSP staples are enforced; it returns the
// leaf public key for CertificateVerify. Prefer this over the
// extract_cert_der + direct-root loop in new call-sites.
pub fn verify_server_chain(cert_msg_body []u8, roots []x509.Certificate, hostname string) !x509.PublicKey {
	return verify_server_chain_stapled(cert_msg_body, roots, hostname, []u8{})
}

// verify_server_chain_stapled is verify_server_chain with an optional
// OCSP staple (status_request bytes from the transport; empty disables).
pub fn verify_server_chain_stapled(cert_msg_body []u8, roots []x509.Certificate, hostname string, ocsp_resp []u8) !x509.PublicKey {
	ders := extract_chain_ders(cert_msg_body)!
	mut certs := []x509.Certificate{}
	for d in ders {
		certs << x509.parse_der(d) or { return error(error_bad_signature) }
	}
	chain := x509.build_chain(certs[0], if certs.len > 1 {
		certs[1..]
	} else {
		[]x509.Certificate{}
	}, roots)!
	x509.verify_chain(chain, x509.VerifyOptions{
		usage:     .server_auth
		hostname:  hostname
		crls:      x509.load_system_crls()
		ocsp_resp: ocsp_resp
	})!
	return chain[0].public_key() or { return error(error_bad_signature) }
}

// verify_client_chain verifies a presented client Certificate body
// against roots (clientAuth EKU, no hostname check, CRLs enforced),
// returning the leaf public key for CertificateVerify.
pub fn verify_client_chain(chain [][]u8, roots []x509.Certificate) !x509.PublicKey {
	if chain.len == 0 {
		return error('tls: client certificate required')
	}
	mut certs := []x509.Certificate{}
	for d in chain {
		certs << x509.parse_der(d) or { return error(error_bad_signature) }
	}
	built := x509.build_chain(certs[0], if certs.len > 1 {
		certs[1..]
	} else {
		[]x509.Certificate{}
	}, roots)!
	x509.verify_chain(built, x509.VerifyOptions{
		usage: .client_auth
		crls:  x509.load_system_crls()
	})!
	return built[0].public_key() or { return error(error_bad_signature) }
}

// extract_ecdsa_pubkey extracts a P-256 public key from subjectPublicKeyInfo.
// extract_ecdsa_pubkey only supports uncompressed P-256 (prime256v1) points.
pub fn extract_ecdsa_pubkey(cert_der []u8) !ecdsa.PublicKey {
	// Certificate ::= SEQUENCE { tbs, sigAlg, sig }
	tag, cert_val, _ := der_tlv(cert_der) or { return err }
	if tag != 0x30 {
		return error('der: expected SEQUENCE for certificate')
	}
	// tbsCertificate
	ttag, tbs, _ := der_tlv(cert_val) or { return err }
	if ttag != 0x30 {
		return error('der: expected SEQUENCE for tbs')
	}
	// Walk tbs fields: version [0] (optional), serial, signature, issuer, validity, subject, spki
	mut off := 0
	// version: context tag 0 (optional)
	if off < tbs.len && tbs[off] == 0xa0 {
		_, _, n := der_tlv(tbs[off..]) or { return err }
		off += n
	}
	// skip serialNumber, signature, issuer, validity, subject (5 SEQUENCE/INTEGER fields)
	for _ in 0 .. 5 {
		_, _, n := der_tlv(tbs[off..]) or { return err }
		off += n
		if off > tbs.len {
			return error('der: truncated tbs')
		}
	}
	// subjectPublicKeyInfo
	_, spki, n := der_tlv(tbs[off..]) or { return err }
	off += n
	_ = off
	// spki: SEQUENCE { algorithm, subjectPublicKey BIT STRING }
	atag, alg, _ := der_tlv(spki) or { return err }
	if atag != 0x30 {
		return error('der: expected SEQUENCE for spki')
	}
	// algorithm: SEQUENCE { oid, params }
	_, _, an := der_tlv(alg) or { return err }
	_ = an
	// skip algorithm (first field of spki)
	// re-parse spki fields
	mut spki_off := 0
	_, alg_val, an2 := der_tlv(spki[spki_off..]) or { return err }
	spki_off += an2
	_ = alg_val
	// subjectPublicKey BIT STRING
	btag, bitstr, _ := der_tlv(spki[spki_off..]) or { return err }
	if btag != 0x03 {
		return error('der: expected BIT STRING for public key')
	}
	if bitstr.len < 2 {
		return error('der: empty public key')
	}
	// first byte = unused bits (must be 0)
	if bitstr[0] != 0 {
		return error('der: unsupported bit string padding')
	}
	pub_bytes := bitstr[1..].clone()
	// pub_bytes should be 0x04 + 64 bytes (uncompressed P-256 point)
	if pub_bytes.len != 65 || pub_bytes[0] != 0x04 {
		return error('der: expected uncompressed P-256 point')
	}
	return ecdsa.PublicKey.from_uncompressed_bytes(pub_bytes, ecdsa.CurveOptions{}) or { return err }
}
