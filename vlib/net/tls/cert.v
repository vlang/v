module tls

import crypto.ecdsa

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
			length = (length << 8) | int(data[off + i])
		}
		off += nb
	}
	if off + length > data.len {
		return error('der: truncated value')
	}
	val := data[off..off + length].clone()
	return tag, val, off + length
}

// extract_cert_der returns the first certificate's DER from a Certificate message body.
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

pub fn parse_cert_pubkey(cert_msg_body []u8) !ecdsa.PublicKey {
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
	return extract_ecdsa_pubkey(cert_der)
}

// parse_cert_verify_sig extracts the signature from a TLS 1.3
// CertificateVerify message body.
pub fn parse_cert_verify_sig(cv_body []u8) ![]u8 {
	if cv_body.len < 4 {
		return error('tls: truncated CertificateVerify')
	}
	// algorithm (2) + sig length (2) + sig
	sig_len := (int(cv_body[2]) << 8) | int(cv_body[3])
	if 4 + sig_len > cv_body.len {
		return error('tls: truncated signature')
	}
	return cv_body[4..4 + sig_len].clone()
}

// public key from subjectPublicKeyInfo. Only P-256 (prime256v1) is supported.
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
