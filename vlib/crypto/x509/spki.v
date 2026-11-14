module x509

pub struct RsaPublicKey {
pub:
	der []u8 // RSAPublicKey DER
}

pub struct EcdsaP256PublicKey {
pub:
	point []u8 // 65 bytes uncompressed
}

pub struct EcdsaP384PublicKey {
pub:
	point []u8 // 97 bytes uncompressed
}

pub struct EcdsaP521PublicKey {
pub:
	point []u8 // 133 bytes uncompressed
}

pub struct MlDsa44PublicKey {
pub:
	raw []u8 // 1312 bytes
}

pub struct MlDsa65PublicKey {
pub:
	raw []u8 // 1952 bytes
}

pub struct MlDsa87PublicKey {
pub:
	raw []u8 // 2592 bytes
}

pub type PublicKey = RsaPublicKey
	| EcdsaP256PublicKey
	| EcdsaP384PublicKey
	| EcdsaP521PublicKey
	| MlDsa44PublicKey
	| MlDsa65PublicKey
	| MlDsa87PublicKey

// OID strings.
const oid_rsa_encryption = '1.2.840.113549.1.1.1'
const oid_ec_public_key = '1.2.840.10045.2.1'
const oid_prime256v1 = '1.2.840.10045.3.1.7'
const oid_secp384r1 = '1.3.132.0.34'
const oid_secp521r1 = '1.3.132.0.35'
const oid_mldsa_44 = '2.16.840.1.101.3.4.3.17'
const oid_mldsa_65 = '2.16.840.1.101.3.4.3.18'
const oid_mldsa_87 = '2.16.840.1.101.3.4.3.19'

pub fn (c &Certificate) public_key() !PublicKey {
	if c.raw.len == 0 {
		return error('x509: no raw certificate bytes')
	}
	// Certificate ::= SEQUENCE { tbs, sigAlg, sig }
	cert_tlv, _ := split_tlv(c.raw, 0)!
	// cert_tlv includes tag+length+content; content starts after header
	hl, _ := der_length(c.raw, 1)!
	tbs_full := cert_tlv[1 + hl..]
	// tbs is a SEQUENCE
	tbs_content := expect_tag(tbs_full, 0, 0x30, 'tbsCertificate')!
	// walk tbs fields: optional version [0], serial, sig, issuer, validity, subject, spki, ...
	mut pos := 0
	if pos < tbs_content.len && tbs_content[pos] == 0xa0 {
		pos = skip_tlv(tbs_content, pos)!
	}
	// skip serialNumber, signature, issuer, validity, subject (5 fields)
	for _ in 0 .. 5 {
		pos = skip_tlv(tbs_content, pos)!
	}
	// subjectPublicKeyInfo
	spki := expect_tag(tbs_content[pos..], 0, 0x30, 'subjectPublicKeyInfo')!
	// parse spki: algorithm SEQUENCE, then BIT STRING
	mut sp := 0
	alg_id := expect_tag(spki[sp..], 0, 0x30, 'algorithm')!
	sp += tlv_total_len(spki, sp)!
	// read OID from alg_id
	oid, oid_len := der_oid(alg_id) or { return err }
	// read params (optional) - skip
	// subjectPublicKey BIT STRING
	bs := expect_tag(spki[sp..], 0, 0x03, 'subjectPublicKey')!
	if bs.len < 1 || bs[0] != 0 {
		return error('x509: bad bit string padding')
	}
	pub_bytes := bs[1..].clone()
	if oid == oid_rsa_encryption {
		return RsaPublicKey{ der: pub_bytes }
	}
	// ML-DSA keys use the signature-scheme OID as the SPKI algorithm OID
	// with absent parameters (draft-ietf-lamps-dilithium-certificates); a
	// present params field is rejected so it cannot change the key's meaning.
	// The BIT STRING holds the raw public key bytes.
	if oid == oid_mldsa_44 || oid == oid_mldsa_65 || oid == oid_mldsa_87 {
		if oid_len != alg_id.len {
			return error('x509: ML-DSA parameters must be absent')
		}
	}
	if oid == oid_mldsa_44 {
		if pub_bytes.len != 1312 {
			return error('x509: bad ML-DSA-44 public key length')
		}
		return MlDsa44PublicKey{ raw: pub_bytes }
	}
	if oid == oid_mldsa_65 {
		if pub_bytes.len != 1952 {
			return error('x509: bad ML-DSA-65 public key length')
		}
		return MlDsa65PublicKey{ raw: pub_bytes }
	}
	if oid == oid_mldsa_87 {
		if pub_bytes.len != 2592 {
			return error('x509: bad ML-DSA-87 public key length')
		}
		return MlDsa87PublicKey{ raw: pub_bytes }
	}
	if oid == oid_ec_public_key {
		curve_oid, _ := der_oid(alg_id[oid_len..]) or {
			return error('x509: expected EC curve OID')
		}
		if pub_bytes.len == 0 || pub_bytes[0] != 0x04 {
			return error('x509: expected uncompressed EC point')
		}
		if curve_oid == oid_prime256v1 {
			if pub_bytes.len != 65 {
				return error('x509: bad P-256 point length')
			}
			return EcdsaP256PublicKey{ point: pub_bytes }
		}
		if curve_oid == oid_secp384r1 {
			if pub_bytes.len != 97 {
				return error('x509: bad P-384 point length')
			}
			return EcdsaP384PublicKey{ point: pub_bytes }
		}
		if curve_oid == oid_secp521r1 {
			if pub_bytes.len != 133 {
				return error('x509: bad P-521 point length')
			}
			return EcdsaP521PublicKey{ point: pub_bytes }
		}
		return error('x509: unsupported EC curve')
	}
	return error('x509: unsupported public key algorithm')
}

// der_oid decodes an OID TLV into a dotted string.
fn der_oid(tlv []u8) !(string, int) {
	if tlv.len == 0 || tlv[0] != 0x06 {
		return error('x509: expected OID')
	}
	hl, content_len := der_length(tlv, 1)!
	content := tlv[1 + hl..1 + hl + content_len]
	if content.len == 0 {
		return error('x509: empty OID')
	}
	mut parts := []int{}
	// first byte encodes two components
	first := int(content[0])
	parts << first / 40
	parts << first % 40
	mut i := 1
	for i < content.len {
		mut val := 0
		for i < content.len && int(content[i]) >= 0x80 {
			val = val * 128 + (int(content[i]) & 0x7f)
			i++
		}
		if i < content.len {
			val = val * 128 + int(content[i])
			i++
		}
		parts << val
	}
	mut s := ''
	for idx, p in parts {
		if idx > 0 { s += '.' }
		s += p.str()
	}
	return s, 1 + hl + content_len
}
