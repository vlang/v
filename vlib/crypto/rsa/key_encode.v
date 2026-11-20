// DER codecs for RSA keys: PKCS#1, PKCS#8 and SPKI, plus PEM helpers.
module rsa

import crypto.pem
import math.big

// oid_rsa_encryption is the DER content bytes of 1.2.840.113549.1.1.1
// (rsaEncryption). It is the single OID table shared by the PKCS#8 and
// SPKI codecs in this module.
const oid_rsa_encryption = [u8(0x2a), 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x01]

// der_len_bytes encodes a definite-form DER length.
fn der_len_bytes(n int) []u8 {
	if n < 0x80 {
		return [u8(n)]
	}
	mut tmp := n
	mut rev := []u8{}
	for tmp > 0 {
		rev << u8(tmp & 0xff)
		tmp >>= 8
	}
	mut out := []u8{cap: rev.len + 1}
	out << u8(0x80 | rev.len)
	for i := rev.len - 1; i >= 0; i-- {
		out << rev[i]
	}
	return out
}

// der_tlv wraps content in a tag-length-value triple.
fn der_tlv(tag u8, content []u8) []u8 {
	mut out := []u8{cap: content.len + 6}
	out << tag
	out << der_len_bytes(content.len)
	out << content
	return out
}

// der_seq wraps content in a SEQUENCE.
fn der_seq(content []u8) []u8 {
	return der_tlv(0x30, content)
}

// der_null returns the ASN.1 NULL TLV.
fn der_null() []u8 {
	return [u8(0x05), 0x00]
}

// der_oid wraps OID content bytes in an OID TLV.
fn der_oid(content []u8) []u8 {
	return der_tlv(0x06, content)
}

// der_bit_string wraps content in a BIT STRING with zero unused bits.
fn der_bit_string(content []u8) []u8 {
	mut c := []u8{cap: content.len + 1}
	c << u8(0x00)
	c << content
	return der_tlv(0x03, c)
}

// der_octet_string wraps content in an OCTET STRING.
fn der_octet_string(content []u8) []u8 {
	return der_tlv(0x04, content)
}

// der_encode_integer encodes a non-negative Integer as a DER INTEGER.
fn der_encode_integer(x big.Integer) []u8 {
	b, _ := x.bytes()
	if b.len == 0 {
		return der_tlv(0x02, [u8(0x00)])
	}
	if b[0] & 0x80 != 0 {
		mut padded := []u8{cap: b.len + 1}
		padded << u8(0x00)
		padded << b
		return der_tlv(0x02, padded)
	}
	return der_tlv(0x02, b)
}

// rsa_alg_id returns the rsaEncryption AlgorithmIdentifier
// (SEQUENCE { OID, NULL }) shared by PKCS#8 and SPKI.
fn rsa_alg_id() []u8 {
	mut content := []u8{}
	content << der_oid(oid_rsa_encryption.clone())
	content << der_null()
	return der_seq(content)
}

// der_read parses one TLV at pos, returning its tag, value and the next position.
fn der_read(data []u8, pos int) !(u8, []u8, int) {
	if pos < 0 || pos + 2 > data.len {
		return error('rsa: truncated DER')
	}
	tag := data[pos]
	mut off := pos + 1
	mut length := 0
	if data[off] & 0x80 == 0 {
		length = int(data[off])
		off++
	} else {
		nb := int(data[off] & 0x7f)
		off++
		if nb == 0 || nb > 4 || off + nb > data.len {
			return error('rsa: bad DER length')
		}
		for i in 0 .. nb {
			length = length * 256 + int(data[off + i])
		}
		off += nb
		if length < 128 {
			return error('rsa: non-minimal DER length')
		}
	}
	if off + length > data.len {
		return error('rsa: truncated DER value')
	}
	return tag, data[off..off + length].clone(), off + length
}

// der_expect parses one TLV at pos and checks its tag.
fn der_expect(data []u8, pos int, tag u8, what string) !([]u8, int) {
	t, content, next := der_read(data, pos)!
	if t != tag {
		return error('rsa: expected ${what}')
	}
	return content, next
}

// der_parse_integer parses a non-negative DER INTEGER at pos.
fn der_parse_integer(data []u8, pos int) !(big.Integer, int) {
	content, next := der_expect(data, pos, 0x02, 'INTEGER')!
	if content.len == 0 {
		return error('rsa: empty INTEGER')
	}
	if content[0] & 0x80 != 0 {
		return error('rsa: negative INTEGER')
	}
	return big.integer_from_bytes(content), next
}

// to_pkcs1_der encodes the key as PKCS#1 RSAPrivateKey DER.
pub fn (key PrivateKey) to_pkcs1_der() []u8 {
	mut content := []u8{}
	content << der_encode_integer(big.zero_int)
	content << der_encode_integer(key.n)
	content << der_encode_integer(big.integer_from_u64(key.e))
	content << der_encode_integer(key.d)
	content << der_encode_integer(key.p)
	content << der_encode_integer(key.q)
	content << der_encode_integer(key.dp)
	content << der_encode_integer(key.dq)
	content << der_encode_integer(key.qinv)
	return der_seq(content)
}

// private_key_from_pkcs1_der parses a PKCS#1 RSAPrivateKey DER blob.
pub fn private_key_from_pkcs1_der(der []u8) !PrivateKey {
	tag, content, next := der_read(der, 0)!
	if tag != 0x30 || next != der.len {
		return error('rsa: expected SEQUENCE private key')
	}
	mut pos := 0
	version, pos2 := der_parse_integer(content, pos)!
	pos = pos2
	if version != big.zero_int {
		return error('rsa: unsupported private key version')
	}
	n, pos3 := der_parse_integer(content, pos)!
	pos = pos3
	e_int, pos4 := der_parse_integer(content, pos)!
	pos = pos4
	d, pos5 := der_parse_integer(content, pos)!
	pos = pos5
	p, pos6 := der_parse_integer(content, pos)!
	pos = pos6
	q, pos7 := der_parse_integer(content, pos)!
	pos = pos7
	dp, pos8 := der_parse_integer(content, pos)!
	pos = pos8
	dq, pos9 := der_parse_integer(content, pos)!
	pos = pos9
	qinv, pos10 := der_parse_integer(content, pos)!
	pos = pos10
	if pos != content.len {
		return error('rsa: trailing private key data')
	}
	eb, _ := e_int.bytes()
	if eb.len > 8 {
		return error('rsa: exponent too large')
	}
	mut e_val := u64(0)
	for b in eb {
		e_val = (e_val << 8) | u64(b)
	}
	return PrivateKey{
		n:    n
		e:    e_val
		d:    d
		p:    p
		q:    q
		dp:   dp
		dq:   dq
		qinv: qinv
	}
}

// to_pkcs8_der encodes the key as PKCS#8 PrivateKeyInfo DER.
pub fn (key PrivateKey) to_pkcs8_der() []u8 {
	mut content := []u8{}
	content << der_encode_integer(big.zero_int)
	content << rsa_alg_id()
	content << der_octet_string(key.to_pkcs1_der())
	return der_seq(content)
}

// private_key_from_pkcs8_der parses a PKCS#8 PrivateKeyInfo DER blob
// holding an RSA key.
pub fn private_key_from_pkcs8_der(der []u8) !PrivateKey {
	tag, content, next := der_read(der, 0)!
	if tag != 0x30 || next != der.len {
		return error('rsa: expected SEQUENCE private key')
	}
	mut pos := 0
	version, pos2 := der_parse_integer(content, pos)!
	pos = pos2
	if version != big.zero_int {
		return error('rsa: unsupported PKCS#8 version')
	}
	alg, pos3 := der_expect(content, pos, 0x30, 'algorithm')!
	pos = pos3
	otag, oid_content, opos := der_read(alg, 0)!
	if otag != 0x06 || oid_content != oid_rsa_encryption {
		return error('rsa: not an RSA private key')
	}
	if opos < alg.len {
		ptag, pcontent, pnext := der_read(alg, opos)!
		if ptag != 0x05 || pcontent.len != 0 || pnext != alg.len {
			return error('rsa: bad PKCS#8 parameters')
		}
	}
	inner, pos4 := der_expect(content, pos, 0x04, 'private key')!
	pos = pos4
	if pos != content.len {
		return error('rsa: trailing PKCS#8 data')
	}
	return private_key_from_pkcs1_der(inner)
}

// to_pkcs1_der encodes the key as PKCS#1 RSAPublicKey DER.
pub fn (key PublicKey) to_pkcs1_der() []u8 {
	mut content := []u8{}
	content << der_encode_integer(key.n)
	content << der_encode_integer(big.integer_from_u64(key.e))
	return der_seq(content)
}

// public_key_from_pkcs1_der parses a PKCS#1 RSAPublicKey DER blob.
pub fn public_key_from_pkcs1_der(der []u8) !PublicKey {
	tag, content, next := der_read(der, 0)!
	if tag != 0x30 || next != der.len {
		return error('rsa: expected SEQUENCE public key')
	}
	n, pos := der_parse_integer(content, 0)!
	e_int, pos2 := der_parse_integer(content, pos)!
	if pos2 != content.len {
		return error('rsa: trailing public key data')
	}
	eb, _ := e_int.bytes()
	if eb.len > 8 {
		return error('rsa: exponent too large')
	}
	mut e_val := u64(0)
	for b in eb {
		e_val = (e_val << 8) | u64(b)
	}
	return PublicKey{
		n: n
		e: e_val
	}
}

// to_spki_der encodes the key as SubjectPublicKeyInfo DER.
pub fn (key PublicKey) to_spki_der() []u8 {
	mut content := []u8{}
	content << rsa_alg_id()
	content << der_bit_string(key.to_pkcs1_der())
	return der_seq(content)
}

// public_key_from_spki_der parses a SubjectPublicKeyInfo DER blob holding
// an RSA key.
pub fn public_key_from_spki_der(der []u8) !PublicKey {
	tag, content, next := der_read(der, 0)!
	if tag != 0x30 || next != der.len {
		return error('rsa: expected SEQUENCE public key')
	}
	mut pos := 0
	alg, pos2 := der_expect(content, pos, 0x30, 'algorithm')!
	pos = pos2
	otag, oid_content, opos := der_read(alg, 0)!
	if otag != 0x06 || oid_content != oid_rsa_encryption {
		return error('rsa: not an RSA public key')
	}
	if opos < alg.len {
		ptag, pcontent, pnext := der_read(alg, opos)!
		if ptag != 0x05 || pcontent.len != 0 || pnext != alg.len {
			return error('rsa: bad SPKI parameters')
		}
	}
	bs, pos3 := der_expect(content, pos, 0x03, 'public key')!
	pos = pos3
	if pos != content.len {
		return error('rsa: trailing SPKI data')
	}
	if bs.len < 1 || bs[0] != 0 {
		return error('rsa: bad public key padding')
	}
	return public_key_from_pkcs1_der(bs[1..].clone())
}

// to_pem_pkcs1 returns the key as a `RSA PRIVATE KEY` PEM block.
pub fn (key PrivateKey) to_pem_pkcs1() !string {
	return pem.Block{
		block_type: 'RSA PRIVATE KEY'
		data:       key.to_pkcs1_der()
	}.encode(pem.EncodeConfig{})
}

// to_pem_pkcs8 returns the key as a `PRIVATE KEY` (PKCS#8) PEM block.
pub fn (key PrivateKey) to_pem_pkcs8() !string {
	return pem.Block{
		block_type: 'PRIVATE KEY'
		data:       key.to_pkcs8_der()
	}.encode(pem.EncodeConfig{})
}

// to_pem_spki returns the key as a `PUBLIC KEY` (SPKI) PEM block.
pub fn (key PublicKey) to_pem_spki() !string {
	return pem.Block{
		block_type: 'PUBLIC KEY'
		data:       key.to_spki_der()
	}.encode(pem.EncodeConfig{})
}

// private_key_from_pem parses a `RSA PRIVATE KEY` (PKCS#1) or
// `PRIVATE KEY` (PKCS#8) PEM block.
pub fn private_key_from_pem(text string) !PrivateKey {
	block, _ := pem.decode(text) or { return error('rsa: invalid PEM') }
	if block.block_type == 'RSA PRIVATE KEY' {
		return private_key_from_pkcs1_der(block.data)
	}
	if block.block_type == 'PRIVATE KEY' {
		return private_key_from_pkcs8_der(block.data)
	}
	return error('rsa: unsupported PEM block `${block.block_type}`')
}

// public_key_from_pem parses a `PUBLIC KEY` (SPKI) or `RSA PUBLIC KEY`
// (PKCS#1) PEM block.
pub fn public_key_from_pem(text string) !PublicKey {
	block, _ := pem.decode(text) or { return error('rsa: invalid PEM') }
	if block.block_type == 'PUBLIC KEY' {
		return public_key_from_spki_der(block.data)
	}
	if block.block_type == 'RSA PUBLIC KEY' {
		return public_key_from_pkcs1_der(block.data)
	}
	return error('rsa: unsupported PEM block `${block.block_type}`')
}
