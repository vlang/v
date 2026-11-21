module rsa

// This file reads and writes the few DER structures that carry RSA keys: the
// PKCS #1 RSAPublicKey and RSAPrivateKey (RFC 8017 §A.1), the X.509
// SubjectPublicKeyInfo (RFC 5280 §4.1.2.7) and the PKCS #8 PrivateKeyInfo
// (RFC 5208 and RFC 5958). Parsing is strict DER: definite and minimal
// lengths, minimal positive integers, no trailing data. The optional
// attributes of a PKCS #8 key are skipped without being decoded.

const tag_integer = u8(0x02)
const tag_bit_string = u8(0x03)
const tag_octet_string = u8(0x04)
const tag_null = u8(0x05)
const tag_oid = u8(0x06)
const tag_sequence = u8(0x30)

// The DER encoding of the rsaEncryption object identifier, 1.2.840.113549.1.1.1.
const oid_rsa_encryption = [u8(0x2a), 0x86, 0x48, 0x86, 0xf7, 0x0d, 0x01, 0x01, 0x01]!

// DerReader reads consecutive DER elements from `data`.
struct DerReader {
	data []u8
mut:
	pos int
}

// read returns the contents of the next element, which must have tag `tag`.
fn (mut r DerReader) read(tag u8) ![]u8 {
	if r.pos + 2 > r.data.len {
		return error('crypto.rsa: truncated DER data')
	}
	if r.data[r.pos] != tag {
		return error('crypto.rsa: unexpected DER tag 0x${r.data[r.pos]:02x}, expected 0x${tag:02x}')
	}
	mut length := int(r.data[r.pos + 1])
	mut start := r.pos + 2
	if length & 0x80 != 0 {
		n := length & 0x7f
		if n == 0 || n > 3 || start + n > r.data.len {
			return error('crypto.rsa: unsupported DER length')
		}
		mut long := u32(0)
		for i in 0 .. n {
			long = (long << 8) | u32(r.data[start + i])
		}
		length = int(long)
		// DER requires the shortest form: no leading zero byte, and the long
		// form only for lengths of 128 and more.
		if r.data[start] == 0 || length < 0x80 {
			return error('crypto.rsa: non-minimal DER length')
		}
		start += n
	}
	if length > r.data.len - start {
		return error('crypto.rsa: truncated DER data')
	}
	r.pos = start + length
	return r.data[start..r.pos]
}

// read_integer returns the big-endian magnitude of the next INTEGER, which must
// be positive and minimally encoded.
fn (mut r DerReader) read_integer() ![]u8 {
	b := r.read(tag_integer)!
	if b.len == 0 {
		return error('crypto.rsa: empty DER integer')
	}
	if b[0] & 0x80 != 0 {
		return error('crypto.rsa: negative DER integer')
	}
	if b.len > 1 && b[0] == 0 && b[1] & 0x80 == 0 {
		return error('crypto.rsa: non-minimal DER integer')
	}
	if b.len > 1 && b[0] == 0 {
		return b[1..]
	}
	return b
}

// read_small_int returns the next INTEGER as an int, for versions.
fn (mut r DerReader) read_small_int() !int {
	b := r.read_integer()!
	if b.len > 3 {
		return error('crypto.rsa: DER integer too large')
	}
	mut v := u32(0)
	for x in b {
		v = (v << 8) | u32(x)
	}
	return int(v)
}

// finish fails if data remains after the last element read.
fn (r &DerReader) finish() ! {
	if r.pos != r.data.len {
		return error('crypto.rsa: trailing data after DER structure')
	}
}

// read_sequence_body returns a reader over the single SEQUENCE in `data`.
fn read_sequence_body(data []u8) !DerReader {
	mut outer := DerReader{
		data: data
	}
	body := outer.read(tag_sequence)!
	outer.finish()!
	return DerReader{
		data: body
	}
}

// read_rsa_algorithm reads an AlgorithmIdentifier, which must be rsaEncryption
// with NULL or absent parameters.
fn (mut r DerReader) read_rsa_algorithm() ! {
	mut alg := DerReader{
		data: r.read(tag_sequence)!
	}
	oid := alg.read(tag_oid)!
	if oid != oid_rsa_encryption[..] {
		return error('crypto.rsa: key algorithm is not rsaEncryption')
	}
	if alg.pos < alg.data.len {
		params := alg.read(tag_null)!
		if params.len != 0 {
			return error('crypto.rsa: invalid rsaEncryption parameters')
		}
	}
	alg.finish()!
}

// der_length encodes a DER length.
fn der_length(n int) []u8 {
	if n < 0x80 {
		return [u8(n)]
	}
	mut out := []u8{}
	mut v := n
	for v > 0 {
		out.prepend(u8(v & 0xff))
		v >>= 8
	}
	out.prepend(u8(0x80 | out.len))
	return out
}

// der_element encodes a DER element.
fn der_element(tag u8, contents []u8) []u8 {
	mut out := [tag]
	out << der_length(contents.len)
	out << contents
	return out
}

// der_integer encodes the big-endian magnitude `b` as a DER INTEGER.
fn der_integer(b []u8) []u8 {
	mut start := 0
	for start < b.len - 1 && b[start] == 0 {
		start++
	}
	mut contents := []u8{}
	if b.len == 0 {
		contents << 0
	} else {
		if b[start] & 0x80 != 0 {
			contents << 0
		}
		contents << b[start..]
	}
	return der_element(tag_integer, contents)
}

// der_rsa_algorithm encodes the rsaEncryption AlgorithmIdentifier.
fn der_rsa_algorithm() []u8 {
	mut contents := der_element(tag_oid, oid_rsa_encryption[..])
	contents << der_element(tag_null, [])
	return der_element(tag_sequence, contents)
}
