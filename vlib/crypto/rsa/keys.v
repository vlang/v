module rsa

import crypto.pem
import crypto.rand
import math.big

// generate_key returns a new private key with a modulus of `bits` bits and the
// public exponent 65537, from the operating system's source of entropy.
pub fn generate_key(bits int) !PrivateKey {
	if bits < min_key_bits || bits > max_key_bits {
		return error('crypto.rsa: ${bits}-bit keys are not supported, expected ${min_key_bits} to ${max_key_bits} bits')
	}
	e := big.integer_from_int(65537)
	one := big.integer_from_int(1)
	for {
		// rand.prime sets the two top bits, so the product has exactly `bits` bits.
		p := rand.prime((bits + 1) / 2)!
		q := rand.prime(bits / 2)!
		if p == q {
			continue
		}
		p1 := p - one
		q1 := q - one
		if e.gcd(p1) != one || e.gcd(q1) != one {
			continue
		}
		lambda := p1 * q1 / p1.gcd(q1)
		d := e.mod_inverse(lambda)!
		return PrivateKey.new(
			n: magnitude(p * q)
			e: 65537
			d: magnitude(d)
			p: magnitude(p)
			q: magnitude(q)
		)
	}
	return error('crypto.rsa: unreachable')
}

// PublicKey.from_der parses a public key in DER, as an X.509
// SubjectPublicKeyInfo (`PUBLIC KEY`) or a PKCS #1 RSAPublicKey
// (`RSA PUBLIC KEY`).
pub fn PublicKey.from_der(der []u8) !PublicKey {
	mut r := read_sequence_body(der)!
	if r.data.len > 0 && r.data[0] == tag_sequence {
		r.read_rsa_algorithm()!
		bit_string := r.read(tag_bit_string)!
		r.finish()!
		if bit_string.len == 0 || bit_string[0] != 0 {
			return error('crypto.rsa: invalid public key bit string')
		}
		return parse_pkcs1_public_key(bit_string[1..])
	}
	return parse_pkcs1_public_key(der)
}

// parse_pkcs1_public_key parses a PKCS #1 RSAPublicKey.
fn parse_pkcs1_public_key(der []u8) !PublicKey {
	mut r := read_sequence_body(der)!
	n := r.read_integer()!
	e := r.read_integer()!
	r.finish()!
	exponent := exponent_from_bytes(e)!
	return PublicKey.new(n, exponent)
}

// PrivateKey.from_der parses a private key in DER, as a PKCS #8
// PrivateKeyInfo (`PRIVATE KEY`) or a PKCS #1 RSAPrivateKey
// (`RSA PRIVATE KEY`).
pub fn PrivateKey.from_der(der []u8) !PrivateKey {
	mut r := read_sequence_body(der)!
	version := r.read_small_int()!
	if r.pos < r.data.len && r.data[r.pos] == tag_sequence {
		// PKCS #8, version 0, or version 1 with an optional public key.
		if version != 0 && version != 1 {
			return error('crypto.rsa: unsupported PKCS #8 version ${version}')
		}
		r.read_rsa_algorithm()!
		key := parse_pkcs1_private_key(r.read(tag_octet_string)!)!
		// Optional attributes [0], which carry nothing a signature needs, then,
		// in version 1 only, the public key [1] (RFC 5958), which must be the
		// public half of the key.
		if r.pos < r.data.len && r.data[r.pos] == 0xa0 {
			r.read(0xa0)!
		}
		if version == 1 && r.pos < r.data.len && r.data[r.pos] == 0x81 {
			embedded := r.read(0x81)!
			mut expected := [u8(0)]
			expected << key.public_key.to_pkcs1_der()
			if embedded != expected {
				return error('crypto.rsa: PKCS #8 public key does not match the private key')
			}
		}
		r.finish()!
		return key
	}
	return parse_pkcs1_private_key(der)
}

// parse_pkcs1_private_key parses a two-prime PKCS #1 RSAPrivateKey.
fn parse_pkcs1_private_key(der []u8) !PrivateKey {
	mut r := read_sequence_body(der)!
	version := r.read_small_int()!
	if version != 0 {
		return error('crypto.rsa: multi-prime RSA keys are not supported')
	}
	n := r.read_integer()!
	e := r.read_integer()!
	d := r.read_integer()!
	p := r.read_integer()!
	q := r.read_integer()!
	dp := r.read_integer()!
	dq := r.read_integer()!
	qinv := r.read_integer()!
	r.finish()!
	exponent := exponent_from_bytes(e)!
	return PrivateKey.new(
		n:    n
		e:    exponent
		d:    d
		p:    p
		q:    q
		dp:   dp
		dq:   dq
		qinv: qinv
	)
}

// to_der returns the public key in DER, as an X.509 SubjectPublicKeyInfo.
pub fn (k &PublicKey) to_der() []u8 {
	mut bit_string := [u8(0)]
	bit_string << k.to_pkcs1_der()
	mut contents := der_rsa_algorithm()
	contents << der_element(tag_bit_string, bit_string)
	return der_element(tag_sequence, contents)
}

// to_pkcs1_der returns the public key in DER, as a PKCS #1 RSAPublicKey.
pub fn (k &PublicKey) to_pkcs1_der() []u8 {
	mut contents := der_integer(k.n_bytes)
	contents << der_integer(exponent_bytes(k.e))
	return der_element(tag_sequence, contents)
}

// to_der returns the private key in DER, as a PKCS #8 PrivateKeyInfo.
pub fn (k &PrivateKey) to_der() []u8 {
	mut contents := der_integer([u8(0)])
	contents << der_rsa_algorithm()
	contents << der_element(tag_octet_string, k.to_pkcs1_der())
	return der_element(tag_sequence, contents)
}

// to_pkcs1_der returns the private key in DER, as a PKCS #1 RSAPrivateKey.
pub fn (k &PrivateKey) to_pkcs1_der() []u8 {
	c := k.components()
	mut contents := der_integer([u8(0)])
	for x in [c.n, exponent_bytes(c.e), c.d, c.p, c.q, c.dp, c.dq, c.qinv] {
		contents << der_integer(x)
	}
	return der_element(tag_sequence, contents)
}

// PublicKey.from_pem parses the first `PUBLIC KEY` or `RSA PUBLIC KEY` PEM
// block of `data`.
pub fn PublicKey.from_pem(data string) !PublicKey {
	return PublicKey.from_der(pem_data(data, ['PUBLIC KEY', 'RSA PUBLIC KEY'])!)
}

// PrivateKey.from_pem parses the first `PRIVATE KEY` or `RSA PRIVATE KEY` PEM
// block of `data`. Encrypted keys are not supported.
pub fn PrivateKey.from_pem(data string) !PrivateKey {
	return PrivateKey.from_der(pem_data(data, ['PRIVATE KEY', 'RSA PRIVATE KEY'])!)
}

// to_pem returns the public key as a `PUBLIC KEY` PEM block.
pub fn (k &PublicKey) to_pem() string {
	return pem_encode('PUBLIC KEY', k.to_der())
}

// to_pem returns the private key as a `PRIVATE KEY` PEM block.
pub fn (k &PrivateKey) to_pem() string {
	return pem_encode('PRIVATE KEY', k.to_der())
}

// pem_data returns the contents of the first PEM block of `data` whose type is
// one of `types`.
fn pem_data(data string, types []string) ![]u8 {
	mut rest := data
	for {
		block, next := pem.decode(rest) or { break }
		if block.block_type in types {
			if block.headers.len > 0 {
				return error('crypto.rsa: encrypted PEM keys are not supported')
			}
			return block.data
		}
		rest = next
	}
	return error('crypto.rsa: no ${types.join(' or ')} PEM block found')
}

// pem_encode returns `der` as a PEM block of type `block_type`.
fn pem_encode(block_type string, der []u8) string {
	block := pem.Block{
		block_type: block_type
		data:       der
	}
	// A block without headers always encodes.
	return (block.encode() or { '' }) + '\n'
}

// exponent_from_bytes returns the big-endian public exponent `e` as an int.
fn exponent_from_bytes(e []u8) !int {
	if e.len > 4 || (e.len == 4 && e[0] & 0x80 != 0) {
		return error('crypto.rsa: public exponent too large')
	}
	mut v := u32(0)
	for b in e {
		v = (v << 8) | u32(b)
	}
	return int(v)
}
