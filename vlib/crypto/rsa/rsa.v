// RSA public-key cryptography (RFC 8017) implemented in pure V.
//
// Key generation draws entropy from `crypto.rand` and uses `math.big`
// for primality testing (Miller-Rabin via `is_probably_prime_checked`,
// reached through `rand.prime`) and modular arithmetic. The encodings
// produced here are byte-compatible with the verification paths in
// `net.tls`; the implementations are intentionally not shared so this
// module never imports `net.tls`. That keeps the dependency direction
// `net` -> `crypto` intact and avoids an import cycle once
// `crypto.x509` grows RSA support.
module rsa

import crypto.rand
import crypto.sha256
import crypto.sha512
import crypto.subtle
import math.big

// Hash identifies the digest function used with an RSA signature.
pub enum Hash {
	sha256
	sha384
	sha512
}

// digest_len returns the digest length in bytes.
fn (h Hash) digest_len() int {
	return match h {
		.sha256 { 32 }
		.sha384 { 48 }
		.sha512 { 64 }
	}
}

// hash_fn returns the digest function for the hash.
fn (h Hash) hash_fn() fn ([]u8) []u8 {
	return match h {
		.sha256 { sha256.sum256 }
		.sha384 { sha512.sum384 }
		.sha512 { sha512.sum512 }
	}
}

// hash_bytes hashes msg with the hash.
fn (h Hash) hash_bytes(msg []u8) []u8 {
	return h.hash_fn()(msg)
}

// digest_info_prefix returns the PKCS#1 v1.5 DigestInfo prefix for the
// hash, i.e. everything before the digest in an encoded message. The
// table mirrors `net.tls` so signatures verify on both sides.
fn (h Hash) digest_info_prefix() []u8 {
	return match h {
		.sha256 {
			[u8(0x30), 0x31, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04,
				0x02, 0x01, 0x05, 0x00, 0x04, 0x20]
		}
		.sha384 {
			[u8(0x30), 0x41, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04,
				0x02, 0x02, 0x05, 0x00, 0x04, 0x30]
		}
		.sha512 {
			[u8(0x30), 0x51, 0x30, 0x0d, 0x06, 0x09, 0x60, 0x86, 0x48, 0x01, 0x65, 0x03, 0x04,
				0x02, 0x03, 0x05, 0x00, 0x04, 0x40]
		}
	}
}

// PublicKey is an RSA public key: the modulus n and the public exponent e.
pub struct PublicKey {
pub:
	n big.Integer
	e u64
}

// PrivateKey is an RSA private key with CRT precomputations, mirroring
// the layout of PKCS#1 RSAPrivateKey.
pub struct PrivateKey {
pub:
	n    big.Integer
	e    u64
	d    big.Integer
	p    big.Integer
	q    big.Integer
	dp   big.Integer
	dq   big.Integer
	qinv big.Integer
}

// public_key returns the public half of the private key.
pub fn (key PrivateKey) public_key() PublicKey {
	return PublicKey{
		n: key.n
		e: key.e
	}
}

// KeyGenOptions tunes RSA key generation.
@[params]
pub struct KeyGenOptions {
pub:
	// bits is the exact modulus size. 2048 and 3072 bits are the
	// recommended production sizes; 1024 exists for fast tests only and
	// must not protect real data.
	bits int = 2048
	// e is the public exponent, 65537 by default.
	e u64 = 65537
}

// generate_key creates an RSA private key of exactly `opts.bits` bits with
// public exponent `opts.e`. Sizes below 1024 bits are rejected.
// Primes come from `rand.prime`, whose Miller-Rabin checks use
// `big.Integer.is_probably_prime_checked`.
pub fn generate_key(opts KeyGenOptions) !PrivateKey {
	bits := opts.bits
	e := opts.e
	if bits < 1024 {
		return error('rsa: key size too small (want >= 1024 bits)')
	}
	if e < 3 || e % 2 == 0 {
		return error('rsa: public exponent must be odd and >= 3')
	}
	exp := big.integer_from_u64(e)
	half := bits / 2
	for {
		p := rand.prime(half)!
		q := rand.prime(bits - half)!
		if p == q {
			continue
		}
		n := p * q
		if n.bit_len() != bits {
			continue
		}
		p1 := p - big.one_int
		q1 := q - big.one_int
		phi := p1 * q1
		if exp.gcd(phi) != big.one_int {
			continue
		}
		d := exp.mod_inverse(phi) or { continue }
		qinv := q.mod_inverse(p) or { continue }
		return PrivateKey{
			n:    n
			e:    e
			d:    d
			p:    p
			q:    q
			dp:   d % p1
			dq:   d % q1
			qinv: qinv
		}
	}
	return error('rsa: unreachable')
}

// validate checks the internal consistency of the key: n == p*q and
// e*d == 1 mod phi(p, q). It does not assess prime quality.
pub fn (key PrivateKey) validate() ! {
	if key.p.signum <= 0 || key.q.signum <= 0 {
		return error('rsa: invalid prime factor')
	}
	if key.p * key.q != key.n {
		return error('rsa: n != p*q')
	}
	phi := (key.p - big.one_int) * (key.q - big.one_int)
	exp := big.integer_from_u64(key.e)
	if (exp * key.d) % phi != big.one_int {
		return error('rsa: e*d != 1 mod phi')
	}
}

// encrypt performs the raw RSA encryption primitive (RSAEP): m^e mod n.
// The message representative must satisfy 0 <= m < n; padding (e.g.
// PKCS#1 v1.5 or OAEP) is the caller's job.
pub fn (key PublicKey) encrypt(m big.Integer) !big.Integer {
	if m.signum < 0 || m.abs_cmp(key.n) >= 0 {
		return error('rsa: message representative out of range')
	}
	return m.mod_pow(key.e, key.n)
}

// decrypt performs the raw RSA decryption primitive (RSADP): c^d mod n.
// The ciphertext representative must satisfy 0 <= c < n.
pub fn (key PrivateKey) decrypt(c big.Integer) !big.Integer {
	if c.signum < 0 || c.abs_cmp(key.n) >= 0 {
		return error('rsa: ciphertext representative out of range')
	}
	return c.big_mod_pow(key.d, key.n)
}

// int_to_fixed_bytes encodes x as exactly k big-endian bytes.
fn int_to_fixed_bytes(x big.Integer, k int) ![]u8 {
	b, _ := x.bytes()
	if b.len > k {
		return error('rsa: integer too large')
	}
	mut out := []u8{len: k}
	for i in 0 .. b.len {
		out[k - b.len + i] = b[i]
	}
	return out
}

// pkcs1_v15_encode builds the PKCS#1 v1.5 encoded message EM for a digest.
fn pkcs1_v15_encode(digest []u8, h Hash, k int) ![]u8 {
	if digest.len != h.digest_len() {
		return error('rsa: bad digest length for hash')
	}
	prefix := h.digest_info_prefix()
	t_len := prefix.len + digest.len
	if k < t_len + 11 {
		return error('rsa: key too short for PKCS#1 v1.5')
	}
	mut em := []u8{cap: k}
	em << u8(0x00)
	em << u8(0x01)
	for _ in 0 .. k - t_len - 3 {
		em << u8(0xff)
	}
	em << u8(0x00)
	em << prefix
	em << digest
	return em
}

// sign_pkcs1_v15 signs a precomputed digest with PKCS#1 v1.5 (RFC 8017
// §8.2.1). The caller hashes the message first with the matching digest.
pub fn (key PrivateKey) sign_pkcs1_v15(digest []u8, h Hash) ![]u8 {
	k := (key.n.bit_len() + 7) / 8
	em := pkcs1_v15_encode(digest, h, k)!
	m := big.integer_from_bytes(em)
	s := m.big_mod_pow(key.d, key.n)!
	return int_to_fixed_bytes(s, k)
}

// verify_pkcs1_v15 verifies a PKCS#1 v1.5 signature over a precomputed digest.
pub fn (key PublicKey) verify_pkcs1_v15(digest []u8, sig []u8, h Hash) ! {
	k := (key.n.bit_len() + 7) / 8
	if sig.len != k {
		return error('rsa: bad PKCS#1 v1.5 signature length')
	}
	s := big.integer_from_bytes(sig)
	m := s.mod_pow(key.e, key.n)
	em := int_to_fixed_bytes(m, k)!
	expected := pkcs1_v15_encode(digest, h, k)!
	if subtle.constant_time_compare(em, expected) != 1 {
		return error('rsa: PKCS#1 v1.5 verification failed')
	}
}

// pss_mgf1 implements MGF1 (RFC 8017, appendix B.2.1).
fn pss_mgf1(hash_fn fn ([]u8) []u8, hlen int, seed []u8, out_len int) ![]u8 {
	if out_len > (hlen << 32) {
		return error('rsa: PSS mask too long')
	}
	mut out := []u8{cap: out_len}
	mut counter := u32(0)
	for out.len < out_len {
		mut block := []u8{cap: seed.len + 4}
		block << seed
		block << u8(counter >> 24)
		block << u8(counter >> 16)
		block << u8(counter >> 8)
		block << u8(counter)
		out << hash_fn(block)
		counter++
	}
	return out[..out_len]
}

// pss_encode builds the EMSA-PSS encoded message EM for msg (RFC 8017 §9.1.1).
fn pss_encode(hash_fn fn ([]u8) []u8, hlen int, msg []u8, em_bits int, salt []u8) ![]u8 {
	em_len := (em_bits + 7) / 8
	if em_len < hlen + salt.len + 2 {
		return error('rsa: key too short for PSS')
	}
	m_hash := hash_fn(msg)
	mut m := []u8{cap: 8 + hlen + salt.len}
	for _ in 0 .. 8 {
		m << u8(0)
	}
	m << m_hash
	m << salt
	h := hash_fn(m)
	ps_len := em_len - hlen - salt.len - 2
	mut db := []u8{cap: em_len - hlen - 1}
	for _ in 0 .. ps_len {
		db << u8(0)
	}
	db << u8(0x01)
	db << salt
	mask := pss_mgf1(hash_fn, hlen, h, em_len - hlen - 1)!
	for i in 0 .. db.len {
		db[i] ^= mask[i]
	}
	db[0] &= u8(0xff >> (8 * em_len - em_bits))
	mut em := []u8{cap: em_len}
	em << db
	em << h
	em << u8(0xbc)
	return em
}

// sign_pss signs msg with EMSA-PSS (RFC 8017 §9.1). The salt is random
// with length equal to the hash length, matching TLS 1.3 (RFC 8446 §4.2.3).
pub fn (key PrivateKey) sign_pss(msg []u8, h Hash) ![]u8 {
	hlen := h.digest_len()
	em_bits := key.n.bit_len() - 1
	salt := rand.bytes(hlen)!
	em := pss_encode(h.hash_fn(), hlen, msg, em_bits, salt)!
	em_len := (em_bits + 7) / 8
	m := big.integer_from_bytes(em)
	s := m.big_mod_pow(key.d, key.n)!
	return int_to_fixed_bytes(s, em_len)
}

// verify_pss verifies an EMSA-PSS signature over msg. Only the
// salt-length == hash-length parameterization (the TLS 1.3 one) is
// accepted.
pub fn (key PublicKey) verify_pss(msg []u8, sig []u8, h Hash) ! {
	hash_fn := h.hash_fn()
	hlen := h.digest_len()
	em_bits := key.n.bit_len() - 1
	em_len := (em_bits + 7) / 8
	if sig.len != em_len || em_len < hlen + hlen + 2 {
		return error('rsa: bad PSS signature length')
	}
	s := big.integer_from_bytes(sig)
	m := s.mod_pow(key.e, key.n)
	em := int_to_fixed_bytes(m, em_len)!
	if em[em.len - 1] != 0xbc {
		return error('rsa: PSS verification failed')
	}
	masked_db := em[..em.len - hlen - 1]
	h_received := em[em.len - hlen - 1..em.len - 1].clone()
	top_bits := u8(0xff >> (8 * em_len - em_bits))
	if masked_db[0] & ~top_bits != 0 {
		return error('rsa: PSS verification failed')
	}
	mask := pss_mgf1(hash_fn, hlen, h_received, em_len - hlen - 1)!
	mut db := []u8{cap: masked_db.len}
	for i in 0 .. masked_db.len {
		db << (masked_db[i] ^ mask[i])
	}
	db[0] &= top_bits
	ps_len := em_len - hlen - hlen - 2
	for i in 0 .. ps_len {
		if db[i] != 0 {
			return error('rsa: PSS verification failed')
		}
	}
	if db[ps_len] != 0x01 {
		return error('rsa: PSS verification failed')
	}
	salt := db[ps_len + 1..].clone()
	expected := pss_encode(hash_fn, hlen, msg, em_bits, salt)!
	if subtle.constant_time_compare(em, expected) != 1 {
		return error('rsa: PSS verification failed')
	}
}
