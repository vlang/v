module rsa

import crypto
import crypto.sha256
import crypto.sha512
import encoding.hex
import os

const testdata = os.join_path(os.dir(@FILE), 'testdata')

fn read_testdata(name string) string {
	return os.read_file(os.join_path(testdata, name)) or { panic(err) }
}

fn private_key(name string) PrivateKey {
	return PrivateKey.from_pem(read_testdata('${name}.pem')) or { panic(err) }
}

fn public_key(name string) PublicKey {
	return PublicKey.from_pem(read_testdata('${name}.pub.pem')) or { panic(err) }
}

fn digest(h crypto.Hash, data []u8) []u8 {
	return hash_sum(h, data) or { panic(err) }
}

fn hash_by_name(name string) crypto.Hash {
	return match name {
		'sha256' { crypto.Hash.sha256 }
		'sha384' { crypto.Hash.sha384 }
		else { crypto.Hash.sha512 }
	}
}

// The vectors were produced by OpenSSL with the keys of testdata/: PKCS #1
// v1.5 signatures, which are deterministic, must match byte for byte, and PSS
// signatures, salted with the digest length or the longest salt, must verify.
fn test_openssl_vectors() {
	message := read_testdata('message.txt').bytes()
	mut count := 0
	for line in read_testdata('vectors.txt').split_into_lines() {
		fields := line.split(' ')
		name, scheme, hash_name, salt, sig_hex := fields[0], fields[1], fields[2], fields[3], fields[4]
		h := hash_by_name(hash_name)
		sig := hex.decode(sig_hex)!
		priv := private_key(name)
		pub_key := public_key(name)
		assert priv.public_key().equal(pub_key)
		d := digest(h, message)
		if scheme == 'pkcs1v15' {
			assert priv.sign_pkcs1v15(h, d)! == sig, '${name} ${hash_name}'
			assert pub_key.verify_pkcs1v15(h, d, sig)!
		} else {
			if salt == 'digest' {
				assert pub_key.verify_pss(h, d, sig)!, '${name} ${hash_name}'
			} else {
				// The longest salt only verifies with automatic detection.
				assert !pub_key.verify_pss(h, d, sig)!
			}
			assert pub_key.verify_pss(h, d, sig, salt_length: pss_salt_length_auto)!
		}
		count++
	}
	assert count == 36
}

fn test_pss_round_trip_for_every_salt_mode() {
	for name in ['key2048', 'key2049', 'key3072'] {
		priv := private_key(name)
		pub_key := priv.public_key()
		for h in [crypto.Hash.sha256, .sha384, .sha512] {
			d := digest(h, 'round trip'.bytes())
			sig := priv.sign_pss(h, d)!
			assert sig.len == pub_key.size()
			assert pub_key.verify_pss(h, d, sig)!
			assert pub_key.verify_pss(h, d, sig, salt_length: hash_size(h)!)!
			longest := priv.sign_pss(h, d, salt_length: pss_salt_length_auto)!
			assert pub_key.verify_pss(h, d, longest, salt_length: pss_salt_length_auto)!
			assert !pub_key.verify_pss(h, d, longest)!
			empty := priv.sign_pss(h, d, salt_length: 0)!
			assert pub_key.verify_pss(h, d, empty, salt_length: 0)!
			// A random salt makes each signature different.
			assert priv.sign_pss(h, d)! != sig
		}
	}
}

fn test_tampered_signatures_and_messages_are_rejected() {
	priv := private_key('key2048')
	pub_key := priv.public_key()
	d := sha256.sum256('original'.bytes())
	other := sha256.sum256('modified'.bytes())
	for pss in [false, true] {
		sig := if pss { priv.sign_pss(.sha256, d)! } else { priv.sign_pkcs1v15(.sha256, d)! }
		verify := fn [pub_key, pss] (digest []u8, sig []u8) bool {
			if pss {
				return pub_key.verify_pss(.sha256, digest, sig) or { panic(err) }
			}
			return pub_key.verify_pkcs1v15(.sha256, digest, sig) or { panic(err) }
		}
		assert verify(d, sig)
		assert !verify(other, sig)
		for i in [0, 1, sig.len / 2, sig.len - 1] {
			mut bad := sig.clone()
			bad[i] ^= 0x01
			assert !verify(d, bad)
		}
		assert !verify(d, sig[1..])
		mut longer := [u8(0)]
		longer << sig
		assert !verify(d, longer)
		// A signature equal to or above the modulus is out of range.
		assert !verify(d, pub_key.modulus())
		assert !verify(d, []u8{len: sig.len, init: 0xff})
	}
	// A signature from another key does not verify.
	sig3072 := private_key('key3072').sign_pkcs1v15(.sha256, d)!
	assert !pub_key.verify_pkcs1v15(.sha256, d, sig3072[..pub_key.size()])!
}

fn test_hash_mismatch_and_bad_digests() {
	priv := private_key('key2048')
	pub_key := priv.public_key()
	d256 := sha256.sum256('x'.bytes())
	d384 := sha512.sum384('x'.bytes())
	sig := priv.sign_pkcs1v15(.sha256, d256)!
	// The DigestInfo binds the hash: a SHA-384 check of a SHA-256 signature fails.
	assert !pub_key.verify_pkcs1v15(.sha384, d384, sig)!
	if _ := priv.sign_pkcs1v15(.sha256, d384) {
		assert false, 'a digest of the wrong length must be refused'
	}
	if _ := priv.sign_pkcs1v15(.sha1, []u8{len: 20}) {
		assert false, 'SHA-1 is not supported'
	}
	if _ := pub_key.verify_pss(.md5, []u8{len: 16}, sig) {
		assert false, 'MD5 is not supported'
	}
	if _ := priv.sign_pss(.sha256, d256, salt_length: -3) {
		assert false, 'invalid salt length'
	}
	// The salt must fit next to the digest in the encoded message.
	d512 := sha512.sum512('x'.bytes())
	if _ := priv.sign_pss(.sha512, d512, salt_length: 200) {
		assert false, 'salt too long for the key'
	}
	if _ := priv.sign_pss(.sha512, d512, salt_length: max_int) {
		assert false, 'salt length overflow'
	}
	assert !pub_key.verify_pss(.sha512, d512, sig, salt_length: max_int)!
}

fn test_key_formats_round_trip() {
	pkcs8 := private_key('key2048')
	pkcs1 := PrivateKey.from_pem(read_testdata('key2048.pkcs1.pem'))!
	assert pkcs8.equal(pkcs1)
	spki := public_key('key2048')
	pkcs1_pub := PublicKey.from_pem(read_testdata('key2048.pkcs1.pub.pem'))!
	assert spki.equal(pkcs1_pub)
	// The DER we write is the DER OpenSSL writes.
	assert pkcs8.to_pem() == read_testdata('key2048.pem')
	assert spki.to_pem() == read_testdata('key2048.pub.pem')
	assert pkcs8.to_pkcs1_der() == PrivateKey.from_pem(read_testdata('key2048.pkcs1.pem'))!.to_pkcs1_der()
	assert PrivateKey.from_der(pkcs8.to_pkcs1_der())!.equal(pkcs8)
	assert PublicKey.from_der(spki.to_pkcs1_der())!.equal(spki)
	assert PrivateKey.new(pkcs8.components())!.equal(pkcs8)
	// Components without the CRT values are completed.
	c := pkcs8.components()
	partial := PrivateKey.new(n: c.n, e: c.e, d: c.d, p: c.p, q: c.q)!
	assert partial.components() == c
	assert spki.bits() == 2048 && spki.size() == 256 && spki.exponent() == 65537
	assert public_key('key2049').bits() == 2049
	assert public_key('key2049').size() == 257
}

// A PKCS #8 version 1 key (RFC 5958) may carry its public key, which must match.
fn test_pkcs8_v2_with_public_key() {
	key := private_key('key2048')
	build := fn [key] (public_key []u8) []u8 {
		mut contents := der_integer([u8(1)])
		contents << der_rsa_algorithm()
		contents << der_element(tag_octet_string, key.to_pkcs1_der())
		mut bits := [u8(0)]
		bits << public_key
		contents << der_element(0x81, bits)
		return der_element(tag_sequence, contents)
	}
	assert PrivateKey.from_der(build(key.public_key().to_pkcs1_der()))!.equal(key)
	other := private_key('key3072').public_key().to_pkcs1_der()
	if _ := PrivateKey.from_der(build(other)) {
		assert false, 'accepted a mismatched public key'
	}
}

fn test_invalid_keys_are_rejected() {
	c := private_key('key2048').components()
	mut bad_d := c.d.clone()
	bad_d[bad_d.len - 1] ^= 0x02
	if _ := PrivateKey.new(PrivateKeyComponents{ ...c, d: bad_d }) {
		assert false, 'a wrong private exponent must be refused'
	}
	if _ := PrivateKey.new(PrivateKeyComponents{ ...c, p: c.q, q: c.p, dp: c.dq, dq: c.dp, qinv: [] }) {
		// Swapping the primes gives another valid CRT form, as long as qinv
		// is recomputed.
	} else {
		assert false
	}
	if _ := PrivateKey.new(PrivateKeyComponents{ ...c, qinv: c.dp }) {
		assert false, 'a wrong qinv must be refused'
	}
	if _ := PrivateKey.new(PrivateKeyComponents{ ...c, p: [] }) {
		assert false, 'the primes are required'
	}
	if _ := PublicKey.new(c.n, 65536) {
		assert false, 'even exponent'
	}
	if _ := PublicKey.new(c.n, 1) {
		assert false, 'exponent 1'
	}
	if _ := PublicKey.new(c.n[..128], 65537) {
		assert false, '1024-bit keys are below the minimum'
	}
	mut even := c.n.clone()
	even[even.len - 1] &= 0xfe
	if _ := PublicKey.new(even, 65537) {
		assert false, 'even modulus'
	}
	der := public_key('key2048').to_der()
	mut trailing := der.clone()
	trailing << 0
	if _ := PublicKey.from_der(trailing) {
		assert false, 'trailing data'
	}
	if _ := PublicKey.from_der(der[..der.len - 1]) {
		assert false, 'truncated data'
	}
	if _ := PublicKey.from_pem(read_testdata('key2048.pem')) {
		assert false, 'a private key PEM block is not a public key'
	}
	if _ := PrivateKey.from_pem('-----BEGIN ENCRYPTED PRIVATE KEY-----\nAAAA\n-----END ENCRYPTED PRIVATE KEY-----\n') {
		assert false, 'encrypted keys are not supported'
	}
}

fn test_generate_key() {
	priv := generate_key(2048)!
	pub_key := priv.public_key()
	assert pub_key.bits() == 2048
	assert pub_key.exponent() == 65537
	d := sha256.sum256('generated'.bytes())
	assert pub_key.verify_pkcs1v15(.sha256, d, priv.sign_pkcs1v15(.sha256, d)!)!
	assert pub_key.verify_pss(.sha256, d, priv.sign_pss(.sha256, d)!)!
	assert PrivateKey.from_pem(priv.to_pem())!.equal(priv)
	assert PublicKey.from_pem(pub_key.to_pem())!.equal(pub_key)
	if _ := generate_key(1024) {
		assert false, '1024-bit keys are below the minimum'
	}
}
