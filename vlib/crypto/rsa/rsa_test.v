module rsa

import crypto.sha256
import crypto.x509
import math.big
import net.tls
import time

// textbook_key builds the classic RSA example (p=61, q=53, e=17) as a
// deterministic KAT fixture.
fn textbook_key() PrivateKey {
	return PrivateKey{
		n:    big.integer_from_int(3233)
		e:    u64(17)
		d:    big.integer_from_int(2753)
		p:    big.integer_from_int(61)
		q:    big.integer_from_int(53)
		dp:   big.integer_from_int(53)
		dq:   big.integer_from_int(49)
		qinv: big.integer_from_int(38)
	}
}

fn test_rsa_textbook_kat() {
	key := textbook_key()
	key.validate() or { panic('validate: ${err}') }
	nb, _ := key.n.bytes()
	assert nb == [u8(0x0c), 0xa1]
	// RFC 8017 textbook vector: 65^17 mod 3233 == 2790.
	m := big.integer_from_int(65)
	c := key.public_key().encrypt(m) or { panic('encrypt: ${err}') }
	assert c == big.integer_from_int(2790)
	assert key.decrypt(c) or { panic('decrypt: ${err}') } == m
	// Out-of-range representatives are rejected.
	if _ := key.public_key().encrypt(key.n) {
		assert false, 'encrypt(n) must fail'
	}
	if _ := key.decrypt(key.n) {
		assert false, 'decrypt(n) must fail'
	}
	// Full DER roundtrips preserve every field.
	pkcs1 := key.to_pkcs1_der()
	back := private_key_from_pkcs1_der(pkcs1) or { panic('pkcs1 parse: ${err}') }
	assert back.n == key.n
	assert back.e == key.e
	assert back.d == key.d
	assert back.p == key.p
	assert back.q == key.q
	assert back.dp == key.dp
	assert back.dq == key.dq
	assert back.qinv == key.qinv
	back.validate() or { panic('validate roundtrip: ${err}') }
	pkcs8 := key.to_pkcs8_der()
	back8 := private_key_from_pkcs8_der(pkcs8) or { panic('pkcs8 parse: ${err}') }
	assert back8.n == key.n && back8.d == key.d
	spki := key.public_key().to_spki_der()
	pub_back := public_key_from_spki_der(spki) or { panic('spki parse: ${err}') }
	assert pub_back.n == key.n && pub_back.e == key.e
	pub1 := public_key_from_pkcs1_der(key.public_key().to_pkcs1_der()) or {
		panic('pub parse: ${err}')
	}
	assert pub1.n == key.n && pub1.e == key.e
	// PEM roundtrips.
	priv_pem := key.to_pem_pkcs8() or { panic('pem enc: ${err}') }
	priv_back := private_key_from_pem(priv_pem) or { panic('pem parse: ${err}') }
	assert priv_back.n == key.n && priv_back.d == key.d
	priv_pem1 := key.to_pem_pkcs1() or { panic('pem1 enc: ${err}') }
	priv_back1 := private_key_from_pem(priv_pem1) or { panic('pem1 parse: ${err}') }
	assert priv_back1.n == key.n && priv_back1.d == key.d
	pub_pem := key.public_key().to_pem_spki() or { panic('pub pem enc: ${err}') }
	pub_pem_back := public_key_from_pem(pub_pem) or { panic('pub pem parse: ${err}') }
	assert pub_pem_back.n == key.n && pub_pem_back.e == key.e
	// Corrupt DER is rejected.
	mut bad := pkcs1.clone()
	bad[bad.len - 1] ^= 0xff
	if _ := private_key_from_pkcs1_der(bad) {
		assert false, 'corrupt DER must fail'
	}
	if _ := private_key_from_pkcs1_der(pkcs1[..pkcs1.len - 5]) {
		assert false, 'truncated DER must fail'
	}
	// validate catches corrupted numbers: n != p*q and e*d != 1 both fail.
	bad_n := PrivateKey{
		n:    key.p
		e:    key.e
		d:    key.d
		p:    key.p
		q:    key.q
		dp:   key.dp
		dq:   key.dq
		qinv: key.qinv
	}
	if _ := bad_n.validate() {
		assert false, 'validate(n != p*q) must fail'
	}
	bad_d := PrivateKey{
		n:    key.n
		e:    key.e
		d:    key.p
		p:    key.p
		q:    key.q
		dp:   key.dp
		dq:   key.dq
		qinv: key.qinv
	}
	if _ := bad_d.validate() {
		assert false, 'validate(e*d != 1) must fail'
	}
}

fn test_rsa_invalid_params() {
	if _ := generate_key(bits: 512) {
		assert false, 'small key must fail'
	}
	if _ := generate_key(bits: 1024, e: 4) {
		assert false, 'even exponent must fail'
	}
	if _ := generate_key(bits: 1024, e: 65536) {
		assert false, 'even exponent must fail'
	}
}

fn test_rsa_keygen_1024_and_roundtrips() {
	key := generate_key(bits: 1024) or { panic('keygen: ${err}') }
	assert key.n.bit_len() == 1024
	assert key.e == 65537
	key.validate() or { panic('validate: ${err}') }
	pk := key.public_key()

	// Raw encrypt/decrypt roundtrip.
	m := big.integer_from_bytes([u8(0xde), 0xad, 0xbe, 0xef, 0x42])
	c := pk.encrypt(m) or { panic('encrypt: ${err}') }
	assert key.decrypt(c) or { panic('decrypt: ${err}') } == m

	// PKCS#1 v1.5 roundtrip, plus tamper rejection.
	msg := 'rsa pkcs1 v1.5 roundtrip'.bytes()
	digest := sha256.sum256(msg)
	sig := key.sign_pkcs1_v15(digest, .sha256) or { panic('sign: ${err}') }
	pk.verify_pkcs1_v15(digest, sig, .sha256) or { panic('verify: ${err}') }
	mut tampered := sig.clone()
	tampered[0] ^= 0x01
	if _ := pk.verify_pkcs1_v15(digest, tampered, .sha256) {
		assert false, 'tampered signature must fail'
	}
	// Cross-compatibility with net.tls in both directions.
	tls.rsa_pkcs1_verify_digest(digest, sig, key.n, key.e, tls.RsaHash.sha256) or {
		panic('tls verify of rsa sig: ${err}')
	}
	tls_sig := tls.rsa_pkcs1_sign_digest(digest, key.d, key.n, tls.RsaHash.sha256) or {
		panic('tls sign: ${err}')
	}
	pk.verify_pkcs1_v15(digest, tls_sig, .sha256) or { panic('verify of tls sig: ${err}') }

	// PSS roundtrip (SHA-256 and SHA-384 fit a 1024-bit key with
	// full-length salt; SHA-512 is covered with a 2048-bit key below).
	for h in [Hash.sha256, Hash.sha384] {
		pss_sig := key.sign_pss(msg, h) or { panic('pss sign: ${err}') }
		pk.verify_pss(msg, pss_sig, h) or { panic('pss verify: ${err}') }
		mut bad_pss := pss_sig.clone()
		bad_pss[bad_pss.len - 2] ^= 0x80
		if _ := pk.verify_pss(msg, bad_pss, h) {
			assert false, 'tampered PSS must fail'
		}
	}
	scheme := u16(0x0804)
	rsa_pss_sig := key.sign_pss(msg, .sha256) or { panic('pss sign: ${err}') }
	tls.rsa_pss_verify(scheme, msg, rsa_pss_sig, key.e, key.n) or {
		panic('tls verify of rsa PSS: ${err}')
	}
	tls_pss_sig := tls.rsa_pss_sign(scheme, msg, key.d, key.n) or {
		panic('tls pss sign: ${err}')
	}
	pk.verify_pss(msg, tls_pss_sig, .sha256) or { panic('verify of tls PSS: ${err}') }

	// DER roundtrips and net.tls private-key parser compatibility.
	pkcs1 := key.to_pkcs1_der()
	back := private_key_from_pkcs1_der(pkcs1) or { panic('pkcs1 parse: ${err}') }
	assert back.n == key.n && back.e == key.e && back.d == key.d
	pkcs8 := key.to_pkcs8_der()
	back8 := private_key_from_pkcs8_der(pkcs8) or { panic('pkcs8 parse: ${err}') }
	assert back8.n == key.n && back8.d == key.d
	tls_priv := tls.parse_rsa_private_key(pkcs8) or { panic('tls pkcs8 parse: ${err}') }
	assert tls_priv.n == key.n && tls_priv.e == key.e && tls_priv.d == key.d
	tls_priv1 := tls.parse_rsa_private_key(pkcs1) or { panic('tls pkcs1 parse: ${err}') }
	assert tls_priv1.n == key.n && tls_priv1.d == key.d
	spki := pk.to_spki_der()
	pub_back := public_key_from_spki_der(spki) or { panic('spki parse: ${err}') }
	assert pub_back.n == key.n && pub_back.e == key.e

	// PEM roundtrips.
	priv_back := private_key_from_pem(key.to_pem_pkcs8() or { panic('pem: ${err}') }) or {
		panic('pem parse: ${err}')
	}
	assert priv_back.n == key.n && priv_back.d == key.d
	pub_pem_back := public_key_from_pem(pk.to_pem_spki() or { panic('pub pem: ${err}') }) or {
		panic('pub pem parse: ${err}')
	}
	assert pub_pem_back.n == key.n && pub_pem_back.e == key.e
}

fn test_rsa_pss_sha512_2048() {
	key := generate_key(bits: 2048) or { panic('keygen: ${err}') }
	assert key.n.bit_len() == 2048
	msg := 'rsa pss sha512 roundtrip'.bytes()
	sig := key.sign_pss(msg, .sha512) or { panic('pss sign: ${err}') }
	key.public_key().verify_pss(msg, sig, .sha512) or { panic('pss verify: ${err}') }
	scheme := u16(0x0806)
	tls.rsa_pss_verify(scheme, msg, sig, key.e, key.n) or {
		panic('tls verify of rsa PSS: ${err}')
	}
	tls_sig := tls.rsa_pss_sign(scheme, msg, key.d, key.n) or {
		panic('tls pss sign: ${err}')
	}
	key.public_key().verify_pss(msg, tls_sig, .sha512) or {
		panic('verify of tls PSS: ${err}')
	}
	// A damaged PSS-SHA512 signature is rejected on both sides.
	mut bad_sig := sig.clone()
	bad_sig[0] ^= 0x01
	if _ := key.public_key().verify_pss(msg, bad_sig, .sha512) {
		assert false, 'tampered PSS-SHA512 must fail'
	}
	if _ := tls.rsa_pss_verify(scheme, msg, bad_sig, key.e, key.n) {
		assert false, 'tampered tls PSS-SHA512 must fail'
	}
}

fn test_rsa_self_signed_cert() {
	key := generate_key(bits: 1024) or { panic('keygen: ${err}') }
	now := time.now()
	opts := x509.CertGenOptions{
		common_name: 'rsa.example.com'
		dns_names:   ['rsa.example.com', 'www.example.com']
		not_before:  now
		not_after:   now.add_days(365)
	}
	cert, der := generate_self_signed(opts, key) or { panic('cert gen: ${err}') }
	assert der.len > 0
	cert.check_host('rsa.example.com') or { panic('host check: ${err}') }
	cert.check_host('www.example.com') or { panic('SAN check: ${err}') }
	// The SPKI must expose the generated RSA public key.
	spki_pk := cert.public_key() or { panic('pubkey: ${err}') }
	match spki_pk {
		x509.RsaPublicKey {
			assert spki_pk.der == key.public_key().to_pkcs1_der()
		}
		else {
			panic('expected RSA public key')
		}
	}
	// The self-signature verifies with net.tls against our public key.
	tls.verify_cert_signature_rsa(der, key.public_key().to_pkcs1_der()) or {
		panic('cert sig verify: ${err}')
	}
	// A damaged signature is rejected.
	mut bad_der := der.clone()
	bad_der[bad_der.len - 10] ^= 0xff
	if _ := tls.verify_cert_signature_rsa(bad_der, key.public_key().to_pkcs1_der()) {
		assert false, 'tampered cert must fail'
	}
	if _ := cert.check_host('other.example.com') {
		assert false, 'non-SAN name must fail'
	}
}
