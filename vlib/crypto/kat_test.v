module crypto

// Cross-module RFC known-answer tests (KATs).
//
// Every vector below is embedded: no network access, no downloads, and a
// failure is always a real regression (tests skip nothing). Sources:
//   SHA-256 / SHA-224  FIPS 180-2 appendices B.1/B.2 (also pinned in
//                      hexhash_vectors_test.v)
//   SHA-384/512 family repo-pinned digests from sha512_test.v
//   HMAC-SHA-256/SHA-1 RFC 2202 section 3 test cases 1-3
//   HKDF-SHA-256        RFC 5869 appendix A test cases 1 and 3
//   AES-128/256-GCM     NIST SP 800-38D worked examples (see aes_gcm_test.v)
//   ChaCha20-Poly1305   RFC 8439 section 2.8.2 (also in
//                      chacha20poly1305_test.v)
//   X25519              RFC 7748 section 6.1 (Alice/Bob) and section 5.2
//   ECDSA P-256         determinism is the RFC 6979 property (mbedTLS
//                      deterministic-nonce path); the oracle vector below is
//                      the OpenSSL-backend cross-implementation vector from
//                      ecdsa_notd_use_openssl_test.v
//   RSA PKCS#1 v1.5     RFC 8017: the section 5.1.1 textbook RSAEP example
//                      (65^17 mod 3233 = 2790) plus a fixed 1024-bit key whose
//                      signature over 'assurance KAT message' was produced by
//                      crypto.rsa itself (PKCS#1 v1.5 is deterministic, so the
//                      pair is a stable known answer)
//   FFDHE               textbook finite-field DH (p=23, g=5) worked example;
//                      the TLS stack negotiates ECDHE-only, so this pins the
//                      underlying math.big mod_pow primitive instead
import crypto.aes
import crypto.ecdsa
import crypto.hkdf
import crypto.hmac
import crypto.rsa
import crypto.sha1
import crypto.sha256
import crypto.sha512
import crypto.subtle
import encoding.hex
import math.big
import x.crypto.chacha20
import x.crypto.chacha20poly1305
import x.crypto.curve25519

fn test_kat_sha256_fips180() {
	assert sha256.sum256([]u8{}).hex() == 'e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855'
	assert sha256.sum256('abc'.bytes()).hex() == 'ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad'
	assert sha256.sum224('abc'.bytes()).hex() == '23097d223405d8228642a477bda255b32aadbce4bda0b3f7e36c9da7'
}

fn test_kat_sha512_family_pinned() {
	assert sha512.sum512('This is a sha512 checksum.'.bytes()).hex() == '4143e55fcba7e39b20f62a1368e5eb28f64a8859458886117ac66027832e0f9f5263daec688c439d2d0fa07059334668d39e59543039703dbb7e03ec9da7f8d7'
	assert sha512.sum384('example bytes'.bytes()).hex() == '8004e0038985a2d4dc40802b149f02cdd5868eaa58d87fae02f0cce2a3b566a6af63e34b11d5ba88c6035b96e587a6d6'
	assert sha512.sum512_224('example bytes'.bytes()).hex() == '2bcbe17a1c3cb7b2b8b75c3118ed8525b6a4c505f2e59f3dc5dfe462'
	assert sha512.sum512_256('example bytes'.bytes()).hex() == '984512a8f874623cf1e2d5bd85c7d1240214163db0ebd0919922768f94879563'
}

fn test_kat_hmac_rfc2202() {
	// RFC 2202 3.1 (HMAC-SHA-256) test cases 1-3.
	key0 := []u8{len: 16, init: 0x0b}
	assert hmac.new(key0, 'Hi There'.bytes(), sha256.sum256, sha256.block_size).hex() == '492ce020fe2534a5789dc3848806c78f4f6711397f08e7e7a12ca5a4483c8aa6'
	assert hmac.new('Jefe'.bytes(), 'what do ya want for nothing?'.bytes(), sha256.sum256,
		sha256.block_size).hex() == '5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843'
	key2 := []u8{len: 16, init: 0xaa}
	data2 := []u8{len: 50, init: 0xdd}
	assert hmac.new(key2, data2, sha256.sum256, sha256.block_size).hex() == '7dda3cc169743a6484649f94f0eda0f9f2ff496a9733fb796ed5adb40a44c3c1'
	// RFC 2202 3.2 (HMAC-SHA-1) test case 1.
	assert hmac.new(key0, 'Hi There'.bytes(), sha1.sum, sha1.block_size).hex() == '675b0b3a1b4ddf4e124872da6c2f632bfed957e9'
	// equal() is the constant-time entry point used on verify paths.
	m1 := hmac.new(key0, 'Hi There'.bytes(), sha256.sum256, sha256.block_size)
	m2 := hmac.new(key0, 'Hi There'.bytes(), sha256.sum256, sha256.block_size)
	assert hmac.equal(m1, m2)
	assert !hmac.equal(m1, 'bogus'.bytes())
}

fn test_kat_hkdf_sha256_rfc5869() {
	// RFC 5869 appendix A.1, test case 1.
	ikm := hex.decode('0b'.repeat(22))!
	salt := hex.decode('000102030405060708090a0b0c')!
	info := hex.decode('f0f1f2f3f4f5f6f7f8f9')!.bytestr()
	prk := hkdf.extract(sha256.new, ikm, salt)!
	assert prk.hex() == '077709362c2e32df0ddc3f0dc47bba6390b6c73bb50f9c3122ec844ad7c2b3e5'
	okm := hkdf.key(sha256.new, ikm, salt, info, 42)!
	assert okm.hex() == '3cb25f25faacd57a90434f64d0362f2a2d2d0a90cf1a5a4c5db02d56ecc4c5bf34007208d5b887185865'
	assert hkdf.expand(sha256.new, prk, info, 42)! == okm
	// RFC 5869 appendix A.1, test case 3 (empty salt and info).
	prk3 := hkdf.extract(sha256.new, ikm, []u8{})!
	assert prk3.hex() == '19ef24a32c717b167f33a91d6f648bdf96596776afdb6377ac434c1c293ccb04'
	okm3 := hkdf.key(sha256.new, ikm, []u8{}, '', 42)!
	assert okm3.hex() == '8da4e775a563c18f715f802a063c5a31b8a11f5c5ee1879ec3454e5f3c738d2d9d201395faa4b61a96c8'
}

fn test_kat_hkdf_salt_flip_must_differ() {
	// Salt sensitivity: flipping one salt bit must change both the
	// extracted PRK and the expanded OKM.
	ikm := hex.decode('0b'.repeat(22))!
	salt := hex.decode('000102030405060708090a0b0c')!
	mut salt2 := salt.clone()
	salt2[0] ^= 0x01
	prk1 := hkdf.extract(sha256.new, ikm, salt)!
	prk2 := hkdf.extract(sha256.new, ikm, salt2)!
	assert prk1 != prk2, 'HKDF extract must be salt-sensitive'
	info := hex.decode('f0f1f2f3f4f5f6f7f8f9')!.bytestr()
	okm1 := hkdf.key(sha256.new, ikm, salt, info, 42)!
	okm2 := hkdf.key(sha256.new, ikm, salt2, info, 42)!
	assert okm1 != okm2, 'HKDF expand must be salt-sensitive'
	assert okm1.len == 42
	assert okm2.len == 42
}

fn test_kat_aes128_gcm_nist() {
	key := hex.decode('feffe9928665731c6d6a8f9467308308')!
	iv := hex.decode('cafebabefacedbaddecaf888')!
	pt := hex.decode('d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39')!
	ad := hex.decode('feedfacedeadbeeffeedfacedeadbeefabaddad2')!
	mut g := aes.new_aes_gcm(key)!
	out := g.encrypt(pt, iv, ad)!
	assert out[..out.len - 16].hex() == '42831ec2217774244b7221b784d0d49ce3aa212f2c02a4e035c17e2329aca12e21d514b25466931c7d8f6a5aac84aa051ba30b396a0aac973d58e091'
	assert out[out.len - 16..].hex() == '5bc94fbc3221a5db94fae95ae7121a47'
	assert g.decrypt(out, iv, ad)! == pt
	mut bad := out.clone()
	bad[0] ^= 0xff
	if _ := g.decrypt(bad, iv, ad) {
		assert false, 'tampered GCM ciphertext must fail authentication'
	}
}

fn test_kat_aes256_gcm_nist() {
	key := hex.decode('feffe9928665731c6d6a8f9467308308feffe9928665731c6d6a8f9467308308')!
	iv := hex.decode('cafebabefacedbaddecaf888')!
	pt := hex.decode('d9313225f88406e5a55909c5aff5269a86a7a9531534f7da2e4c303d8a318a721c3c0c95956809532fcf0e2449a6b525b16aedf5aa0de657ba637b39')!
	ad := hex.decode('feedfacedeadbeeffeedfacedeadbeefabaddad2')!
	mut g := aes.new_aes_gcm(key)!
	out := g.encrypt(pt, iv, ad)!
	assert out[..out.len - 16].hex() == '522dc1f099567d07f47f37a32a84427d643a8cdcbfe5c0c97598a2bd2555d1aa8cb08e48590dbb3da7b08b1056828838c5f61e6393ba7a0abcc9f662'
	assert out[out.len - 16..].hex() == '76fc6ece0f4e1768cddf8853bb2d551b'
	assert g.decrypt(out, iv, ad)! == pt
	mut bad256 := out.clone()
	bad256[0] ^= 0xff
	if _ := g.decrypt(bad256, iv, ad) {
		assert false, 'tampered AES-256-GCM ciphertext must fail authentication'
	}
}

fn test_kat_chacha20poly1305_rfc8439() {
	// RFC 8439 section 2.8.2.
	key := hex.decode('808182838485868788898a8b8c8d8e8f909192939495969798999a9b9c9d9e9f')!
	nonce := hex.decode('070000004041424344454647')!
	pt := "Ladies and Gentlemen of the class of '99: If I could offer you only one tip for the future, sunscreen would be it.".bytes()
	aad := hex.decode('50515253c0c1c2c3c4c5c6c7')!
	mut co := chacha20poly1305.new(key, nonce.len, chacha20.Options{})!
	ct := co.encrypt(pt, nonce, aad)!
	assert ct[..ct.len - co.overhead()].hex() == 'd31a8d34648e60db7b86afbc53ef7ec2a4aded51296e08fea9e2b5a736ee62d63dbea45e8ca9671282fafb69da92728b1a71de0a9e060b2905d6a5b67ecd3b3692ddbd7f2d778b8c9803aee328091b58fab324e4fad675945585808b4831d7bc3ff4def08e4b7a9de576d26586cec64b6116'
	assert ct[ct.len - co.overhead()..].hex() == '1ae10b594f09e26a7e902ecbd0600691'
	assert co.decrypt(ct, nonce, aad)! == pt
	mut bad := ct.clone()
	bad[bad.len - 1] ^= 0x01
	if _ := co.decrypt(bad, nonce, aad) {
		assert false, 'tampered ChaCha20-Poly1305 tag must fail verification'
	}
}

fn test_kat_x25519_rfc7748() {
	// RFC 7748 section 6.1: Alice and Bob.
	a := hex.decode('77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a')!
	a_pbk := hex.decode('8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a')!
	b := hex.decode('5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb')!
	b_pbk := hex.decode('de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f')!
	k := hex.decode('4a5d9d5ba4ce2de1728e3bf480350f25e07e21c947d19e3376f09b3c1e161742')!
	mut a_pv := curve25519.PrivateKey.new_from_seed(a)!
	assert a_pv.public_key()!.bytes()! == a_pbk
	mut b_pv := curve25519.PrivateKey.new_from_seed(b)!
	b_pub := b_pv.public_key()!
	assert b_pub.bytes()! == b_pbk
	assert curve25519.derive_shared_secret(mut a_pv, b_pub)! == k
	// RFC 7748 section 5.2 vector.
	mut scalar := hex.decode('a546e36bf0527c9d3b16154b82465edd62144c0ac1fc5a18506a2244ba449ac4')!
	base := hex.decode('e6db6867583030db3594c1a424b15f7c726624ec26b3353b10a903a6d0ab1c4c')!
	assert curve25519.x25519(mut scalar, base)!.hex() == 'c3da55379de9c6908e94ea4df28d084f32eccf03491c71f754b4075577a28552'
}

fn test_kat_ecdsa_p256_oracle() {
	// Cross-implementation oracle: produced by the independent OpenSSL
	// backend, verified here by the default mbedTLS backend.
	pubkey_bytes := [u8(0x04), 0x01, 0xcb, 0x89, 0x0e, 0x0e, 0x7f, 0x4c, 0x80, 0x55, 0x8f, 0x6a,
		0x4f, 0xa1, 0x04, 0xbc, 0x69, 0x13, 0x40, 0x67, 0xde, 0xf0, 0x72, 0x01, 0xd3, 0x7f, 0x57,
		0x21, 0xc1, 0xfa, 0xc7, 0x03, 0xc4, 0xe2, 0xdb, 0x92, 0xb1, 0x37, 0xd3, 0x19, 0x4c, 0x3c,
		0x7d, 0x81, 0x5a, 0x81, 0xe6, 0x68, 0x14, 0xd7, 0xea, 0x3f, 0x6c, 0xe9, 0xf1, 0xd6, 0x4d,
		0xcc, 0xa4, 0x76, 0xd8, 0xe4, 0xc6, 0xd1, 0x4b]
	message := 'crypto.ecdsa mbedTLS-vs-OpenSSL cross-implementation oracle vector'.bytes()
	sig := [u8(0x30), 0x45, 0x02, 0x20, 0x73, 0xca, 0x75, 0x5b, 0x45, 0xa5, 0x1f, 0x65, 0x8c, 0xf5,
		0xdb, 0x04, 0x30, 0xdb, 0xa5, 0x6b, 0xa7, 0x7d, 0xf5, 0xb0, 0x03, 0x8c, 0x02, 0xff, 0xe9,
		0x58, 0x3a, 0x3c, 0x17, 0x2b, 0x82, 0xe9, 0x02, 0x21, 0x00, 0x99, 0x11, 0xfb, 0x81, 0x0b,
		0x1f, 0xc4, 0xf2, 0xa6, 0xef, 0x39, 0x9f, 0x5b, 0x4d, 0x8d, 0x4a, 0x18, 0x88, 0xd6, 0xe9,
		0xc9, 0x23, 0x26, 0x1a, 0x0f, 0xad, 0x1a, 0x51, 0x59, 0x17, 0x9a, 0x37]
	pb := ecdsa.PublicKey.from_uncompressed_bytes(pubkey_bytes)!
	assert pb.verify(message, sig)!
	mut bad := sig.clone()
	bad[bad.len - 2] ^= 0x80
	assert !(pb.verify(message, bad) or { false })
}

fn test_kat_ecdsa_p256_deterministic_rfc6979() ! {
	// RFC 6979 property: the same key and message always produce the same
	// signature (mbedTLS deterministic-nonce path), and it verifies.
	seed := []u8{len: 32, init: 1}
	mut pv := ecdsa.new_key_from_seed(seed)!
	defer {
		pv.free()
	}
	msg := 'assurance ECDSA determinism message'.bytes()
	sig1 := pv.sign(msg)!
	sig2 := pv.sign(msg)!
	assert sig1 == sig2
	pb := pv.public_key()!
	defer {
		pb.free()
	}
	assert pb.verify(msg, sig1)!
	mut bad := sig1.clone()
	bad[bad.len - 1] ^= 0x01
	if pb.verify(msg, bad)! {
		assert false, 'mutated ECDSA signature must not verify'
	}
}

fn test_kat_rsa_textbook_rfc8017() {
	// RFC 8017 section 5.1 RSAEP textbook example: 65^17 mod 3233 = 2790.
	key := rsa.PrivateKey{
		n:    big.integer_from_int(3233)
		e:    u64(17)
		d:    big.integer_from_int(2753)
		p:    big.integer_from_int(61)
		q:    big.integer_from_int(53)
		dp:   big.integer_from_int(53)
		dq:   big.integer_from_int(49)
		qinv: big.integer_from_int(38)
	}
	key.validate()!
	m := big.integer_from_int(65)
	c := key.public_key().encrypt(m)!
	assert c == big.integer_from_int(2790)
	assert key.decrypt(c)! == m
	if _ := key.public_key().encrypt(key.n) {
		assert false, 'encrypt(n) must fail'
	}
}

fn test_kat_rsa_pkcs1_v15_fixed_1024() {
	// Fixed 1024-bit key (e=65537); signature over 'assurance KAT message'
	// (SHA-256) produced by crypto.rsa itself. PKCS#1 v1.5 signing is
	// deterministic, so this pair is a stable known answer.
	pub_der := hex.decode('30818902818100c3949ae5815f3feb91de6520701093c870fd657d3ba7c3dfd29fe5ebb019b3bf847a40b9a9707582d2a22b7429ec690cc77f9239984cc8685018a90fed9cf200aa678288f0bbb9d5322b3251fb37b260fcd36cc963c5d9b3955ae1676f7ee3deb05578ccf4988da48da16d197ddc612c7ef58e90063a32cfe666eaf6ef05cd630203010001')!
	pubkey := rsa.public_key_from_pkcs1_der(pub_der)!
	digest := hex.decode('f2b8d3ebee1863fd7e942275e546a2ca9ca771172318149dff3b5550b78488a8')!
	sig := hex.decode('b66fa3e1fe139bc9f66c3b087c9825065a710d01991fd1e787e75c74b645314edfebed4126f86938051856228157225d37a643ff452d5d243e0af11c9f7fdc5fee23135ade33a30957b4ebc9bce03278ba3b4568e2d1caaa75ff5d7cb9c561a4eb113abc97d9796ace5e0b45302cd3f782463fd32d592d40708aaece4b0c1d49')!
	assert sig.len == 128
	pubkey.verify_pkcs1_v15(digest, sig, .sha256)!
	mut bad := sig.clone()
	bad[10] ^= 0xff
	if _ := pubkey.verify_pkcs1_v15(digest, bad, .sha256) {
		assert false, 'mutated PKCS#1 v1.5 signature must fail'
	}
	wrong := sha256.sum256('wrong message'.bytes())
	if _ := pubkey.verify_pkcs1_v15(wrong, sig, .sha256) {
		assert false, 'signature over another digest must fail'
	}
}

fn test_kat_ffdhe_consistency_textbook() {
	// Textbook finite-field DH (RFC 3526-style toy group p=23, g=5):
	// a=6 -> A=8, b=15 -> B=19, shared=2 on both sides.
	p := big.integer_from_int(23)
	g := big.integer_from_int(5)
	a_pub := g.mod_pow(6, p)
	b_pub := g.mod_pow(15, p)
	assert a_pub == big.integer_from_int(8)
	assert b_pub == big.integer_from_int(19)
	assert b_pub.mod_pow(6, p) == big.integer_from_int(2)
	assert a_pub.mod_pow(15, p) == big.integer_from_int(2)
}

fn test_kat_subtle_compare() {
	assert subtle.constant_time_compare([u8(1), 2, 3], [u8(1), 2, 3]) == 1
	assert subtle.constant_time_compare([u8(1), 2, 3], [u8(1), 2, 4]) == 0
	assert subtle.constant_time_compare([u8(1), 2], [u8(1), 2, 3]) == 0
}
