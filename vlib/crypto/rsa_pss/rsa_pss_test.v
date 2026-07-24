// A NIST CAVP known-answer test (independent oracle) is intentionally NOT
// included here yet, for the same reason noted in
// vlib/crypto/ecdsa/ecdsa_p256_ecdh_test.v: no unverified expected value
// should be checked in. Tracked as part of Phase 2's from-scratch TLS 1.3
// test vector suite (vlib/net/quic/testdata/tls13_vectors/), which will need
// real RSA-PSS CertificateVerify signatures from captured traffic anyway.
module rsa_pss

fn test_rsa_pss_sign_verify_round_trip_sha256() {
	mut priv := PrivateKey.new(bits: 2048)!
	defer {
		priv.free()
	}
	pub_key := priv.public_key()

	message := 'TLS 1.3 CertificateVerify test message'.bytes()
	sig := priv.sign(message, .sha256)!
	assert sig.len > 0

	ok := pub_key.verify(message, sig, .sha256)!
	assert ok
}

fn test_rsa_pss_sign_verify_round_trip_sha384_and_sha512() {
	mut priv := PrivateKey.new(bits: 2048)!
	defer {
		priv.free()
	}
	pub_key := priv.public_key()
	message := 'another message'.bytes()

	sig384 := priv.sign(message, .sha384)!
	assert pub_key.verify(message, sig384, .sha384)!

	sig512 := priv.sign(message, .sha512)!
	assert pub_key.verify(message, sig512, .sha512)!
}

fn test_rsa_pss_verify_rejects_tampered_message() {
	mut priv := PrivateKey.new(bits: 2048)!
	defer {
		priv.free()
	}
	pub_key := priv.public_key()

	message := 'original message'.bytes()
	sig := priv.sign(message, .sha256)!

	tampered := 'tampered message'.bytes()
	ok := pub_key.verify(tampered, sig, .sha256)!
	assert !ok
}

fn test_rsa_pss_verify_rejects_tampered_signature() {
	mut priv := PrivateKey.new(bits: 2048)!
	defer {
		priv.free()
	}
	pub_key := priv.public_key()

	message := 'a message'.bytes()
	mut sig := priv.sign(message, .sha256)!
	sig[0] ^= 0xFF

	ok := pub_key.verify(message, sig, .sha256)!
	assert !ok
}

fn test_rsa_pss_verify_rejects_wrong_key() {
	mut priv_a := PrivateKey.new(bits: 2048)!
	mut priv_b := PrivateKey.new(bits: 2048)!
	defer {
		priv_a.free()
		priv_b.free()
	}
	pub_b := priv_b.public_key()

	message := 'a message'.bytes()
	sig := priv_a.sign(message, .sha256)!

	ok := pub_b.verify(message, sig, .sha256)!
	assert !ok
}

fn test_rsa_pss_hash_scheme_mismatch_fails_verification() {
	// A signature made with one hash scheme must not verify under a
	// different one — TLS 1.3's rsa_pss_rsae_sha256/384/512 are distinct
	// SignatureScheme values and must not be treated as interchangeable.
	mut priv := PrivateKey.new(bits: 2048)!
	defer {
		priv.free()
	}
	pub_key := priv.public_key()
	message := 'a message'.bytes()

	sig := priv.sign(message, .sha256)!
	ok := pub_key.verify(message, sig, .sha384)!
	assert !ok
}
