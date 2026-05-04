// vtest build: has_modern_openssl?
module mldsa_test

import x.crypto.mldsa

fn test_mldsa_basic_sign_and_verify() ! {
	pv := mldsa.PrivateKey.generate(.ml_dsa_44)!
	pb := pv.public_key()
	msg := 'ml-dsa basic roundtrip'.bytes()
	sig := pv.sign(msg, deterministic: true)!
	assert sig.len > 0
	assert pb.verify(msg, sig)!
	mismatch := pb.verify('ml-dsa mismatch'.bytes(), sig)!
	assert !mismatch
}

fn test_mldsa_seed_and_raw_key_roundtrip() ! {
	kind := mldsa.Kind.ml_dsa_65
	pv := mldsa.PrivateKey.generate(kind)!
	seed := pv.seed()
	priv := pv.bytes()
	pub_bytes := pv.public_key().bytes()
	assert seed.len == 32
	assert priv.len == kind.private_key_size()
	assert pub_bytes.len == kind.public_key_size()

	pv_from_seed := mldsa.PrivateKey.from_seed(seed, kind)!
	assert pv_from_seed.bytes() == priv

	pb := mldsa.PublicKey.from_bytes(pub_bytes, kind)!
	msg := 'ml-dsa imported public key'.bytes()
	sig := pv.sign(msg, deterministic: true)!
	assert pb.verify(msg, sig)!
}
