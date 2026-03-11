// vtest build: has_modern_openssl?
module mldsa_test

import x.crypto.mldsa

fn test_mldsa_basic_sign_and_verify() ! {
	mut pv := mldsa.PrivateKey.new()!
	defer {
		pv.free()
	}
	mut pb := pv.public_key()!
	defer {
		pb.free()
	}
	msg := 'ml-dsa basic roundtrip'.bytes()
	sig := pv.sign(msg)!
	assert sig.len > 0
	assert pv.verify(sig, msg)!
	assert pb.verify(sig, msg)!
	assert !pb.verify(sig, 'ml-dsa mismatch'.bytes())!
}

fn test_mldsa_seed_and_raw_key_roundtrip() ! {
	kind := mldsa.Kind.ml_dsa_65
	mut pv := mldsa.PrivateKey.new(kind: kind)!
	defer {
		pv.free()
	}
	seed := pv.seed()!
	priv := pv.bytes()!
	pub_bytes := pv.public_bytes()!
	assert seed.len == 32
	assert priv.len == kind.private_key_size()
	assert pub_bytes.len == kind.public_key_size()

	mut pv_from_seed := mldsa.PrivateKey.from_seed(seed, kind)!
	defer {
		pv_from_seed.free()
	}
	assert pv_from_seed.bytes()! == priv

	mut pb := mldsa.PublicKey.from_bytes(pub_bytes, kind)!
	defer {
		pb.free()
	}
	msg := 'ml-dsa imported public key'.bytes()
	sig := pv.sign(msg, deterministic: 1)!
	assert pb.verify(sig, msg)!
}
