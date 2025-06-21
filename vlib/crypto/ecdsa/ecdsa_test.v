// vtest build: present_openssl? && !(openbsd && gcc) && !(sanitize-memory-clang || docker-ubuntu-musl)
module ecdsa

fn test_ecdsa() {
	// Generate key pair
	pub_key, priv_key := generate_key() or { panic(err) }

	// Sign a message
	message := 'Hello, ECDSA!'.bytes()
	signature := priv_key.sign(message) or { panic(err) }

	// Verify the signature
	is_valid := pub_key.verify(message, signature) or { panic(err) }
	println('Signature valid: ${is_valid}')
	assert is_valid

	priv_key.free()
	pub_key.free()
}

fn test_ecdsa_signing_with_recommended_hash_options() {
	// Generate key pair
	pub_key, priv_key := generate_key() or { panic(err) }

	// Sign a message
	message := 'Hello, ECDSA!'.bytes()
	signature := priv_key.sign(message) or { panic(err) }

	// Verify the signature
	is_valid := pub_key.verify(message, signature) or { panic(err) }
	println('Signature valid: ${is_valid}')
	assert is_valid
	pub_key.free()
	priv_key.free()
}

fn test_generate_key() ! {
	// Test key generation with high level opaque
	pub_key, priv_key := generate_key() or { panic(err) }
	assert pub_key.evpkey != unsafe { nil }
	assert priv_key.evpkey != unsafe { nil }

	priv_key.free()
	pub_key.free()
}

fn test_new_key_from_seed() ! {
	// Test generating a key from a seed
	seed := [u8(1), 2, 3, 4, 5]
	priv_key := new_key_from_seed(seed) or { panic(err) }
	retrieved_seed := priv_key.bytes() or { panic(err) }
	assert seed == retrieved_seed
	priv_key.free()
}

fn test_new_key_from_seed_with_leading_zeros_bytes() ! {
	// Test generating a key from a seed
	seed := [u8(0), u8(1), 2, 3, 4, 5]
	priv_key := new_key_from_seed(seed) or { panic(err) }
	retrieved_seed := priv_key.bytes() or { panic(err) }
	assert seed == retrieved_seed
	priv_key.free()
}

fn test_sign_and_verify() ! {
	// Test signing and verifying a message
	pub_key, priv_key := generate_key() or { panic(err) }
	message := 'Test message'.bytes()
	signature := priv_key.sign(message) or { panic(err) }
	is_valid := pub_key.verify(message, signature) or { panic(err) }
	assert is_valid

	priv_key.free()
	pub_key.free()
}

fn test_seed() ! {
	// Test retrieving the seed from a private key
	pub_key, priv_key := generate_key() or { panic(err) }
	seed := priv_key.bytes() or { panic(err) }
	assert seed.len > 0
	priv_key.free()
	pub_key.free()
}

fn test_public_key() ! {
	// Test getting the public key from a private key
	pubkk, priv_key := generate_key() or { panic(err) }
	pub_key1 := priv_key.public_key() or { panic(err) }
	pub_key2, privkk := generate_key() or { panic(err) }
	assert !pub_key1.equal(pub_key2)

	pubkk.free()
	privkk.free()
	priv_key.free()
	pub_key1.free()
	pub_key2.free()
}

fn test_private_key_equal() ! {
	// Test private key equality
	pbk, priv_key1 := generate_key() or { panic(err) }
	seed := priv_key1.bytes() or { panic(err) }
	priv_key2 := new_key_from_seed(seed) or { panic(err) }
	assert priv_key1.equal(priv_key2)

	pbk.free()
	priv_key1.free()
	priv_key2.free()
}

fn test_private_key_equality_on_different_curve() ! {
	// default group
	pbk, priv_key1 := generate_key() or { panic(err) }
	seed := priv_key1.bytes() or { panic(err) }
	// using different group
	priv_key2 := new_key_from_seed(seed, nid: .secp384r1) or { panic(err) }
	assert !priv_key1.equal(priv_key2)
	pbk.free()
	priv_key1.free()
	priv_key2.free()
}

fn test_public_key_equal() ! {
	// Test public key equality
	pbk, priv_key := generate_key() or { panic(err) }
	pub_key1 := priv_key.public_key() or { panic(err) }
	pub_key2 := priv_key.public_key() or { panic(err) }
	assert pub_key1.equal(pub_key2)
	pbk.free()
	priv_key.free()
	pub_key1.free()
	pub_key2.free()
}

fn test_sign_with_new_key_from_seed() ! {
	// Test signing with a key generated from a seed
	seed := [u8(10), 20, 30, 40, 50]
	priv_key := new_key_from_seed(seed) or { panic(err) }
	message := 'Another test message'.bytes()
	signature := priv_key.sign(message) or { panic(err) }
	pub_key := priv_key.public_key() or { panic(err) }
	is_valid := pub_key.verify(message, signature) or { panic(err) }
	assert is_valid
	priv_key.free()
	pub_key.free()
}

fn test_invalid_signature() ! {
	// Test verifying an invalid signature
	pub_key, pvk := generate_key() or { panic(err) }
	message := 'Test message'.bytes()
	invalid_signature := [u8(1), 2, 3] // Deliberately invalid
	result := pub_key.verify(message, invalid_signature) or {
		// Expecting verification to fail
		assert err.msg() == 'Failed to verify signature'
		pub_key.free()
		pvk.free()
		return
	}
	assert !result
	pub_key.free()
	pvk.free()
}

fn test_different_keys_not_equal() ! {
	// Test that different keys are not equal
	pbk1, priv_key1 := generate_key() or { panic(err) }
	pbk2, priv_key2 := generate_key() or { panic(err) }
	assert !priv_key1.equal(priv_key2)
	pbk1.free()
	pbk2.free()
	priv_key1.free()
	priv_key2.free()
}

fn test_private_key_new() ! {
	priv_key := PrivateKey.new()!
	assert priv_key.ks_flag == .fixed
	size := evp_key_size(priv_key.evpkey)!
	assert size == 32
	pubkey := priv_key.public_key()!

	message := 'Another test message'.bytes()
	signature := priv_key.sign(message)!
	is_valid := pubkey.verify(message, signature)!
	assert is_valid

	// new private key
	seed := priv_key.bytes()!
	priv_key2 := new_key_from_seed(seed)!
	pubkey2 := priv_key2.public_key()!
	assert priv_key.equal(priv_key2)
	assert pubkey.equal(pubkey2)
	is_valid2 := pubkey2.verify(message, signature)!
	assert is_valid2

	// generates new key with different curve
	priv_key3 := new_key_from_seed(seed, nid: .secp384r1)!
	pubkey3 := priv_key3.public_key()!
	assert !priv_key3.equal(priv_key2)
	assert !pubkey3.equal(pubkey2)
	is_valid3 := pubkey3.verify(message, signature)!
	assert !is_valid3

	priv_key.free()
	priv_key2.free()
	priv_key3.free()
	pubkey.free()
	pubkey2.free()
	pubkey3.free()
}

// See https://discord.com/channels/592103645835821068/592114487759470596/1334319744098107423
fn test_key_with_msg_exceed_key_size() ! {
	pv := PrivateKey.new()!
	msg := 'a'.repeat(200).bytes()
	opt := SignerOpts{
		hash_config: .with_no_hash
	}
	signed := pv.sign(msg, opt)!
	pb := pv.public_key()!

	// should be verified
	st := pb.verify(msg, signed, opt)!
	assert st

	// different msg should not be verified
	other_msg := 'a'.repeat(392).bytes()
	ds := pb.verify(other_msg, signed, opt)!
	// This should assert to false.
	assert !ds

	pv.free()
	pb.free()
}
