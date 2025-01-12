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
	key_free(priv_key.key)
}

// This test should exactly has the same behaviour with default sign(message),
// because we passed .with_no_hash flag as an option.
fn test_ecdsa_signing_with_options() {
	// Generate key pair
	pub_key, priv_key := generate_key() or { panic(err) }

	// Sign a message
	message := 'Hello, ECDSA!'.bytes()
	opts := SignerOpts{
		hash_config: .with_no_hash
	}
	signature := priv_key.sign_with_options(message, opts) or { panic(err) }

	// Verify the signature
	is_valid := pub_key.verify(message, signature) or { panic(err) }
	println('Signature valid: ${is_valid}')
	key_free(pub_key.key)
	assert is_valid
}

fn test_generate_key() ! {
	// Test key generation
	pub_key, priv_key := generate_key() or { panic(err) }
	assert pub_key.key != unsafe { nil }
	assert priv_key.key != unsafe { nil }
	key_free(priv_key.key)
}

fn test_new_key_from_seed() ! {
	// Test generating a key from a seed
	seed := [u8(1), 2, 3, 4, 5]
	priv_key := new_key_from_seed(seed) or { panic(err) }
	retrieved_seed := priv_key.seed() or { panic(err) }
	assert seed == retrieved_seed
	key_free(priv_key.key)
}

fn test_sign_and_verify() ! {
	// Test signing and verifying a message
	pub_key, priv_key := generate_key() or { panic(err) }
	message := 'Test message'.bytes()
	signature := priv_key.sign(message) or { panic(err) }
	is_valid := pub_key.verify(message, signature) or { panic(err) }
	assert is_valid
	key_free(priv_key.key)
}

fn test_seed() ! {
	// Test retrieving the seed from a private key
	_, priv_key := generate_key() or { panic(err) }
	seed := priv_key.seed() or { panic(err) }
	assert seed.len > 0
	key_free(priv_key.key)
}

fn test_public_key() ! {
	// Test getting the public key from a private key
	_, priv_key := generate_key() or { panic(err) }
	pub_key1 := priv_key.public_key() or { panic(err) }
	pub_key2, _ := generate_key() or { panic(err) }
	assert !pub_key1.equal(pub_key2)
	key_free(priv_key.key)
	key_free(pub_key1.key)
	key_free(pub_key2.key)
}

fn test_private_key_equal() ! {
	// Test private key equality
	_, priv_key1 := generate_key() or { panic(err) }
	seed := priv_key1.seed() or { panic(err) }
	priv_key2 := new_key_from_seed(seed) or { panic(err) }
	assert priv_key1.equal(priv_key2)
	key_free(priv_key1.key)
	key_free(priv_key2.key)
}

fn test_private_key_equality_on_different_curve() ! {
	// default group
	_, priv_key1 := generate_key() or { panic(err) }
	seed := priv_key1.seed() or { panic(err) }
	// using different group
	priv_key2 := new_key_from_seed(seed, nid: .secp384r1) or { panic(err) }
	assert !priv_key1.equal(priv_key2)
	key_free(priv_key1.key)
	key_free(priv_key2.key)
}

fn test_public_key_equal() ! {
	// Test public key equality
	_, priv_key := generate_key() or { panic(err) }
	pub_key1 := priv_key.public_key() or { panic(err) }
	pub_key2 := priv_key.public_key() or { panic(err) }
	assert pub_key1.equal(pub_key2)
	key_free(priv_key.key)
	key_free(pub_key1.key)
	key_free(pub_key2.key)
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
	key_free(priv_key.key)
	key_free(pub_key.key)
}

fn test_invalid_signature() ! {
	// Test verifying an invalid signature
	pub_key, _ := generate_key() or { panic(err) }
	message := 'Test message'.bytes()
	invalid_signature := [u8(1), 2, 3] // Deliberately invalid
	result := pub_key.verify(message, invalid_signature) or {
		// Expecting verification to fail
		assert err.msg() == 'Failed to verify signature'
		key_free(pub_key.key)
		return
	}
	assert !result
	key_free(pub_key.key)
}

fn test_different_keys_not_equal() ! {
	// Test that different keys are not equal
	_, priv_key1 := generate_key() or { panic(err) }
	_, priv_key2 := generate_key() or { panic(err) }
	assert !priv_key1.equal(priv_key2)
	key_free(priv_key1.key)
	key_free(priv_key2.key)
}
